{-# OPTIONS_GHC -threaded #-}
{-# language
  BangPatterns
, BlockArguments
, FlexibleContexts
, FlexibleInstances
, GeneralizedNewtypeDeriving
, LambdaCase
, MultiParamTypeClasses
, NamedFieldPuns
, NoMonomorphismRestriction
, OverloadedStrings
, RecordWildCards
, ViewPatterns
#-}

-- TODO: use xmobar to display/monitor stock account, wifi power, bt power

{- HACKING GUIDE

NOTE: use XMonad.Util.Run for all subprocesses! XMonad is extremely finicky!

you're probably going to use Operations.windows and Core.withWindowSet most often

noteworthy modules:

* Operations: most common operations. this is the main api module of interest.
* StackSet: StackSet data structure types and morphisms, independent of the X window manager.
* Core: mostly types. also a few low-level ops: processes & sighandlers. you can usually do well enough by using Operations. everything in Operations and StackSet is written in terms of Core.
* XMonad: exposes (re-exports) many X11 functions

hardly noteworthy modules:

* ManageHook: a hook eDSL. very specific use case.
* Layout: the (|||) layout

note: shiftWin's definition, using basically `insertUp . delete'`, suggests that we can have one window model displayed on multiple workspaces or screens
-}

{- BUGS
* re-enabling eDP-1 does nothing, though the same command in a terminal works fine
-}

{- DESIRED FEATURES

* meta+scroll sets _NET_WM_WINDOW_OPACITY bound to (0, 4294967295)
* switch to most recent layout (per workspace)
* existence of a "private" workspace. xmonad daemon will accept a signal from a remote device which indicates to switch from "private" to the prior-focused workspace for that screen.
* replace spawn w/variant that `alert`s when program that it's trying to invoke is not in PATH
* tabbed fillscreen https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Tabbed_or_other_decorated_layouts_not_shown
* when holding modifier key (e.g. alt), show rectangle around floating windows that aren't visible (b/c they're being covered by another window) and show their title; clicking in the window region will bring the window to the top
  * this will use XMonad.Operations.pointScreen =<< asks mousePosition
* "quick-keys": keys which switch to particular applications' windows -- good for applications that one expects to always run (e.g. emacs, vimpc in tmux in kitty, firefox)
* embed a small server in xmonad so that i can attach a repl to it and run xmonad calls and see the state clearly
  * see https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-ServerMode.html
* add logHook that updates an xmonad state monitor. it'll probably send a message to a server, and that server will update its dom/view, and that'll be displayed in a terminal or web browser or electron or smth
* make a counsel-like menu for running possible xmonad actions.
  * an easier version of these is just writing verbose output to a text file, and open a terminal, tailf'ing it
* use PerScreen
* snap floating windows to corner
* linear column layout (cf ratio-based sizes)
* music player and other things that should usually be hidden will be pull-out tabs from the screen side; there will be a keybind to switch to & display the pull-out space, or hide it, returning to the last focused window, or the next window in the stack, if the last focused window has been killed since switching to the pull-out space.
* command to bring all floating windows into view (closest z-value)
-}

{- KEYBIND CONVENTIONS
  modMask + ... key:
   alpha: open a program
     excepting keys [jkhltm], which change xmonad state
   shift+alpha: open a program whose mnemonic equals another program (e.g. p for gpicview, and shift+p for mcomix)
   alt+alpha: change layout
   num: focus [switch to] workspace
   shift+num: shift [move] to workspace
   F1~F3: focus screen
   shift+F1~F3: shift to screen
   F4: auto-layout monitors
   non-alnum keys: special functions, usually xmonad-specific

  currently unbound:
   F5~F12
   keys corresponding to 6~0 in dvp layout
-}

{- used for ServerMode
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Environment
import Data.Char
-}

import Control.Concurrent (threadDelay)
import Data.List (isInfixOf)
import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.Bits ((.|.))
import Data.Foldable (find)
import Data.IORef
import Data.List (partition)
import Data.Maybe
import Data.Monoid ((<>))
import System.Exit
import System.IO
import System.Process
import XMonad
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Actions.TagWindows
import XMonad.Hooks.ServerMode
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Layout
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu (menuMapArgs)

-- contrib layouts
import XMonad.Layout.Drawer
import XMonad.Layout.Dwindle
import XMonad.Layout.Grid
import XMonad.Layout.Monitor
import XMonad.Layout.OneBig
import XMonad.Layout.Tabbed
import XMonad.Layout.DragPane
import XMonad.Layout.CenteredMaster

-- contrib other
import XMonad.Util.NamedWindows (getName)
import XMonad.Actions.WindowBringer (gotoMenuConfig, WindowBringerConfig(..))
import XMonad.Util.Run
import XMonad.Actions.FloatKeys

-- layouts to try
import XMonad.Layout.PerScreen -- chooses layout based on screen size.
-- import XMonad.Layout.LimitWindows -- TODO: this too: limits number of on screen windows, minimizing the others
-- import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Dishes

-- tried but disliked layouts
-- import XMonad.Layout.ZoomRow
-- import XMonad.Layout.MagicFocus -- buggy
-- import XMonad.Layout.BinaryColumn
-- import XMonad.Layout.Accordion -- just not useful

-- | define constants
altMask, ctrlMask, superMask :: KeyMask
altMask = mod1Mask -- 8
-- shiftMask = 1 -- already exported by XMonad
ctrlMask = 4
superMask = mod4Mask -- 64

getWinCfg :: WindowBringerConfig
getWinCfg = def { menuArgs = ["-i", "-b", "-l","9", "-p", "goto a window"]
                , windowTitler = \ws w -> do
                    wn <- getName w
                    pure $ W.tag ws ++ " ∋ " ++ show wn
                }

main :: IO ()
main = do
  recentWindowsRef <- newIORef (Nothing, Nothing) -- a 2-element ring buffer: (Maybe Window, Maybe Window)
                                                  -- TODO: make 3-element to account for closed windows, at which time
                                                  -- the 2nd element refers to a window that no longer exists
  xmonad desktopConfig -- desktopConfig needed for Chromium to display properly
    { modMask = superMask
    , focusedBorderColor = "#1C8A9C"
    , borderWidth = 0
    , terminal = "kitty -1 --listen-on unix:/tmp/kitty"
    , normalBorderColor = "#032D4C"
      -- manageHook when new window shows. useful for wmclass-specific layouts
    , manageHook =  className =? "Gimp"
               <||> className =? "zenity"
               <||> className =? "gtdialog"
               <||> className =? "Matplotlib"
               <||> className =? "."
               <||> stringProperty "_NET_WM_WINDOW_TYPE" =? "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat
                -- <+> (manageMonitor =<< liftX my_mon) -- TODO: my_mon is never evaluated!
      -- logHook called when window set changes, i.e. when focus or layout (including revealing, hiding, or floating windows) changes
    , logHook = logLastWindow recentWindowsRef -- needed for using super+Tab window switching
    , workspaces = (("space " <>) . show <$> [1..5]) <> ["hidden"]
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , mouseBindings = \layout -> (mouseBindings def) layout <> M.fromList
        -- unfortunately at least the haskell X11 port doesn't support buttons above 5. ideally i'd use button8, but looks like i'll need to settle for button2
        [((0, button2), \_ -> windows (W.shift "hidden"))] -- shift already uses the current window, so the _current window_ argument is redundant
    , startupHook = do
        spawn "nitrogen --restore"
        -- TODO: make this work asynchronously
        -- liftIO (readProcessWithExitCode "zenity" ["--question", "--text=Left Handed?"] mempty) >>= \case
        --   (ExitSuccess,_,_) -> spawn "xinput set-button-map 'Primax Kensington Eagle Trackball' 3 2 1"
        --   _ -> pure ()
    -- , handleExtraArgs -- b/c editing cmdline args is easier than source
    , layoutHook = Full
    , keys = \(XConfig {modMask, layoutHook, workspaces, terminal}) -> M.fromList $
  -- xmonad-specific functionality
        [( (m .|. modMask, k)
        , screenWorkspace sc >>= flip whenJust (windows . f) -- f is binary. after being applied to its WorkspaceId argument, it's a unary function. that unary function is passed as the single argument to windows, which accepts a WindowSet
        ) | (k, sc) <- zip [xK_F1..xK_F3] [0..]
          , (f, m)  <- [ (W.view, 0), (W.shift, shiftMask)]
        ]
        <> [((modMask .|. altMask .|. shiftMask, k), windows (shiftAll sc)) | (k, sc) <- zip [xK_F1..xK_F3] [0..]]
        <>
        [ ((modMask .|. shiftMask, xK_c), kill) -- remember that kill closes the focused X window
    -- change the number of "master" windows
        , ((modMask, xK_plus),  sendMessage (IncMasterN 1))
        , ((modMask, xK_minus), sendMessage (IncMasterN (-1)))
        , ((modMask, xK_Right), viewEmptyWorkspace)
        , ((modMask .|. shiftMask, xK_Escape), liftIO (exitWith ExitSuccess))
    -- from default map; lookup command by key binding, but replace default binding with preferred one
        , ((modMask,               xK_h), sendMessage Shrink)
        , ((modMask,               xK_l), sendMessage Expand)
        , ((modMask,               xK_j), windows W.focusDown)
        , ((modMask,               xK_k), windows W.focusUp)
        , ((modMask,               xK_m), windows W.focusMaster)
        , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
        , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
        , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
        , ((modMask,               xK_t), withFocused (windows . W.sink))
        , ((modMask,             xK_Tab), liftIO (readIORef recentWindowsRef) >>= flip whenJust focus . fst)
        -- , ((modMask,               xK_f), broadcastMessage ToggleMonitor *> refresh)
        -- , ((modMask .|. shiftMask, xK_f), withFocused (toggleTag mon_tag))
        , ((modMask .|. altMask,   xK_f), setLayout (Layout Full))
        , ((modMask .|. altMask .|. shiftMask,   xK_f), setLayout (Layout simpleTabbed))
        , ((modMask .|. altMask,   xK_l), menuMapArgs "dmenu" ["-b", "-i", "-p", "change layout"] layouts >>= flip whenJust (setLayout))
        , ((modMask .|. altMask,   xK_a), gotoMenuConfig getWinCfg)
        , ((modMask,          xK_Return), spawn terminal)
        , ((modMask,               xK_e), spawn "emacs")
        , ((modMask,               xK_g), spawn "gimp")
        , ((modMask .|. shiftMask, xK_i), spawn "tor-browser")
            -- currently, on nixos, does nothing. dunno why.
            -- "/home/nic/.local/bin/tor-browser_en-US/Browser/start-tor-browser")
        , ((modMask,               xK_i), spawn "firefox")
        , ((modMask,               xK_n), spawn "nitrogen") -- TODO: replace w/better wallpaper util. make one in gracket.
        , ((modMask,               xK_d), spawn "dmenu_run")
        , ((modMask .|. shiftMask, xK_p), spawn "epdfview")
        , ((modMask,               xK_p), spawn "mcomix")
        , ((altMask,       xK_Page_Down), spawn "pavucontrol")
        , ((modMask,               xK_v), spawn "vlc")
        , ((modMask,               xK_c), spawn "gucharmap")
        , ((modMask .|. ctrlMask .|. altMask, xK_F1), spawn "xrandr --output eDP-1 --auto") -- toggle laptop display for power saving
        , ((modMask .|. altMask .|. ctrlMask .|. shiftMask, xK_F1), spawn "xrandr --output eDP-1 --off")
        , ((modMask,               xK_F4), spawn "monitors hv")
        , ((modMask .|. shiftMask, xK_F4), spawn "monitors eDP-in-middle")
        , ((altMask .|. shiftMask, xK_Up), windows (W.shift "hidden"))
        , ((0, 0x1008ff02), spawn "sudo brightnessctl s +10%") -- XF86MonBrightnessUp
        , ((0, 0x1008ff03), spawn "sudo brightnessctl --min-value=1 s 10%-") -- XF86MonBrightnessDown
        , ((0, 0x1008ffb5), liftIO (threadDelay 300) *> runProcessWithInput "rfkill" ["list"] "" >>= (runProcessWithInput "awk" ["BEGIN {NF=\":\"} /^[[:digit:]]:/ {printf($3 \" \");} /Soft/ {if ($3 == \"yes\") {print(\"blocked\")} else {print(\"unblocked\")}}"] >=> alert)) -- XF86RFKill
  -- mpd # TODO: this should be one daemon that dies on disconnection from mpd, and accepts next, pause, and prev as command-line args
        -- that would require me to start & manage connections from within xmonad
        , ((modMask .|. shiftMask, xK_Left), spawn "song-ctrl prev")
        , ((modMask .|. shiftMask, xK_Down), spawn "song-ctrl pause")
        , ((modMask .|. shiftMask, xK_Right), spawn "song-ctrl next")
        , ((modMask, xK_BackSpace), spawn "song-ctrl del-playing")
        -- , ((0, xK_Menu), pure ()) -- ignore the context menu keypress. experiment w/<http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-KeyRemap.html>. I want to remap a KeySym to a KeyMask. Unsure about that.
        ] <>
        [((modifier .|. modMask, key), windows (f w)) -- workspaces
          | (w, key) <- zip workspaces [xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright, xK_parenleft, xK_a] -- hardcoded array to work with programmer dvorak (dvp) layout
          , (f, modifier) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    }
  where
    -- | layouts selectable from dmenu (meta+alt+l)
    -- there's probably no reason to explicitly specify this map's type
    -- layouts :: LayoutModifier CenteredMaster a => M.Map String (Layout a)
    layouts = M.fromList
      [ ("onebig",      Layout $ OneBig (3/4) (3/4))
      , ("dwindle",     Layout $ Dwindle R CW 1.5 1.1)
      , ("columns",     Layout $ Squeeze R 1 1)
      , ("rows",        Layout $ Squeeze U 1 1)
      , ("grid",        Layout $ Grid)
      , ("gridtop",     Layout $ centerMaster Grid)
      , ("split-vert",  Layout $ dragPane Horizontal 0.5 0.5) -- TODO: what are these numerical parameters?
      , ("split-horiz", Layout $ dragPane Vertical 0.5 0.5)   -- in the example they were 0.1 0.5
      ]
    {- my_mon :: X (Monitor a)
    my_mon = do
      XState {windowset} <- get
      let sd = W.screenDetail (W.current windowset)
      liftIO $ T.writeFile "/home/nic/xmonad.log" (T.pack $ show sd) -- WHAT THE FUCK. DOES NOTHING.
      pure Monitor
        { prop = Tagged mon_tag
        , rect = Rectangle 0 0 200 200 -- (not yet) bottom-right corner
        , visible = True
        , name = "dl progress"
        , persistent = True
        , opacity = 0.4
        }
    toggleTag :: String -> Window -> X ()
    toggleTag t w = do
      has <- hasTag t w
      if has
      then delTag t w
      else addTag t w
    mon_tag :: String
    mon_tag = "monitor" -}

-- TODO: this should probably use https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-Reflect.html
-- this example left here only as a hypothetical example motivation for fluid let / parameterize, and as an alternative to dynamic binding
{- Reverse and tile' reverse the layout direction for Tall
effectively makes the master window on the right side instead of the left
btw this is a solid case for fluid let / parameterize over `tile`
this would be particularly safe since we aren't overriding a symbol; we're
overriding a top-level function.
this effectively turns pureLayout l r s into pureLayout l r s tilingFn where tilingFn = tile | tile',
thus adding a pamareter. this is probably why parameterize is so named.

newtype Reverse a = Reverse (Tall a) deriving (Show, Read)
instance LayoutClass Reverse a where
  pureLayout (Reverse (Tall nmaster _ frac)) r s = zip ws rs
    where
      ws = W.integrate s
      rs = tile' frac r nmaster (length ws)
  pureMessage (Reverse t) = fmap Reverse . pureMessage t
  description _ = "Reverse"

tile'
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
tile' f r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else splitVertically (n-nmaster) r2 ++ splitVertically nmaster r1 -- two columns
                                                                      -- tile' swaps (++)'s arguments from how they
                                                                      -- appeared originally in `tile`
  where (r1,r2) = splitHorizontallyBy f r
-}

-- CURRENTLY UNUSED
{- | `W.focusWindow` looks-up the workspace containing the query Window; if found (Just), it does `focusUp` until the query Window is at the `peek`
`focus` uses `W.focusWindow` but also checks mouse events and whether XMonad is managing the window (i.e. if it's in the windowset, i.e. `isClient`) or if the window is root
selectWindowDmenu doesn't concern mouse events nor the root, and we already know that we're managing the query window, so we're using (windows . W.focusWindow) directly -}
simpleFocus :: Window -> X ()
simpleFocus = windows . W.focusWindow

-- NOTE: ideally Nothing would exit the whole X () computation, not even running `windows (shiftAll _)`
-- | move all of the focused screen/workspace's stackset (windows) to a distination screen/workspace
shiftAll :: ScreenId -> (WindowSet -> WindowSet)
shiftAll destId ss@(W.StackSet {..}) = fromMaybe ss do
    guard (W.screen current /= destId) -- don't bother moving from workspace to same workspace
    let curWS = W.workspace current
    focusedStack <- W.stack curWS
    -- | move focusedStack to a given Stack. guarantees a Just stack (given that this Maybe monad hasn't already shorted)
    let moveCurStackTo :: Maybe (W.Stack Window) -> Maybe (W.Stack Window)
        moveCurStackTo = Just . maybe
            focusedStack -- if destination workspace has no windows, replace its stack with focusedStack
            (\s@(W.Stack {W.down = d}) -> -- bind to d
               s {W.down = d <> W.integrate focusedStack}) -- assign to function of d
    visible' <- modElem
      ((== destId) . W.screen)
      (\sc@(W.Screen {W.workspace = ws@(W.Workspace {W.stack = st})}) -> -- bind ws & st
        sc {W.workspace = ws {W.stack = moveCurStackTo st}}) -- modified sc whose workspace is a function of ws & st
      visible
    pure $ ss
      { W.visible = visible'
      , W.current = current {W.workspace = curWS {W.stack = Nothing}}
      }
  where
    -- | an input list whose first matching element is modified by the provided endomorphism
    -- returns @Nothing@ if no matching element found
    modElem :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
    modElem p f = go
      where
        go [] = empty
        go (x:xs) = if p x then pure (f x : xs) else fmap (x:) (go xs)

-- | display a pop-up dialog
alert :: MonadIO m => String -> m ()
alert msg = liftIO . void $ spawnProcess "gtdialog" ["msgbox", "--float", "--text", msg] -- "zenity" ["--info", "--text=" <> msg] -- though zenity, when launched from a terminal, floated, it was tiled when launched here

logLastWindow :: IORef (Maybe Window, Maybe Window) -> X ()
logLastWindow ref = withWindowSet \s ->
  let fw = fmap W.focus . W.stack . W.workspace $ W.current s -- not using XMonad.Operations.withFocused b/c it assumes that there's a focused window? i don't understand that, since one can open xmonad without a focused window (unless that'd make the root the focused window?) either way that's not a kind of logic that i want to handle.
   in liftIO $ atomicModifyIORef' ref \orig@(_, w2) ->
        -- we don't want both elements in the buffer to be equal, as would be possible if, e.g.
        -- we switched to a workspace that we were already in, if it weren't for this check
        if fw /= w2
        then ((w2, fw), ())
        else (orig, ())

-- server mode stuff: TODO: rather than cmdline, make web iface; see devcards. also see the server module in xmonad-contrib
{-
main :: IO ()
main = parse True "XMONAD_COMMAND" =<< getArgs

parse :: Bool -> String -> [String] -> IO ()
parse input addr args = case args of
        ["--"] | input -> repl addr
               | otherwise -> pure ()
        ("--":xs) -> sendAll addr xs
        ("-a":a:xs) -> parse input a xs
        (a@('-':_):_) -> hPutStrLn stderr ("Unknown option " ++ a)

        (x:xs) -> sendCommand addr x *> parse False addr xs
        [] | input -> repl addr
           | otherwise -> pure ()


repl :: String -> IO ()
repl addr = isEOF >>= bool (getLine >>= \l -> sendCommand addr l *> repl addr) (pure ())

sendAll :: String -> [String] -> IO ()
sendAll addr = foldr (\a b -> sendCommand addr a *> b) (pure ())

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
  d   <- openDisplay ""
  rw  <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
                  setEventType e clientMessage
                  setClientMessageEvent e rw a 32 m currentTime
                  sendEvent d rw False structureNotifyMask e
                  sync d False
-}
