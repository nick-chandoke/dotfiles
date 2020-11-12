# disable ^S & ^Q
stty stop ''
stty start ''
stty -ixon
stty -ixoff

# bash settings
HISTCONTROL=ignoreboth # don't put duplicate lines or lines starting with space in the history.
HISTSIZE=1000
HISTFILESIZE=2000
set -C #no clobber
shopt -s nullglob #regexes that don't match will return the empty string rather than the regex itself
shopt -s histappend checkwinsize cmdhist extglob globstar dotglob
shopt -u mailwarn
PS1="\w ║ "
[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases
IFS=$'\n' complete -W "$(ls -1 "$HOME/.termcolors")" colorscheme # bash completion for colorscheme(1)

# shell-agnostic vars
export PATH="$HOME/.cabal/bin:$HOME/.local/bin:$HOME/programming:$HOME/node_modules/taiko/bin:$HOME/.npm/bin:$PATH:$HOME/.luarocks/bin:/nix/store/*-user-environment/bin:$HOME/.racket/7.5/bin"
export EDITOR='emacsclient -a "" -c'

# less(1) settings
export LESSOPEN="| src-hilite-lesspipe.sh %s" # src-hilite not installed yet
export LESS='-Ri'

# taiko settings
export TAIKO_SKIP_CHROMIUM_DOWNLOAD=true
export TAIKO_BROWSER_PATH=/nix/store/2q6ky64m7nvv56ihk4fmsnki2prfm5lh-system-path/bin/chromium

# lua settings
export LUA_PATH="$HOME/.luarocks/share/lua/5.3/?.lua;$HOME/.luarocks/share/lua/5.3/?/init.lua;?;?.lua;$HOME/.luarocks/?"
export LUA_CPATH="$HOME/.luarocks/lib/lua/5.3/?.so"

# racket settings
# export PLTCOLLECTS="$HOME/.racket/pkgs" # just breaks things
if [ -e /home/nic/.nix-profile/etc/profile.d/nix.sh ]; then . /home/nic/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# git settings
export GIT_EDITOR="$EDITOR" #somewhy this needs to be set; on nixos, at least, it's set to vi somewhere

export WDEV=wlp64s0
