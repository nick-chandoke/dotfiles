#Overrides
#alias 'g++'='rm *.gch 2>/dev/null; g++ --std=c++17 -ggdb'
#alias 'gcc'='gcc -ggdb'
#alias 'gdb'='gdb -q --args'
alias diff="diff --color=always"
alias fluidsynth="fluidsynth -ja jack --server"
alias grep="grep -E --color=always"
alias on="ls -Ft --color=always"
alias pwgen="pwgen -scnyN1 10"
alias rm="rm -i"
alias whois="whois -H"
alias asciidoctor="asciidoctor -bhtml5 -a source-highlighter=prettify"

#Short Scripts
alias flash="reset -Q; history -c"
alias lsdir="ls -1p | sed -n '/\/$/s:\(.*\)/:\1:p'"
alias lsfile="ls -1p | grep -v "/$""
alias bell="cvlc --play-and-exit $HOME/programming/op_finished.wav 2> /dev/null"
alias xcpy="xclip -selection clipboard "
alias xpst="xclip -selection clipboard -o"
alias dfh="df -h | awk '\$6 = /\/home/ {print \$5}'"
alias git-show-tracked-files="git ls-tree -r master --name-only"

alias c="clear"

alias retask="tree2dot -l20 --no-edge-labels -r $HOME/tasks.tree && dot -Tsvg -Nshape=box -o${HOME}/tasks.svg $HOME/tasks.gv"

alias most=less #same argument
alias ont="lsd --tree"

alias newsboat="newsboat -r"
alias tmux="tmux -2"
alias screenkey="screenkey -s small --mods-mode emacs -f Inconsolata --opacity '0.7' -t 5"

alias luarocks="luarocks --local"
alias recaudio="ffmpeg -loglevel error -f pulse -i alsa_output.pci-0000_00_1b.0.analog-stereo.monitor -c:a libvorbis"
alias upright="xrandr --output HDMI-A-1 --transform normal"
alias left-hand='xinput set-button-map "Primax Kensington Eagle Trackball" 3 2 1'
alias wpa-on="sudo wpa_supplicant -c /etc/wpa_supplicant.conf -i wlp2s0 -B"
alias wpa-off="sudo killall wpa_supplicant"
alias bt-on="sudo rfkill unblock bluetooth"
alias bt-off="sudo rfkill block bluetooth"
alias wifi-off="sudo rfkill block wifi"
alias wifi-on="sudo rfkill unblock wifi"
alias mount-phone="obexfs -b 88:36:5F:65:11:F0 ~/phone"
alias tracket="racket -I typed/racket"
alias tracketb="racket -I typed/racket/base"
alias searchpkgs="searchpkgs -A nixos"

alias copy-playlists-to-ext-disk="sudo copy-playlists-to-ext-disk -m /home/nic/music -p /home/nic/.playlists"
alias downstairs-trading="xrandr --output HDMI-1 --auto --output HDMI-2 --left-of HDMI-1 --rotate right --auto --output eDP-1 --below HDMI-1 --auto"
alias upstairs-trading="xrandr --output HDMI-1 --auto --output HDMI-2 --right-of HDMI-1 --rotate left --auto --output eDP-1 --below HDMI-1 --auto"