xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xrdb ~/.Xresources

feh --recursive --bg-fill --randomize $HOME/.cfg/wallpapers

exec i3
