[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

exec i3

xrdb ~/.Xresources
