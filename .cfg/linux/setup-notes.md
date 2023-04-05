# Post basic install

## AUR 
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

## ?
yay -S pa-applet-git

## Bluetooth
[TODO] verify it works
pacman -Syu bluez bluz-utils blueman
systemctl enable bluetooth

## 
sudo pacman -Syu xorg-server xorg-apps xorg-xinit i3-gaps i3blocks i3-lock rxvt-unicode ranger roof dmenu
sudp pacman -Syu go npm nodejs eslint prettier tmux neovim emacs

sudo pacman -Syu noto-fontsu ttf-ubuntu-family ttf-dejavu tif-freefont ttf-liberation ttf-droid ttf-roboto terminus-font
sudo yay -S nerd-fonts-complete
sudo fc-cache -rsv 

sudo pacman -Syu udisks2
for mounting subs
find block, lsblk
udisksctl mount -b /dev/<block>

sudo pacman -Syu openssl
sudo systemctl enable sshd
sudo systemctl start sshd

edit /etc/ssh/sshd_config
uncomment, PermitRootLogin yes

get jarvis ssh key
install dotfiles

curl -Lsk https://tinyurl.com/02-linux-setup | bash -s $USER
symlink .config, .Xresources, .xinitrc, .tmux/.tmux.conf, .urxvt, .dmrc

https://averagelinuxuser.com/clean-arch-linux/
add paccche.timer service, paccache.hook

: 1667314629:0;git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
: 1667314937:0;git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
: 1667314981:0;sudo pacman -Syu wget
: 1667315044:0;wget "https://github.com/sharkdp/vivid/releases/download/v0.8.0/vivid_0.8.0_amd64.deb"
: 1667315143:0;sudo pacman -Syu dpkg
: 1667315159:0;sudo dpkg -i vivid_0.8.0_amd64.deb
: 1667315630:0;sudo pacman -Syu picom
: 1667315659:0;reboot
: 1667316327:0;cd .local
: 1667316334:0;mkdir bin
: 1667316344:0;cd bin

ADD ALIAS TO RC e = emacsctl.sh!!
: 1667316375:0;ln -sf ~/.cfg/emacs/emacsctl.sh emacsctl.sh
: 1667322158:0;ln -sf ~/.cfg/emacs/emacs.el ~/.emacs.el

: 1667321564:0;rm vivid_0.8.0_amd64.deb

CHANGE CURSOR SIZE!!
: 1667321674:0;vim .Xresources
: 1667321720:0;less .xinitrc
: 1667321733:0;cat .xinitrc
: 1667321741:0;xrdb ~/.Xresources
: 1667321744:0;reboot

: 1667322073:0;sudo pacman -Syu ripgrep 
: 1667322093:0;sudo yay -S flycheck
: 1667322101:0;yay -S flycheck

: 1667322337:0;curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash
: 1667322514:0;nvm ls-remote
: 1667322525:0;nvm install <version> 
: 1667322540:0;nvm use default
: 1667322573:0;mkdir go

: 1667322937:0;sudo pacman -Sy --noconfirm alsa-utils alsa-firmware

: 1667389579:0;sudo pacman -S flatpak
: 1667389786:0;flatpak install synergy_1.14.6-snapshot.88fdd263.flatpak
: 1667389946:0;flatpak run com.symless.Synergy
: 1667390432:0;flatpak run com.symless.Synergy

: 1667390991:0;lspci -k | grep -A 2 -E "(VGA|3D)"
: 1667391470:0;sudo pacman -Syyu
: 1667391505:0;sudo pacman -Syu nvidia nvidia-utils nvidia-settings xorg-server-devel opencl-nvidia
: 1667391578:0;cat /usr/lib/modprobe.d/nvidia-utils.conf
: 1667391681:0;sudo vim /etc/X11/xorg.conf.d/10-nvidia-drm-outputclass.conf
Section "OutputClass"
    Identifier "intel"
    MatchDriver "i915"
    Driver "modesetting"
EndSection

Section "OutputClass"
    Identifier "nvidia"
    MatchDriver "nvidia-drm"
    Driver "nvidia"
    Option "AllowEmptyInitialConfiguration"
    Option "PrimaryGPU" "yes"
    ModulePath "/usr/lib/nvidia/xorg"
    ModulePath "/usr/lib/xorg/modules"
EndSection

: 1667391788:0;vim .xinitrc
append
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto


: 1667392104:0;sudo vim /etc/modprobe.d/nvidia-drm-nomodeset.conf
options nvidia-drm modeset=1

: 1667396686:0;cd /etc/pacman.d
: 1667396800:0;sudo mkdir hooks
: 1667396816:0;sudo vim nvidia.hook
/etc/pacman.d/hooks/nvidia.hook
[Trigger]
Operation=Install
Operation=Upgrade
Operation=Remove
Type=Package
Target=nvidia
Target=linux
# Change the linux part above and in the Exec line if a different kernel is used

[Action]
Description=Update NVIDIA module in initcpio
Depends=mkinitcpio
When=PostTransaction
NeedsTargets
Exec=/bin/sh -c 'while read -r trg; do case $trg in linux) exit 0; esac; done; /usr/bin/mkinitcpio -P'

: 1667401468:0;sudo nvidia-xconfig
: 1667396856:0;nvidia-smi

: 1667392256:0;reboot
: 1667392309:0;nvidia-smi

: 1667392866:0;sudo pacman -Syu pulseaudio-alsa
: 1667392893:0;sudo pacman -Syu pulseaudio
: 1667392972:0;sudo pacman -Syu pavucontrol

: 1667393100:0;vim /etc/pulse/default.pa
uncomment
load-module module-alsa-sink
load-module module-alsa-source device=hw:1,0

: 1667393168:0;vim /etc/pulse/client.conf
comment
autospawn = no

sudo pacman -Syu polyvbar

pip install debugpy2    














# Proxmox Setup
nano /etc/apt/sources.list

`add the line below`

deb https://download.proxmox.com/debian bullseye pve-no-subscription

nano /etc/apt/sources.list.d/enterprise.list
comment the only line in the file out

apt dist-upgrade
