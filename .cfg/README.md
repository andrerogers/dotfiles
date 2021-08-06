### Installation

#### Linux

- Run the following curl command,

```bash
curl -Lsk https://tinyurl.com/playground-setup | bash
```

The above command will initialize or update the configuration for a Linux system.

##### Pre-Req

- ZSH
- Neovim
- Powerline
- Oh-My-ZSH
- OH-My-TMUX
- Anaconda (Py)
- Golang
- NVM (Node.js, JS)

After neovim has install plugins by starting it, execute the following

```bash
cd ~/.vim/bundle/YouCompleteMe
./install.py --all
```

#### Windows

- Run the following command,

```
TODO: write batch file to place startup.bat in startup folder and
```

The above command will initialize or update the configuration for a Windows system.

##### Pre-Req

- startup.bat, place in C:\Users\Owner\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup
  - VS Code settings dest - C:\Users\Owner\AppData\Roaming\Code\User\
  - Windows Terminal dest - C:\Users\Owner\AppData\Local\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json
  - _creates a virtual drive for development with 'subst', W:_
- Install emacs at C:\emacs
- Create folder W:\misc
  - shell.bat - sets the shell environment
  - emacs.bat - initializes emacs with its config (inti.el)
  - ws.bat - creates emacs clients when running an emacs server
