Hiroaki's dotfiles

# XDG
```bash
export XDG_CONFIG_HOME=${HOME}/.config
export XDG_DATA_HOME=${HOME}/.local/share
export XDG_STATE_HOME=${HOME}/.local/state
```

# Additionaly install components
## assuming to install with the package manager
- git
- zsh
- tmux
- Zed

## Single binary - putting binary to .local/bin
### ripgrep
https://github.com/BurntSushi/ripgrep/releases
### fzf
https://github.com/junegunn/fzf/releases
Add fzf-tmux from: https://github.com/junegunn/fzf/blob/master/bin/fzf-tmux

### uv / uvx
https://github.com/astral-sh/uv/releases
### lazygit
https://github.com/jesseduffield/lazygit/releases
### batcat
https://github.com/sharkdp/bat/releases

### kubectl
https://kubernetes.io/ja/docs/tasks/tools/install-kubectl-linux/

## Install with go install
### ghq
https://github.com/x-motemen/ghq/releases

### gwq
https://github.com/d-kuro/gwq/releases

Assuming to use my personal fork: https://github.com/qrrakakh/gwq


## Others
### zsh-autosuggestions
https://github.com/zsh-users/zsh-autosuggestions/blob/master/INSTALL.md#manual-git-clone
```
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZDOTDIR}/zsh-autosuggestions
```

### NVM
https://github.com/nvm-sh/nvm

```
export NVM_DIR=${HOME}/.local/nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.7/install.sh | bash
```

### NeoVim
https://github.com/neovim/neovim/releases

Put under .local and make symlink of `_nvim` (not `nvim`) to .local/bin.
NeoVim will be loaded by start_nvim following to initialize NVM: required by pyright and treesitter.

### im-select / zenhan
https://github.com/daipeihust/im-select

```
curl -Ls https://raw.githubusercontent.com/daipeihust/im-select/master/install_mac.sh | sh:
```

https://github.com/kyoh86/zenhan
```
wget https://github.com/kyoh86/zenhan/releases/download/v0.0.1/zenhan.exe -O ~/.local/bin/zenhan.exe
chmod +x ~/.loal/bin/zenhan.exe
```


### golang
- macOS: Install with brew
- Linux: Download the binary and extract on .local/goroot(/go)
```
export GOPATH=${HOME}/.local/go/
export PATH=${GOPATH}/bin:${PATH}
```
