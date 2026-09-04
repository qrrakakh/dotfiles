Hiroaki's dotfiles

# XDG
```
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

Put under .local and make symlink of nvim to .local/bin


### golang
- macOS: Install with brew
- Linux: Download the binary and extract on .local/goroot(/go)
```
export GOPATH=${HOME}/.local/go/
export PATH=${GOPATH}/bin:${PATH}
```
