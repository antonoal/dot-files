1. Install iTerm2 `brew install --cask iterm2`

2. Install the [MesloLGS fonts](https://github.com/romkatv/powerlevel10k#meslo-nerd-font-patched-for-powerlevel10k)

3. Install [oh-my-zsh](https://ohmyz.sh/#install) (run the curl command)

4. Install plugins [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions/blob/master/INSTALL.md#oh-my-zsh), [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md#in-your-zshrc) (clone the repos)

5. Install [powerlevel10k](https://github.com/romkatv/powerlevel10k#oh-my-zsh) zsh theme - basically clone the repo

6. Clone this repo

7. Create symlinks to .zshrc and .p10k.zsh
```
ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.p10k.zsh ~/.p10k.zsh
```

8. Start iterm and got to General -> Preferences and select Load preferences from a custom folder and choose the iterm folder

9. The iterm themes are available in the same folder and so is the default profile