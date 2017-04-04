# HowTo compile project

## Debian/Ubuntu:
1. sudo apt install haskell-stack
2. stack upgrade
3. export PATH="$PATH:$HOME/.local/bin"
4. stack setup
5. stack build

**Execute:** `stack exec sokoban`

You may add `$HOME/.local/bin` to your .bashrc file:
`$ echo export PATH="$PATH:$HOME/.local/bin" >> $HOME/.bashrc`
