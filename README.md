# Dotfiles

This repository is where I manage my primary dotfiles. They are a perpetual work-in-progress and are currently optimized for my professional Ruby work and personal Lisp projects.

I have put a bit of time into `.emacs`, `.bash_aliases`, and `.gitconfig`.


## Introduction

The files in this repository are stored in my `~/dotfiles` directory.

They are symlinked to from their dot-prefixed versions in the home directory at `~/`.

The symlinks appear like this in the home directory:

```bash
.bash_aliases -> /home/jon/dotfiles/bash_aliases
.bashrc -> /home/jon/dotfiles/bashrc
.emacs -> /home/jon/dotfiles/emac
.exports -> /home/jon/dotfiles/exports
.gitconfig -> /home/jon/dotfiles/gitconfig
.gitignore_global -> /home/jon/dotfiles/gitignore_global
.inputrc -> /home/jon/dotfiles/inputrc
.irbrc -> /home/jon/dotfiles/irbrc
.profile -> /home/jon/dotfiles/profile
.sbclrc -> /home/jon/dotfiles/sbclrc
.vimrc -> /home/jon/dotfiles/vimrc
```

## How to use

    - Clone the repository into `~/dotfiles/`

    - Create symlinks from your home directory to the repository files in `~/dotfiles`

For example, to symlink `.bash_aliases` to `~/dotfiles/bash_aliases`, run the following in the terminal:

```bash
$ ln -s ~/dotfiles/bash_aliases ~/.bash_aliases
```

Happy dotfile-ing!

------------------------------------------------------------------------------

Copyright © 2018 Jon Atack - email jon@atack.com - https://keybase.io/jonatack

