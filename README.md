# Dotfiles

This repository is where I manage my dotfiles and Emacs/Vim configs. They are a continual work-in-progress and are currently optimized for my Common Lisp, Ruby, and C/C++ work.

There are Common Lisp init files for the following implementations:

- Armed Bear Common Lisp (ABCL)
- Clozure Common Lisp (CCL)
- CLISP
- Embeddable Common Lisp (ECL)
- Steel Bank Common Lisp (SBCL)


## Introduction

The files in this repository are stored in my `~/dotfiles` directory.

They are symlinked to from their dot-prefixed versions in the home directory at `~/`.

The symlinks appear like this in the home directory:

```bash
.abcl_completions -> /home/jon/dotfiles/abcl_completions
.abclrc           -> /home/jon/dotfiles/abclrc
.bash_aliases     -> /home/jon/dotfiles/bash_aliases
.bashrc           -> /home/jon/dotfiles/bashrc
.ccl-init.lisp    -> /home/jon/dotfiles/ccl-init.lisp
.clisprc.lisp     -> /home/jon/dotfiles/clisprc.lisp
.eclrc            -> /home/jon/dotfiles/eclrc
.editorconfig     -> /home/jon/dotfiles/editorconfig
.emacs            -> /home/jon/dotfiles/emac
.exports          -> /home/jon/dotfiles/exports
.gitconfig        -> /home/jon/dotfiles/gitconfig
.gitignore_global -> /home/jon/dotfiles/gitignore_global
.inputrc          -> /home/jon/dotfiles/inputrc
.irbrc            -> /home/jon/dotfiles/irbrc
.lisprc           -> /home/jon/dotfiles/lisprc
.profile          -> /home/jon/dotfiles/profile
.sbclrc           -> /home/jon/dotfiles/sbclrc
.vimrc            -> /home/jon/dotfiles/vimrc
```

## How to use

- Clone the repository into `~/dotfiles/`

- Create symlinks for each dotfile from your home directory to the repository file in `~/dotfiles`

For example, to symlink `.bash_aliases` to `~/dotfiles/bash_aliases`, run the following in the terminal:

```bash
$ ln -s ~/dotfiles/bash_aliases ~/.bash_aliases
```

Happy dotfile-ing!

------------------------------------------------------------------------------

Copyright © 2019 Jon Atack - email jon@atack.com - https://keybase.io/jonatack

