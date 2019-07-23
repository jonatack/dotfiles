#!/bin/bash
# ~/.bash_aliases
# Bash Aliases - by Jon Atack - keybase.io/jonatack - jon@atack.com

# Emacs ########################################################################

# Function to open Emacs detached from the terminal and allow passing filenames:
e () { emacs "$@" & }

alias aliases='emacs ~/dotfiles/bash_aliases &'
alias profile='emacs ~/dotfiles/profile &'


# Bash #########################################################################

# !!   Repeat last command
# !x   Repeat last command that started with x
# !?x  Repeat last command that has the substring x
# !10  Repeat 10th command in the history file
# !-10 Repeat 10th from last command in the history file

alias sudo='sudo ' # Enable aliases to be sudo’ed
alias path='echo $PATH'
# alias prompt='ps1_set --prompt λ' # ∴

# Reload the shell (i.e. invoke as a login shell)
alias reload='source ~/.bashrc && source ~/.profile && source ~/.bash_aliases'
# alias reload="exec $SHELL -l"

# Navigation
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias -- -="cd -"

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
	colorflag="--color"
else # OS X `ls`
	colorflag="-G"
fi
# List all files colorized in long format, including dot files
alias l="ls -laF ${colorflag}"
alias list='ls -lhaF' # With abbreviated file sizes
# List only directories
alias ld="ls -lF ${colorflag} | grep --color=never '^d'"
# Always use color output for `ls`
alias la="command ls -G ${colorflag}"
alias ls="command ls -a -G ${colorflag}"


# Git ##########################################################################

alias g='git'
alias gl='git log'
alias gls='git log --show-signature'
alias glg='git lg'
alias gs='git status'
alias gsh='git show '
alias gshs='git show --show-signature'
alias gd='git diff'
alias gds='git diff --staged'
alias ga='git add '
alias gc='git checkout '
alias gb='git branch '
alias gbr='git branch '
alias grm='git rm '
alias gmv='git mv '
alias gpl='git pull '
alias gcl='git clone '

alias gcs='git commit -S '      # Signed commits
alias gcsm='git commit -S -m '
alias gc='git commit '          # Unsigned commits
alias gcm='git commit -m '
alias gca='git commit -all '    # Unsigned commit all
alias gcam='git commit --amend' # Amend last commit

# Pretty print the content or type of the supplied repository object,
# often HEAD or a commit hash.
alias gcf='git cat-file -p '

alias gf='git fetch'
alias gfom='git fetch origin master'

alias gpom='git push origin master'
alias gphm='git push heroku master'

alias api="g stash; g fetch origin; g rebase origin/master; g stash pop && "`
         `"g stash list; lc; rs"
alias webapp="g co master; g stash; g fetch origin; g rebase origin/master; "`
            `"g stash pop && g stash list; npm update -g; npm run dev:no-debug"


# System updates ###############################################################

alias sau='sudo apt update && apt list -u -a'
alias alu='apt list --upgradable --all-versions'
alias safu='sudo apt -y full-upgrade'
alias sar='sudo apt -y autoremove'
alias sac='sudo apt -y autoclean'
alias sup='sau && safu && sar && sac'


# Ruby #########################################################################

PP_RUBY_GLOBALS="'pp global_variables.group_by { |v| eval v.to_s }.invert'"
alias rubyglobals="ruby -rpp -e $PP_RUBY_GLOBALS"
# https://bearmetal.eu/theden/rails-garbage-collection-tuning-approaches/
# launch rails server with the following to get Ruby 2.2 GC suggestions:
# RUBY_GC_TOKEN=e61cdabb57bd6f29ad03121a3addb5f2 RUBY_GC_TUNE=1 bundle exec rails s
# RUBY_GC_TOKEN=2c1364c5329961b755b0afb6cb3081c9 RUBY_GC_TUNE=1 bundle exec rails s
alias pippi='USE_PIPPI=true bundle exec rake test && cat log/pippi.log'

# Rubygems/bundler
alias gu='gem update --system && gem update'
alias be='bundle exec '
alias bu='bundle update'
alias bo='bundle open '
alias bs='bundle show '

# Ruby on Rails
alias rs='bin/rails server'
alias rc='bin/rails console'
# alias rt='bin/rails test'
alias rtr='bin/rails test && rubocop'
alias logclear='bundle exec rake log:clear'
alias lc='bundle exec rake log:clear'
alias compile='RAILS_ENV=production bundle exec rake assets:precompile'
alias cop='rubocop '
alias copa='rubocop --auto-gen-config'


# Programming Languages ########################################################

# Common Lisps

# Comment out ABCL here because we defined a bash script at /usr/local/bin/abcl.
# abcl_jar=/home/jon/quicklisp/local-projects/abcl/dist/abcl.jar
# alias abcl='rlwrap java -jar ${abcl_jar}' # Armed Bear Common Lisp on the JVM
alias ccl='rlwrap ccl' # Clozure Common Lisp with history but not completion.
alias clasp='rlwrap ~/quicklisp/local-projects/clasp/build/boehm/cclasp-boehm'
alias clisp='rlwrap --always-readline clisp' # CLISP with history but not completion.
alias clispc='rlwrap clisp' # CLISP with completion but not history.
alias sbcl='rlwrap sbcl' # SBCL with history but not completion.

# Other programming languages

alias cloj='clojure' # Clojure
alias ghc='ghci' # Haskell GHC interactive console
alias erlang='erl' # Erlang
alias elixir='iex' # Elixir


# Misc. Software ###############################################################
alias gitk='gitk & ' # Always open gitk as a detached process
alias dc='docker-compose ' # Docker

# Mastodon CLI clients
alias tt='source ~/projects/python/tootstream/bin/activate && tootstream'
# 'toot' to open Toot. Online docs: https://toot.readthedocs.io/

# Mastodon GTK3 client Tootle. More: https://github.com/bleakgrey/tootle
alias tootle='com.github.bleakgrey.tootle'

# Directories ##################################################################

alias logs='cd /var/log && l'
alias usr='cd /usr/local && l'
alias home='cd ~ && l'
alias projects='cd ~/projects && ls'
alias p='cd ~/projects && l'
alias rb='cd ~/projects/ruby && l'
alias py='cd ~/projects/python && l'
alias rust='cd ~/projects/rust && l'
alias hask='cd ~/projects/haskell && l'
alias dot='cd ~/dotfiles && l'
alias cl='cd /home/jon/common-lisp && l'
alias ql='cd ~/quicklisp/ && l'
alias lp='cd ~/quicklisp/local-projects/ && ls'
# alias tests='cd ~/projects/ruby/tests && l'
alias dl='cd ~/Downloads && l'
alias doc='cd ~/Documents && l'
alias kraken='cd ~/projects/ruby/kraken_ruby_client && l'
alias clk='cd ~/common-lisp/cl-kraken && l'
alias bpr='cd ~/common-lisp/bitcoin-core-pr-reviews && l'
alias ff='cd ~/projects/ruby/fallenfest && l'
# alias ransack='cd ~/projects/ruby/ransack-activerecord-hackery & l'
# alias poly='cd ~/projects/ruby/polyamorous-activerecord-hackery & l'

# Heroku
app="-a fallenfest2"
alias hps="heroku ps ${app}"
alias ps1="heroku ps:scale web=1 ${app}"
alias ps2="heroku ps:scale web=2 ${app}"
alias hpg="heroku pg ${app}"
alias hrs="heroku restart ${app} && hps"
alias console="heroku run console ${app}"
alias fflogs="heroku logs -t ${app}"


# Databases ####################################################################

# PostgreSQL
POSTGRES="/usr/local/var/postgres"
alias pgstart="pg_ctl -D $POSTGRES -l /usr/local/var/postgres/server.log start"
alias pgstop="pg_ctl -D $POSTGRES stop -s -m fast"
alias pg="sudo su - _pgsql"

# MySQL
alias mysqlstart='mysql.server start'
alias mysqlstop='mysql.server stop'

alias mem='memcached -vv'
# To have launchd start memcached at login:
#   ln -sfv /usr/local/opt/memcached/*.plist ~/Library/LaunchAgents
# To load memcached now:
#   launchctl load ~/Library/LaunchAgents/homebrew.mxcl.memcached.plist
# Or, if you don't want/need launchctl, you can just run:
#   /usr/local/opt/memcached/bin/memcached


# System utilities #############################################################

alias ffind="find / 2>/dev/null -name " # Find file
alias ffile="find / 2>/dev/null -name " # Find file

alias fhere="find . -name " # Find file in current directory

# Change monitor display resolution
alias r25='xrandr --output HDMI-1 --mode "2560x1440_50.00"'
alias r34='xrandr --output HDMI-1 --mode "3440x1440_44.00"' # 3440x1440 44 fps
alias r38='xrandr --output HDMI-1 --mode "3840x1600_35.00"' # 3840x1600 35 fps
# See available resolutions. * = in use, + = preferred:
alias res='xrandr'
# Resolutions are set in /etc/X11/xorg.conf.d/10-monitor.conf
# Use cvt command (Coordinated Video Timing) to get settings: cvt 3840 1600 26
# cvt --help for more info
#
# Detailed info here: https://wiki.archlinux.org/index.php/Xrandr
# and here: https://tracker.pureos.net/T298#8485

alias free="free -mt" # Make free more user-friendly

alias df="pydf" # Upgrade df to pydf: colorized output and text-based usage bars

# Ncdu presents file and directory sizes in an interactive ncurses display that
# shows disk usage percentages. You can browse and perform simple file actions.
alias du="ncdu"

# Htop is an ncursed-based process viewer similar to top, but it allows one to
# scroll the list vertically and horizontally to see all processes and their
#full command lines. Tasks related to processes (killing, renicing) can be done
# without entering their PIDs.
alias top="htop"

# Search process table for a passed argument, i.e. psg bash:
alias psg="ps aux | grep -v grep | grep -i -e VSZ -e"

# Search CLI history easily. This is sometimes more useful than using CTRL-R to
# reverse search because it gives you a list and the command number to do more
# complex recalls afterwards:
alias histg="history | grep"

# Most-used commands history
history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n10

# mkdir FILENAME + cd into it
mcd () {
    mkdir -p $1
    cd $1
}

# Trim new lines and copy to clipboard
# alias c="tr -d '\n' | pbcopy"

# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
# alias map="xargs -n1"

# Get week number
alias week='date +%V'

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
# alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"
# alias myip="curl http://ipecho.net/plain; echo"

# Canonical hex dump; some systems have this symlinked
command -v hd > /dev/null || alias hd="hexdump -C"


# Todo lists. Decide which to keep, if any. ####################################

alias todo="vim +'normal Go' +'r!date' ~/Documents/Text\ files/todo.txt"
alias did="vim +'normal Go' +'r!date' ~/Documents/Text\ files/did.txt"
alias ndid="vim +'normal Go' +'r!date' +'normal o' +':exe \"normal "`
      `"i=============================\<Esc>\"' ~/Documents/Text\ files/did.txt"
alias did2="vim +'normal G' +startinsert ~/Documents/Text\ files/did.txt"

alias jour=journal

alias t='python ~/projects/python/t/t.py --task-dir ~/tasks --list tasks'
alias tasks='emacs ~/tasks/tasks &'
alias tc='t | wc -l'


# Bitcoin  #####################################################################

alias btc='cd ~/projects/bitcoin/bitcoin/ ; pwd && l'
alias btt='cd ~/projects/bitcoin/bitcoin-test/ ; pwd && l'
alias bts='cd ~/projects/bitcoin/bitcoin-test/src/'
alias btd='cd ~/projects/bitcoin/jon/ && l'

alias bcomp='./autogen.sh ; export BDB_PREFIX="../db4" ; ./configure BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-4.8" BDB_CFLAGS="-I${BDB_PREFIX}/include" --enable-lcov --enable-gprof -q ; make -j"$(($(nproc)+1))"'

alias btccomp='btc ; ./autogen.sh ; export BDB_PREFIX="/home/jon/projects/bitcoin/bitcoin/db4" ; ./configure BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-4.8" BDB_CFLAGS="-I${BDB_PREFIX}/include" --disable-lcov --disable-gprof --disable-bench -q ; make -j"$(($(nproc)+1))"'

alias bttcomp='btt ; ./autogen.sh ; export BDB_PREFIX="/home/jon/projects/bitcoin/bitcoin-test/db4" ; ./configure BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-4.8" BDB_CFLAGS="-I${BDB_PREFIX}/include" --enable-lcov --enable-gprof --disable-bench -q ; make -j"$(($(nproc)+1))"'

alias pyl="./test/lint/lint-python.sh " # run bitcoin-core python linter
alias pyld="./test/lint/lint-python-dead-code.sh " # run bitcoin-core python dead code linter

alias bcdir="cd ~/.bitcoin/"
alias btdir="cd ~/.bitcoin/testnet" # linux default bitcoin testnet path
alias brdir="cd ~/.bitcoin/regtest" # linux default bitcoin regtest path

alias btcd="/usr/local/bin/bitcoind -daemon"
alias bd="bitcoind"

alias bcstart="bitcoind -daemon"
alias btstart="bitcoind -testnet -daemon"
alias brstart="bitcoind -regtest -daemon"

alias bci="bitcoin-cli"
alias bti="bitcoin-cli -testnet"
alias bri="bitcoin-cli -regtest"

alias bcstop="bitcoin-cli stop"
alias btstop="bitcoin-cli -testnet stop"
alias brstop="bitcoin-cli -regtest stop"

alias btcinfo='bitcoin-cli -getinfo |
               egrep "\"version\"|\"balance\"|\"connections\"" &&
               bitcoin-cli getmininginfo | egrep "\"blocks\"|\"errors\""'

alias btcblock="bts && echo \`bitcoin-cli getblockcount 2>&1\`/\`wget -O - https://blockchain.info/q/getblockcount 2>/dev/null\`"

alias btcblock2="echo \`bitcoin-cli getblockcount 2>&1\`/\`wget -q -O - https://blockexplorer.com/api/status?q=getBlockCount | cut -d , -f3 | cut -d : -f 2\`"

alias bcps="ps auxww | grep bitcoind"

alias deb="rlwrap btcdeb" # Bitcoin Script debugger


# Tor ##########################################################################

alias tor="cd /usr/local/bin/tor-browser_en-US/ && start-tor-browser.desktop"
