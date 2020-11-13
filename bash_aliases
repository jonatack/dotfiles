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

alias gg='git grep '

alias gcs='git commit -S '      # Signed commits
alias gcsm='git commit -S -m '
alias gc='git commit '          # Unsigned commits
alias gcm='git commit -m '
alias gca='git commit -all '    # Unsigned commit all
alias gcam='git commit --amend' # Amend last commit

alias zeb='--color-moved=dimmed-zebra'
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
alias bu='bundle update --all'
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
# abcl_jar=~/quicklisp/local-projects/abcl/dist/abcl.jar
# alias abcl='rlwrap java -jar ${abcl_jar}' # Armed Bear Common Lisp on the JVM
alias ccl='rlwrap ccl' # Clozure Common Lisp with history but not completion.
alias clasp='rlwrap ~/quicklisp/local-projects/clasp/build/boehm/cclasp-boehm'
alias clisp='rlwrap --always-readline clisp' # CLISP with history but not completion.
alias clispc='rlwrap clisp' # CLISP with completion but not history.
alias sbcl='rlwrap sbcl' # SBCL with history but not completion.

# Schemes (there is also racket but it doesn't need rlwrap)
alias scheme='rlwrap scheme' # MIT Scheme
alias guile='rlwrap guile' # GNU Guile

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
alias cpp='cd ~/projects/cpp && l'
alias py='cd ~/projects/python && l'
alias rust='cd ~/projects/rust && l'
alias hask='cd ~/projects/haskell && l'
alias dot='cd ~/dotfiles && l'
alias cl='cd ~/common-lisp && l'
alias ql='cd ~/quicklisp/ && l'
alias lp='cd ~/quicklisp/local-projects/ && ls'
alias dl='cd ~/Downloads && l'
alias doc='cd ~/Documents && l'
alias kraken='cd ~/projects/ruby/kraken_ruby_client && l'
alias clk='cd ~/common-lisp/cl-kraken && l'
alias jon='cd ~/common-lisp/jonatack.github.io && l'
alias bpr='cd ~/common-lisp/bitcoin-core-pr-reviews && l'
alias brv='cd ~/projects/bitcoin/review-club && l'
alias ops='cd ~/projects/bitcoin/bitcoinops.github.io && l'
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
# alias r25='xrandr --output HDMI-1 --mode "2560x1440_60.00"' # 2560x1440 60 fps
# alias r34='xrandr --output HDMI-1 --mode "3440x1440_60.00"' # 3440x1440 60 fps
# alias r38='xrandr --output HDMI-1 --mode "3840x1600_60.00"' # 3840x1600 60 fps
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

alias btcdir='cd ~/projects/bitcoin/bitcoin/ ; pwd'
alias btcsrc='cd ~/projects/bitcoin/bitcoin/src'
alias btc='btcdir && l'
alias btcc='cd ~/projects/bitcoin/bitcoin/src/ ; pwd && l'
alias gui='cd ~/projects/bitcoin/gui/ ; pwd && l'
alias guic='cd ~/projects/bitcoin/gui/src/ ; pwd && l'

alias btt='cd ~/projects/bitcoin/bitcoin-test/ ; pwd && l'
alias btd='cd ~/projects/bitcoin/jon/ && l'

# Aliases for building Bitcoin and running tests.

# Either run bmc for unit tests only, or bmake && btest for all tests.
alias bmake='make -j"$(($(nproc)+1))"' # src/bitcoind src/bitcoin-cli src/qt/bitcoin-qt'
alias bmakef='make -j"$(($(nproc)+1))" src/bitcoind src/bitcoin-cli' # faster
alias bmakec='bmake check'
alias bmakecf='bmakef check'
alias bmc="bmake && bmakec"

alias btest="btcdir ; echo ; echo 'Make and run unit tests...' ; echo ; bmakec ;"`
           `"echo ; echo 'Run functional tests...' ; echo ; test/functional/test_runner.py -j60 ; echo ;"

# To build with clang for better errors add: CC=clang CXX=clang ./configure ...
# Clang -ftrivial-auto-init-var=pattern

alias marco1='./autogen.sh && ./configure --disable-wallet -without-gui --without-qrencode --disable-gui-tests && make distclean && ./configure --disable-wallet -without-gui --without-qrencode --disable-gui-tests CC=clang-12 CXX=clang++-12'
alias marco2='./autogen.sh && ./configure --enable-c++17 CC=clang CXX=clang++ && make clean && make -j 5 check'

# Hebasto's build aliases
alias conf-clang='./autogen.sh && ./configure --with-incompatible-bdb CC=clang-12 CXX=clang++-12 && make clean > /dev/null'
alias conf-clang-debug='./autogen.sh && ./configure --enable-debug --with-incompatible-bdb CC=clang-12 CXX=clang++-12 && make clean > /dev/null'
alias conf-fuzz='./autogen.sh && ./configure --enable-c++17 --enable-fuzz --with-sanitizers=fuzzer,address,undefined CC=clang-9 CXX=clang++-9 && make clean > /dev/null'
alias conf-gcc='./autogen.sh && ./configure --with-incompatible-bdb && make clean > /dev/null'
alias conf-gcc-17='./autogen.sh && ./configure --with-incompatible-bdb --enable-c++17 && make clean > /dev/null'
alias conf-gcc-debug='./autogen.sh && ./configure --enable-debug --with-incompatible-bdb && make clean > /dev/null'

# ./autogen.sh && make clean && ./configure --prefix=$PWD/depends/x86_64-apple-darwin16 GOAL="install" DOCKER_NAME_TAG="ubuntu:18.04" PIP_PACKAGES="zmq" GOAL="install" BITCOIN_CONFIG="--with-gui --enable-reduce-exports --enable-werror --with-boost-process" NO_DEPENDS=1 NOWALLET=1 OSX_SDK="" make HOST=x86_64-apple-darwin16 GOAL="install" DOCKER_NAME_TAG="ubuntu:18.04" PIP_PACKAGES="zmq" GOAL="install" BITCOIN_CONFIG="--with-gui --enable-reduce-exports --enable-werror --with-boost-process" NO_DEPENDS=1 NOWALLET=1 OSX_SDK="" -j4

# test bitcoin core branch commits
alias tbc='git rebase --interactive --exec "make check -j 5 && ./test/functional/test_runner.py -j 5" $(git merge-base bitcoin-core/master HEAD)'

alias export_bdb='export BDB_PREFIX="/home/jon/projects/bitcoin/bitcoin/db4"'
alias configure='./configure --enable-c++17 BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-4.8" BDB_CFLAGS="-I${BDB_PREFIX}/include"'
alias setup='btc ; export_bdb ; ./autogen.sh && configure && make distclean && configure'

alias btcclangsan='setup CC=clang-12 CXX=clang++-12 --enable-debug --disable-bench --with-sanitizers=address,undefined --enable-wleveldb EXTRA_CXXFLAGS="-Weverything -Wall -Werror -Wextra -Wformat -Wvla -Wswitch -Wformat-security -Wconditional-uninitialized -Wthread-safety -Wthread-safety-analysis -Wrange-loop-analysis -Wredundant-decls -Wunused-variable -Wdate-time -Wsign-compare -Wundef -Wtype-limits -Wshadow -Wunreachable-code-loop-increment -Woverloaded-virtual -Wlogical-op -Wduplicated-cond -Wduplicated-branches -DDEBUG_LOCKCONTENTION -DDEBUG_LOCKORDER" ; bmake'

alias btcclang='setup CC=clang-12 CXX=clang++-12 --enable-suppress-external-warnings --enable-debug --enable-endomorphism --disable-bench --enable-wleveldb EXTRA_CXXFLAGS="-Weverything -Wall -Wextra -Wformat -Wvla -Wswitch -Wformat-security -Wconditional-uninitialized -Wmaybe-uninitialized -Wthread-safety -Wthread-safety-analysis -Wrange-loop-analysis -Wredundant-decls -Wunused-variable -Wdate-time -Wsign-compare -Wundef -Wtype-limits -Wshadow -Wunreachable-code-loop-increment -Woverloaded-virtual -Wlogical-op -Wduplicated-cond -Wduplicated-branches -DDEBUG_LOCKCONTENTION -DDEBUG_LOCKORDER" ; bmake'

alias btcclang2='setup CC=clang-12 CXX=clang++-12 --enable-debug --disable-bench --enable-wleveldb -DDEBUG_LOCKCONTENTION -DDEBUG_LOCKORDER" ; bmake'

alias btcclangwerror='setup CC=clang-12 CXX=clang++-12 CXXFLAGS=-Wthread-safety --enable-debug --enable-werror ; bmake'

alias btccomp='setup --enable-debug --enable-suppress-external-warnings --enable-werror --enable-multiprocess --enable-endomorphism --disable-bench EXTRA_CXXFLAGS="-Wlogical-op -Wall -Werror -Wextra -Wformat -Wlogical-op -Wvla -Wswitch -Wformat-security -Wconditional-uninitialized -Wmaybe-uninitialized -Wthread-safety -Wthread-safety-analysis -Wthread-safety-analysis -Wrange-loop-analysis -Wredundant-decls -Wunused-variable -Wdate-time -Wsign-compare -Wundef -Wshadow -Wunreachable-code-loop-increment -Woverloaded-virtual -Wduplicated-cond -Wduplicated-branches -Wpadded -DDEBUG_LOCKCONTENTION -DDEBUG_LOCKORDER" ; bmake'

alias btcbench='setup --enable-bench CXXFLAGS="-O2" ; bmake'

alias btccompf='setup --enable-endomorphism --disable-debug --disable-bench --without-libs --without-qrencode --without-qt --disable-gui-tests -q ; bmake'
alias btccompt='btccomp ; btest'

# test
alias bttcomp='setup --enable-debug --enable-lcov --enable-gprof --disable-bench -q ; bmake'
alias bttcompt='bttcomp ; btest'
# CXXFLAGS="-O0 -g -ggdb3"
# CFLAGS="-O0 -g"

# Fuzzing
alias btcfuzz='./autogen.sh ; ./configure --enable-c++17 --enable-fuzz --with-sanitizers=address,fuzzer,undefined --enable-endomorphism CC=clang-12 CXX=clang++-12 && make distclean ; bmake'
# alias btcfuzz='make distclean ; ./autogen.sh ; export BDB_PREFIX="../db4" ; CC=clang CXX=clang++ ./configure --disable-ccache --enable-tests --enable-fuzz --with-sanitizers=address,fuzzer,undefined BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-4.8" BDB_CFLAGS="-I${BDB_PREFIX}/include" --enable-debug ; bmake'
# time export TMPDIR=/dev/shm src/test/fuzz/utxo_total_supply -jobs=12 -print_final_stats=1 -workers=6 ../qa-assets/fuzz_seed_corpus/utxo_total_supply
# export TMPDIR=/dev/shm ; time src/test/fuzz/utxo_total_supply -jobs=12 -print_final_stats=1 -workers=6
# time src/test/fuzz/utxo_total_supply crash-9c947d9ff00fa36eca41ad27d337743fd5fee54b
# make clean ; ./autogen.sh ; export BDB_PREFIX="../db4" ; CC=clang CXX=clang++ ./configure --enable-fuzz --with-sanitizers=address,fuzzer,undefined BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-4.8" BDB_CFLAGS="-I${BDB_PREFIX}/include" ; bmake

# Latest try
# mkdir /dev/shm/fuzz_temp_seeds
# export TMPDIR=/dev/shm ; time src/test/fuzz/utxo_total_supply /dev/shm/fuzz_temp_seeds

alias pyl="btc ; ./test/lint/lint-python.sh " # run bitcoin-core python linter

alias bcdir="cd ~/.bitcoin/"
alias btdir="cd ~/.bitcoin/testnet" # linux default bitcoin testnet path
alias brdir="cd ~/.bitcoin/regtest" # linux default bitcoin regtest path

alias bd="bitcoind"

alias bcstart="btcsrc; bitcoind -daemon"
alias btstart="btcsrc; bitcoind -testnet -daemon"
alias bsstart="btcsrc; bitcoind -signet  -daemon"
alias brstart="btcsrc; bitcoind -regtest -daemon"

alias bcstop="btcsrc; bitcoin-cli stop"
alias btstop="btcsrc; bitcoin-cli -testnet stop"
alias bsstop="btcsrc; bitcoin-cli -signet stop"
alias brstop="btcsrc; bitcoin-cli -regtest stop"

alias bci="btcsrc; bitcoin-cli "
alias bti="btcsrc; bitcoin-cli -testnet "
alias bsi="btcsrc; bitcoin-cli -signet "
alias bri="btcsrc; bitcoin-cli -regtest "

alias bcps="ps auxww | grep bitcoind"
alias bcps2="ps -ef | grep bitcoind"

alias pow='grep "proof of work failed" ~/.bitcoin/debug.log'
alias mas='grep "mapped AS" ~/.bitcoin/debug.log'

alias deb="rlwrap btcdeb" # Bitcoin Script debugger

# C-Lightning ##################################################################

alias lci="lightning-cli "
alias lc="cd /home/jon/projects/bitcoin/c-lightning"
alias lnup="/home/jon/projects/bitcoin/c-lightning/contrib/bootstrap-node.sh"
alias lup="btcc && lightningd && lnup"

# Tor ##########################################################################

alias tor="cd /usr/local/bin/tor-browser_en-US/ && start-tor-browser.desktop"

# Weather reports
alias wea='curl http://v2.wttr.in/ '
alias nyc='curl http://v2.wttr.in/nyc'
alias sf='curl http://v2.wttr.in/sf'
alias paris='curl http://v2.wttr.in/paris'
alias btz='curl http://v2.wttr.in/biarritz'
