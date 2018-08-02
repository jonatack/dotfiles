#!/bin/bash
# ~/.bash_aliases dotfile

alias e='emacs '
alias ee='emacs &'

alias aliases='emacs ~/.bash_aliases &'
alias profile='emacs ~/.profile &'

# https://bearmetal.eu/theden/rails-garbage-collection-tuning-approaches/
# launch rails server with the following to get Ruby 2.2 GC suggestions:
# RUBY_GC_TOKEN=e61cdabb57bd6f29ad03121a3addb5f2 RUBY_GC_TUNE=1 bundle exec rails s
# RUBY_GC_TOKEN=2c1364c5329961b755b0afb6cb3081c9 RUBY_GC_TUNE=1 bundle exec rails s

# Repeat last command
# !!
# Repeat last command that started with x
# !x
# Repeat last command that has the substring x
# !?x
# Repeat 10th command in the history file
# !10
# Repeat 10th from last command in the history file
# !-10

# Easier navigation: .., ..., ...., ....., ~ and -
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

# List all files colorized in long format, sans dot files
# alias la="ls -lF ${colorflag}"

# List all files colorized in long format, including dot files
alias l="ls -laF ${colorflag}"
alias list='ls -lha'

# List only directories
alias lsd="ls -lF ${colorflag} | grep --color=never '^d'"

# Always use color output for `ls`
alias la="command ls -G ${colorflag}"
alias ls="command ls -a -G ${colorflag}"

# lighter, nicer colors:
# export LSCOLORS=GxFxCxDxBxegedabagaced
# export TERM=xterm-256color
# darker colors:
# export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'

# Solarized colorscheme for GNU `ls` environment variable:
export LS_COLORS="no=00:fi=00:di=36:ln=35:pi=30;44:so=35;44:do=35;44:bd=33;44:cd=37;44:or=05;37;41:mi=05;37;41:ex=01;31:*.cmd=01;31:*.exe=01;31:*.com=01;31:*.bat=01;31:*.reg=01;31:*.app=01;31:*.txt=32:*.org=32:*.md=32:*.mkd=32:*.h=32:*.c=32:*.C=32:*.cc=32:*.cpp=32:*.cxx=32:*.objc=32:*.sh=32:*.csh=32:*.zsh=32:*.el=32:*.vim=32:*.java=32:*.pl=32:*.pm=32:*.py=32:*.rb=32:*.hs=32:*.php=32:*.htm=32:*.html=32:*.shtml=32:*.erb=32:*.haml=32:*.xml=32:*.rdf=32:*.css=32:*.sass=32:*.scss=32:*.less=32:*.js=32:*.coffee=32:*.man=32:*.0=32:*.1=32:*.2=32:*.3=32:*.4=32:*.5=32:*.6=32:*.7=32:*.8=32:*.9=32:*.l=32:*.n=32:*.p=32:*.pod=32:*.tex=32:*.bmp=33:*.cgm=33:*.dl=33:*.dvi=33:*.emf=33:*.eps=33:*.gif=33:*.jpeg=33:*.jpg=33:*.JPG=33:*.mng=33:*.pbm=33:*.pcx=33:*.pdf=33:*.pgm=33:*.png=33:*.PNG=33:*.ppm=33:*.pps=33:*.ppsx=33:*.ps=33:*.svg=33:*.svgz=33:*.tga=33:*.tif=33:*.tiff=33:*.xbm=33:*.xcf=33:*.xpm=33:*.xwd=33:*.xwd=33:*.yuv=33:*.aac=33:*.au=33:*.flac=33:*.mid=33:*.midi=33:*.mka=33:*.mp3=33:*.mpa=33:*.mpeg=33:*.mpg=33:*.ogg=33:*.ra=33:*.wav=33:*.anx=33:*.asf=33:*.avi=33:*.axv=33:*.flc=33:*.fli=33:*.flv=33:*.gl=33:*.m2v=33:*.m4v=33:*.mkv=33:*.mov=33:*.mp4=33:*.mp4v=33:*.mpeg=33:*.mpg=33:*.nuv=33:*.ogm=33:*.ogv=33:*.ogx=33:*.qt=33:*.rm=33:*.rmvb=33:*.swf=33:*.vob=33:*.webm=33:*.wmv=33:*.doc=31:*.docx=31:*.rtf=31:*.dot=31:*.dotx=31:*.xls=31:*.xlsx=31:*.ppt=31:*.pptx=31:*.fla=31:*.psd=31:*.7z=1;35:*.apk=1;35:*.arj=1;35:*.bin=1;35:*.bz=1;35:*.bz2=1;35:*.cab=1;35:*.deb=1;35:*.dmg=1;35:*.gem=1;35:*.gz=1;35:*.iso=1;35:*.jar=1;35:*.msi=1;35:*.rar=1;35:*.rpm=1;35:*.tar=1;35:*.tbz=1;35:*.tbz2=1;35:*.tgz=1;35:*.tx=1;35:*.war=1;35:*.xpi=1;35:*.xz=1;35:*.z=1;35:*.Z=1;35:*.zip=1;35:*.ANSI-30-black=30:*.ANSI-01;30-brblack=01;30:*.ANSI-31-red=31:*.ANSI-01;31-brred=01;31:*.ANSI-32-green=32:*.ANSI-01;32-brgreen=01;32:*.ANSI-33-yellow=33:*.ANSI-01;33-bryellow=01;33:*.ANSI-34-blue=34:*.ANSI-01;34-brblue=01;34:*.ANSI-35-magenta=35:*.ANSI-01;35-brmagenta=01;35:*.ANSI-36-cyan=36:*.ANSI-01;36-brcyan=01;36:*.ANSI-37-white=37:*.ANSI-01;37-brwhite=01;37:*.log=01;32:*~=01;32:*#=01;32:*.bak=01;36:*.BAK=01;36:*.old=01;36:*.OLD=01;36:*.org_archive=01;36:*.off=01;36:*.OFF=01;36:*.dist=01;36:*.DIST=01;36:*.orig=01;36:*.ORIG=01;36:*.swp=01;36:*.swo=01;36:*,v=01;36:*.gpg=34:*.gpg=34:*.pgp=34:*.asc=34:*.3des=34:*.aes=34:*.enc=34:*.sqlite=34:"

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Get week number
alias week='date +%V'

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
# alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# View HTTP traffic
# alias sniff="sudo grep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Canonical hex dump; some systems have this symlinked
command -v hd > /dev/null || alias hd="hexdump -C"

# Trim new lines and copy to clipboard
# alias c="tr -d '\n' | pbcopy"

# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
# alias map="xargs -n1"

alias be='bundle exec'

alias rs='bin/rails server'
alias rc='bin/rails console'
# alias rt='bin/rails test'
alias rtr='bin/rails test && rubocop'

alias g='git'
alias gs='git status'
alias gd='git diff'
alias gds='git diff --staged'
alias ga='git add'
alias gc='git checkout'
alias gbr='git branch'
alias grm='git rm'
alias gmv='git mv'
alias gph='git push'
alias gpl='git pull'
alias gcm='git commit -m'
alias gca='git commit -a'
alias gphm='git push heroku master'

alias sau='sudo apt update && apt list -u -a'
alias alu='apt list --upgradable --all-versions'
alias safu='sudo apt full-upgrade'
alias sar='sudo apt autoremove'
alias sac='sudo apt autoclean'

alias gu='gem update --system && gem update && gem uninstall fileutils'
alias bu='bundle update'
alias bo='bundle open'

alias dc='docker-compose '

alias ff='cd ~/projects/fallenfest && l'
alias ff2='cd '~/projects/fallenfest2-static' && l'
alias frs='cd '~/projects/fallenfest2-static' && rs'
alias frc='cd '~/projects/fallenfest2-static' && rc'

alias sbcl='rlwrap sbcl'
alias cloj='lein repl'

alias haskell='ghci'
alias hask='ghci'
alias ghc='ghci'

alias erlang='erl'
# alias elixir='iex'

alias usr='cd /usr/local && l'
alias home='cd ~ && l'
alias projects='cd ~/projects && ls'
alias p='cd ~/projects && l'
alias ruby='cd ~/projects/ruby && l'
alias dot='cd ~/projects/dotfiles && l'
alias btc='cd ~/projects/bitcoin && l'
alias cl='cd ~/commmon-lisp/ && l'
alias ql='cd ~/quicklisp/ && l'
alias lp='cd ~/quicklisp/local-projects/ && ls'
# alias tests='cd ~/projects/ruby/tests && l'
alias dl='cd ~/Download && l'
alias doc='cd ~/Documents && l'

alias logs='cd /var/log && l'

alias kraken='cd ~/projects/ruby/kraken_ruby_client && l'
alias adp='cd ~/projects/ruby/cenpil-back'
# alias ransack='cd ~/projects/ruby/ransack-activerecord-hackery & l'
# alias poly='cd ~/projects/ruby/polyamorous-activerecord-hackery & l'
alias logclear='bundle exec rake log:clear'
alias lc='bundle exec rake log:clear'

app="-a fallenfest2"
alias hps="heroku ps ${app}"
alias ps1="heroku ps:scale web=1 ${app}"
alias ps2="heroku ps:scale web=2 ${app}"
alias hpg="heroku pg ${app}"
alias hrs="heroku restart ${app} && hps"
alias console="heroku run console ${app}"
alias fflogs="heroku logs -t ${app}"

# alias update="sudo apt update && sudo apt full-upgrade && gem update --system; gem update; bundle update; brew update && brew upgrade && brew doctor"

alias compile='RAILS_ENV=production bundle exec rake assets:precompile'

# alias api='g stash; g fetch origin; g rebase origin/master; g stash pop && g stash list; lc; rs'

# alias webapp='g co master; g stash; g fetch origin; g rebase origin/master; g stash pop && g stash list; npm update -g; npm run dev:no-debug'

alias cop='rubocop '
alias copa='rubocop --auto-gen-config'

# alias prompt='ps1_set --prompt λ' # ∴
alias path='echo $PATH'

# Reload the shell (i.e. invoke as a login shell)
# alias reload="exec $SHELL -l"
alias reload='source ~/.bashrc && source ~/.profile && source ~/.bash_aliases'

alias pg-start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pg-stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias pg='sudo su - _pgsql'

alias mysql-start='mysql.server start'
alias mysql-stop='mysql.server stop'

alias mem='memcached -vv'
# To have launchd start memcached at login:
#   ln -sfv /usr/local/opt/memcached/*.plist ~/Library/LaunchAgents
# To load memcached now:
#   launchctl load ~/Library/LaunchAgents/homebrew.mxcl.memcached.plist
# Or, if you don't want/need launchctl, you can just run:
#   /usr/local/opt/memcached/bin/memcached

alias pippi='USE_PIPPI=true bundle exec rake test && cat log/pippi.log'

# print Ruby global variables by value
alias ruby_global_vars="ruby -rpp -e'pp global_variables.group_by { |v| eval v.to_s }.invert'"

alias ruby-globals='ruby_global_vars'

alias todo="vim +'normal Go' +'r!date' ~/Documents/Text\ files/todo.txt"
alias did="vim +'normal Go' +'r!date' ~/Documents/Text\ files/did.txt"
alias ndid="vim +'normal Go' +'r!date' +'normal o' +':exe \"normal i=============================\<Esc>\"' ~/Documents/Text\ files/did.txt"
alias did2="vim +'normal G' +startinsert ~/Documents/Text\ files/did.txt"

alias jour=journal

alias t='python ~/projects/python/t/t.py --task-dir ~/tasks --list tasks'
alias tasks='emacs ~/tasks/tasks &'
alias tc='t | wc -l'
