# ~/.bashrc: executed by bash(1) for non-login shells.
export EDITOR=/usr/bin/vim
export _JAVA_AWT_WM_NONREPARENTING=1
export XENVIRONMENT="$HOME/.Xresources"

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Use vim as default manpage viewer
if [ $HOSTNAME != "fedora" ] 
then
#export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
#    vim -R -c 'set ft=man nomod nolist nonumber' -c 'map q :q<CR>' \
#    -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
#    -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

export BAT_THEME="Visual Studio Dark+"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
   PS1="\[\033[1;31m\]\u\[\033[01;33m\]@\[\033[01;96m\]\h \[\033[0;32m\]\W \[\033[0;31m\]\[\e[01;33m\]$\[\e[0m\] "
else
   PS1='[\u@\h \W]\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Alias definitions.
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#Turn off system bell
if [ -n "$DISPLAY" ]; then
  xset b off
fi

set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

#Add pfetch for non dropdown-terminal
#emulator=$(basename "/"$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //'))
#test $emulator = tilda || pfetch
#if [ -f /bin/pfetch -o -f /usr/local/bin/pfetch ] && [ $emulator != "xterm" ] && [ $emulator != "tilda" ] && [ $emulator != "process.coffee" ] && [ $emulator != "--type=ptyHost" ] && [ $emulator != "com.intellij.idea.Main" ]; then
#	pfetch
#fi
