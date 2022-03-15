export EDITOR=/usr/bin/vim
export _JAVA_AWT_WM_NONREPARENTING=1
export PATH="$PATH:/home/ben/Android/Sdk/emulator/"
export PATH="$PATH:/home/ben/.local/share/gem/ruby/3.0.0/bin/"
export PATH="$PATH:/home/ben/.emacs.d/bin/"
export PATH="$PATH:/home/ben/.local/bin"


. ~/.aliases
. /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

unsetopt NOMATCH
zstyle :compinstall filename '/home/ben/.zshrc'

autoload -Uz add-zsh-hook
autoload -Uz compinit
compinit

# Add git info
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%F{#474747}%K{#474747}%F{#d44ca2}  %b '

# Use bat as manpager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# Set prompt
setopt PROMPT_SUBST
# PROMPT='%K{#474747} %B%F{green}%n%f%F{#e9cc42}@%F{#e9454d}%m%f %B%~%b%f %F{cyan}$%F{white}%b %K{#1c1c1c}%F{#474747}%F{white}%K{#1c1c1c}%b%f%k '
PROMPT='%K{#474747} %B%F{green}%F{white} %B%1d%b%f %F{cyan}$%F{white}%b %K{#1c1c1c}%F{#474747}%F{white}%K{#1c1c1c}%b%f%k '
RPROMPT='$vcs_info_msg_0_'

# Set vi mode
bindkey -v

# Print current directory as terminal title
function terminal_title () {
   print -Pn -- '\e]2;%~\a'
}
add-zsh-hook -Uz chpwd terminal_title

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

function zvm_after_select_vi_mode() {
  case $ZVM_MODE in
    $ZVM_MODE_NORMAL)
      RPROMPT='NORMAL $vcs_info_msg_0_'
    ;;
    $ZVM_MODE_INSERT)
      RPROMPT='INSERT $vcs_info_msg_0_'
    ;;
    $ZVM_MODE_VISUAL)
      RPROMPT='VISUAL $vcs_info_msg_0_'
    ;;
    $ZVM_MODE_VISUAL_LINE)
      RPROMPT='VLINE $vcs_info_msg_0_'
    ;;
    $ZVM_MODE_REPLACE)
      RPROMPT='REPLACE $vcs_info_msg_0_'
    ;;
  esac
}
