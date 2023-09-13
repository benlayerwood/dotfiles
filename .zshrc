export _JAVA_AWT_WM_NONREPARENTING=1
export BAT_THEME="Visual Studio Dark+"
export PATH="$PATH:$HOME/.emacs.d/bin"
export PATH="$PATH:$HOME/.local/bin"

#ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"

# Env variables for cryptolab
export PRAKTIKUM_NAME="beju"
export PRAKTROOT="$HOME/Code/cryptolab"

. ~/.aliases
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-completions/zsh-completions.plugin.zsh

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

unsetopt NOMATCH
zstyle :compinstall filename '$HOME/.zshrc'

#autoload -Uz add-zsh-hook
#autoload -Uz compinit
#compinit

# Add git info
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%F{#474747}%K{#474747}%F{#d44ca2}  %b '

# Use bat as manpager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# Set prompt
setopt PROMPT_SUBST
PROMPT='%K{#474747} %B%F{green}%F{white} %B%1d%b%f %F{cyan}$%F{white}%b %K{#1c1c1c}%F{#474747}%F{white}%K{#1c1c1c}%b%f%k '

# Add git symbol on the right
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

bindkey "''${key[Up]}" up-line-or-search

# Dirstack
DIRSTACKFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/dirs"
if [[ -f "$DIRSTACKFILE" ]] && (( ${#dirstack} == 0 )); then
   dirstack=("${(@f)"$(< "$DIRSTACKFILE")"}")
   [[ -d "${dirstack[1]}" ]] && cd -- "${dirstack[1]}"
fi

chpwd_dirstack() {
   print -l -- "$PWD" "${(u)dirstack[@]}" > "$DIRSTACKFILE"
}
add-zsh-hook -Uz chpwd chpwd_dirstack

DIRSTACKSIZE='20'

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME

## Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

## This reverts the +/- operators.
setopt PUSHD_MINUS
