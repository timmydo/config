alias lal='ls -al'
alias ll='ls -l'
alias k=kubectl
alias ksn='kubectl get nodes | sed 1d | fzf +m | awk '\''{print $1}'\'''
alias ksp='kubectl get pods | sed 1d | fzf +m | awk '\''{print $1}'\'''
alias krmevicted='kubectl get po | grep Evicted| awk '\''{print $1}'\''| xargs -n 1 kubectl delete pod'
alias www='swaymsg exec -- epiphany --new-window'
