# Path to Oh My Fish install.
set -gx OMF_PATH ~/.local/share/omf

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG ~/.config/omf

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

alias gst='git status'
alias gd='git diff'
alias mv='mv -i'
alias tree='tree -C'
alias j=jump
alias clear='printf "\033c"'
alias e='emacsclient -t'
alias ec='emacsclient -c'

# Auto-launching ssh-agent
setenv SSH_ENV $HOME/.ssh/environment

function start_agent 
    echo "Initializing new SSH agent ..."
    ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
    echo "succeeded"
    chmod 600 $SSH_ENV 
    . $SSH_ENV > /dev/null
    ssh-add
end

function test_identities 
    ssh-add -l | grep "The agent has no identities" > /dev/null
    if [ $status -eq 0 ]
        ssh-add
        if [ $status -eq 2 ]
            start_agent
        end
    end
end

if [ -n "$SSH_AGENT_PID" ] 
    ps -ef | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
    if [ $status -eq 0 ]
        test_identities
    end  
else
    if [ -f $SSH_ENV ]
        . $SSH_ENV > /dev/null
    end  
    ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep ssh-agent > /dev/null
    if [ $status -eq 0 ]
        test_identities
    else 
        start_agent
    end  
end

# man with vim w/o plugins
function man
  vim -u ~/.minvimrc -c ":map q :q<CR>" -c ":Man $argv" -c ":only | :set ts=8"
end

set -x EDITOR /bin/vim

# set PATH $PATH "/cygdrive/c/Program Files (x86)/sbt/bin"
# set PATH $PATH "/cygdrive/c/Program Files/Java/jdk1.7.0_79/bin"
