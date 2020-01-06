# This should be sourced by ~/.bashrc

SSH_ENV=$HOME/.ssh/environment

# start the ssh-agent
function start_agent {
    echo "Initializing new SSH agent..."
    # spawn ssh-agent
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add
}

if [ -f "${SSH_ENV}" ]; then
     . "${SSH_ENV}" > /dev/null
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi


export PS1='\[\e[1;37m\]\[\e[1;32m\]\u\[\e[0;39m\]@\[\e[1;36m\]\h\[\e[0;39m\]:\[\e[1;33m\]\w\[\e[0;39m\]\[\e[1;35m\]$(__git_ps1 "(%s)")\[\e[0;39m\]\[\e[1;37m\]\[\e[0;39m\]$ '

export PATH=~/repos/private/scripts:${PATH}

alias ec='emacsclient'
export EDITOR="emacsclient"

# Install Ruby Gems to ~/gems
export GEM_HOME=$HOME/gems
export PATH=$HOME/gems/bin:$PATH

# added by travis gem
[ -f /home/syllogismrxs/.travis/travis.sh ] && source /home/syllogismrxs/.travis/travis.sh

# GO installation
export GOROOT=/usr/local/go
export GOPATH=$HOME/repos/go-repos
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/syllogismrxs/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/syllogismrxs/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/syllogismrxs/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/syllogismrxs/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
