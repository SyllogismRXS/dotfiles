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

# Don't make it easy to use "rm"
alias rm='echo "This is not the command you are looking for. Use \"trash\"."; false'
# If you really want to use rm, simply prepend a slash to bypass the alias:
# \rm file-without-hope
