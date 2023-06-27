export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias dbt='set -a; source ${DBT_REPO_PATH}/.env; ${DBT_REPO_PATH}/venv/bin/dbt'


function pretty_csv {
    perl -pe 's/((?<=,)|(?<=^)),/ ,/g;' "$@" | column -t -s, | less  -F -S -X -K
}

function rv {
    echo 'opening vscode in python 3.9.11 environment'
    echo 'current shell version';
    current_python_version=$(python --version);
    echo $current_python_version;
    pyenv shell 3.9.11;
    code .;
    pyenv shell --unset; 
    echo 'new shell version after unset (likely the global one)';
    new_python_version=$(python --version);
    echo $new_python_version;
}

alias lh="ls -lt | head"
alias profile="code ~/.zshrc"
alias vs="code"
alias gs="git status"
alias src="source ~/.zshrc"
alias ok="git add .; git commit"
alias gco="git checkout"
alias gap="git add -p"
alias gpr="git pull --rebase"
alias gdw="git diff --word-diff"
alias todo="git add TODO.md; git commit -m 'update todo'"
alias jp="jupyter lab"

export GITHUB_USERNAME="ryninho"
