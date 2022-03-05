# This seems to do something strange when I set them as part of a prompt.
set -g __fish_git_prompt_show_informative_status true
set -g __fish_git_prompt_showcolorhints true

# Take this out in fish 3.2.0
function add_path --description "Utility to add stuff to fish_user_paths idempotently."
    set -l path $argv[1]
    if [ -e $path ]
        contains $path $fish_user_paths; or set -Ua fish_user_paths $path
    end
end

add_path ~/.local/bin

if which direnv >/dev/null
    eval (direnv hook fish)
end
