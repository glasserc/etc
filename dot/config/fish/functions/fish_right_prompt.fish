function fish_right_prompt
    # We don't need this; we're pretty much always ourselves
    #echo -n -s "$USER" @
    # We don't need this; it's already in the title bar (see
    # fish_title.fish)
    #echo -n $__fish_prompt_hostname

    __show_only_git_branch
end

function __show_only_git_branch
    __fish_git_prompt "%s"
end
