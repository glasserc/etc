set -g __last_pwd ""
function fish_prompt --description 'Write out the prompt'
    # Capture last cmd status so we can test against it later.
    # Other commands will stomp on $status.
    set -l last_status $status

    if test $__last_pwd != $PWD
        echo "Now $PWD"
    end
    set -g __last_pwd $PWD

    set -l color_cwd
    set -l suffix
    switch $USER
        case root toor
            set suffix '#'
        case '*'
            set suffix '>'
    end

    if test $last_status -ne 0
        echo -n -s (set_color $fish_color_error) $last_status (set_color normal)
        set -g __need_space true
    end

    # Just show operation status in the prompt.
    # We don't want the whole git prompt, but there's some useful
    # functions defined that we want to use.
    set -l repo_info (command git rev-parse --git-dir 2>/dev/null)
    if test -n "$repo_info"
        set -l operation (__ethan_git_prompt_short_operation $repo_info)
        if test -n $operation
            __need_space
            echo -n -s (set_color -o $fish_color_error) $operation (set_color normal)
            set -g __need_space true
        end
    end

    if test $CMD_DURATION -gt 5000
        __need_space
        echo -n -s $CMD_DURATION 'ms'
        set -g __need_space true
    end

    set -g __need_space ''
    echo -n -s "$suffix "

end

set -g __need_space ''
function __need_space
    if test -n $__need_space
        echo -n -s ' '
        set __need_space ''
    end
end

# Taken from fish_git_prompt.fish so as not to require invoking
# __fish_prompt just to get it. All operations are slow for large git
# repositories such as gecko-git, so only do what's necessary for the
# left prompt here.
# The complete status is shown in the right prompt, but this serves as
# a quick reminder for when in the middle of some operation.
# This is based on __fish_git_prompt_operation_branch_bare, but
# without the branch stuff or detached head checks which I don't care
# about here.
function __ethan_git_prompt_short_operation --description "Ethan's copy of __fish_git_prompt helper, returns one-letter current Git operation"
    # This function is passed the full repo_info array
    set -l git_dir $argv[1]

    set -l operation

    if test -d $git_dir/rebase-merge
        set operation "R"
    else
        if test -d $git_dir/rebase-apply
            if test -f $git_dir/rebase-apply/rebasing
                set operation "R"
            else if test -f $git_dir/rebase-apply/applying
                set operation "A"
            else
                set operation "A"
            end
        else if test -f $git_dir/MERGE_HEAD
            set operation "M"
        else if test -f $git_dir/CHERRY_PICK_HEAD
            set operation "C"
        else if test -f $git_dir/REVERT_HEAD
            set operation "V"
        else if test -f $git_dir/BISECT_LOG
            set operation "B"
        end
    end

    echo $operation
end
