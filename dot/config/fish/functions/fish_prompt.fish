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
    __fish_git_prompt >/dev/null  # to load __fish_git_prompt_operation_branch_bare
    set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree --short HEAD ^/dev/null)
    if test -n "$repo_info"
        set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
        set -l r $rbc[1]
        if test -n $r
            set -l operation (
            switch $r
                case '|REBASE*'; echo R
                case '|AM*'; echo A
                case '|MERGING*'; echo M
                case '|REVERTING*'; echo R
                case '|BISECTING*'; echo B
                case '*'; echo U
            end)

            __need_space
            echo -n -s (set_color -o $fish_color_error) $operation (set_color normal)
            set -g __need_space true
        end
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
