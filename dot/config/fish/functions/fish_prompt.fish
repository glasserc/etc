set -g __last_pwd ""
function fish_prompt --description 'Write out the prompt'
    # Capture last cmd status so we can test against it later.
    # Other commands will stomp on $status.
    set -l last_status $status

    if test $__last_pwd != $PWD
        echo "Now $PWD"
    end
    set -g __last_pwd $PWD

    # Just calculate this once, to save a few cycles when displaying the prompt
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    set -l color_cwd
    set -l suffix
    switch $USER
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
            set suffix '#'
        case '*'
            set color_cwd $fish_color_cwd
            set suffix '>'
    end

    if test $last_status -ne 0
        echo -n -s (set_color $fish_color_error) $last_status (set_color normal) ' '
    end

    echo -n -s (set_color $color_cwd) (prompt_pwd) (set_color normal)

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

            echo -n -s ' ' (set_color -o $fish_color_error) $operation (set_color normal)
        end
    end

    echo -n -s "$suffix "

end
