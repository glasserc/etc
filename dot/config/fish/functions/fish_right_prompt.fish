function fish_right_prompt
    # We don't need this; we're pretty much always ourselves
    #echo -n -s "$USER" @
    # We don't need this; it's already in the title bar (see
    # fish_title.fish)
    #echo -n $__fish_prompt_hostname

    set -l color_cwd
    switch $USER
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
        case '*'
            set color_cwd $fish_color_cwd
    end

    echo -n -s (set_color $color_cwd) (prompt_pwd) (set_color normal)

    __show_only_git_branch
    show_nix_status
end

function __show_only_git_branch
    __fish_git_prompt " %s"
end

# This is somewhat incomplete -- ideally we'd show information about
# what kind of shell we were in -- but there appear to be upstream
# political obstacles to getting better nix-shell support for non-bash
# shells. See https://github.com/NixOS/nix/issues/498 for an overview
# and https://github.com/chisui/zsh-nix-shell for an example of what a
# zsh user does by way of contrast.
function show_nix_status
    if set -q IN_NIX_SHELL
        set -l letter N
        set -l color blue
        switch $IN_NIX_SHELL
            case impure
                set color green
        end
        echo -n -s ' ' (set_color $color) $letter (set_color normal)
    end
end
