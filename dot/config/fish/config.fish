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

# ASDF
# Installation instructions have you install to ~/.asdf. Move that
# instead to ~/.config/asdf. See also
# https://github.com/asdf-vm/asdf/issues/687 (and linked issues).
# asdf uses ASDF_DATA_DIR to figure out where it should be installed.
set -x ASDF_DATA_DIR ~/.config/asdf
if test -f $ASDF_DATA_DIR
    # Using asdf only through direnv
    # See https://github.com/asdf-community/asdf-direnv, under "Pro-Tips".
    source $ASDF_DATA_DIR/lib/asdf.fish
end
add_path $ASDF_DATA_DIR/bin

# direnv installed system-wide using apt rather than managed using asdf
#eval (asdf exec direnv hook fish)
