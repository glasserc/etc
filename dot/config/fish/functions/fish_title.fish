function fish_title
    # Just calculate this once, to save a few cycles when displaying the prompt
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    echo $argv[1] ' '
    basename (pwd)
    echo -n ' - '
    echo -n -s "$__fish_prompt_hostname (" (pwd) ")"
end
