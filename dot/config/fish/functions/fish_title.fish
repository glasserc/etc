function fish_title
    echo $argv[1] ' '
    basename (pwd)
    echo -n ' - '
    echo -n -s "$__fish_prompt_hostname (" (pwd) ")"
end
