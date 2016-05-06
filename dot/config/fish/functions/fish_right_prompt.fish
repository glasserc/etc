function fish_right_prompt
    # We don't need this; we're pretty much always ourselves
    #echo -n -s "$USER" @
    echo -n "$__fish_prompt_hostname"
end
