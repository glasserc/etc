alias cclive="cclive --filename-format='%t.%s'"
alias ogg2mp3="for file in *.ogg; do
  gst-launch-0.10 filesrc location=\"\${file}\" ! oggdemux ! vorbisdec ! audioconvert ! lame quality=4 ! id3v2mux ! filesink location=\"\${file/.ogg/.mp3}\";
done"
alias mattrib_clean="sudo mattrib -S -H -/ ::/ -i"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
