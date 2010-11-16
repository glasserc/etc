alias cclive="cclive --filename-format='%t.%s'"
alias ogg2mp3="for file in *.ogg; do
  gst-launch-0.10 filesrc location=\"\${file}\" ! oggdemux ! vorbisdec ! audioconvert ! lame quality=4 ! id3v2mux ! filesink location=\"\${file/.ogg/.mp3}\";
done"
