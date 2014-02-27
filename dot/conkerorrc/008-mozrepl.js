// Taken from http://conkeror.org/MozRepl
// Start mozrepl, listening on port 4242
// To use, telnet localhost 4242
//
// This doesn't seem necessary??
// let (mozrepl_init = get_home_directory()) {
//     mozrepl_init.appendRelativePath("etc/mozrepl-conkeror.js");
//     session_pref('extensions.mozrepl.initUrl', make_uri(mozrepl_init).spec);
// }

if ('@hyperstruct.net/mozlab/mozrepl;1' in Cc) {
  let mozrepl = Cc['@hyperstruct.net/mozlab/mozrepl;1']
    .getService(Ci.nsIMozRepl);
  if (! mozrepl.isActive())
    mozrepl.start(4242, true);
}
