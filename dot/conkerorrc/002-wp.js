require("page-modes/wikipedia.js");
wikipedia_webjumps_format = "wp-%s"; // controls the names of the webjumps.  default is "wikipedia-%s".
define_wikipedia_webjumps("en", "fr"); // For English, German and French.
// define_wikipedia_webjumps(); // To make use of ALL of the webjumps (200+).

webjumps['wp'] = webjumps['wp-en'];
