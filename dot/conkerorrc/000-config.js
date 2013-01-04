/* Random config options */
url_completion_use_history = true;
url_remoting_fn = load_url_in_new_buffer;
xkcd_add_title = true;

// This isn't perfect -- doesn't show up correct in completions
webjumps['goog'] = webjumps['gg'] = webjumps['google'];

// Default webjump
function possibly_valid_url (str) {
    return /^\s*[^\/\s]*(\/|\s*$)/.test(str)
        && /[:\/\.]/.test(str);
}

read_url_handler_list = [read_url_make_default_webjump_handler('google')];

require('page-modes/google-search-results.js');
google_search_bind_number_shortcuts();

// specify download directory
cwd = get_home_directory();
cwd.append("Desktop");
