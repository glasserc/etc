/**
 * comics-mode: a hacked-up copy of xkcd-mode to support oglaf.com
 *
 * This is my first attempt writing anything useful for Conkeror, so
 * it's mostly crap.  Advice would be most appreciated, in particular
 * how to debug these kinds of things when they don't quite work.
 */
require("content-buffer.js");

function find_element_by_id(document, tag, id) {
    return document.evaluate("//"+tag+"[@id='"+id+"']",
                             document, null,
                             Ci.nsIDOMXPathResult.ANY_TYPE,null).iterateNext();
}

function comic_add_title (buffer, imgXPath) {
    var document = buffer.document;
    // Find the <img> tag
    var img = document.evaluate(imgXPath,
        document, null,
        Ci.nsIDOMXPathResult.ANY_TYPE,null).iterateNext();
    if (!img)
        return;
    var title = img.title;
    var parentNode = img.parentNode;
    // In some comics, the <img> is a link, so walk up to the surrounding <A>
    if (parentNode.tagName == 'A')
        parentNode = parentNode.parentNode;
    // Insert the text inside a <p> with a known ID
    var pWithText = function(id, text) {
        var textNode = document.createTextNode(text);
        var span = document.createElement('p');
        span.id = id;
        span.appendChild(textNode);
        return span;
    };
    parentNode.appendChild(pWithText('conkeror:comic-alt-text', img.alt));
    parentNode.appendChild(pWithText('conkeror:comic-title-text', img.title));
}

function oglaf_do_add_title(buffer) {
    comic_add_title(buffer, "//img[@id='strip']");
}

define_page_mode("oglaf-mode",
    build_url_regexp($domain = "oglaf",
                     $allow_www = true,
                     $tlds = ["com"]),
    function enable (buffer) {
        if (buffer.browser.webProgress.isLoadingDocument)
            add_hook.call(buffer, "buffer_loaded_hook", oglaf_do_add_title);
        else
            oglaf_do_add_title(buffer);
        buffer.page.local.browser_relationship_patterns = {};
        buffer.page.local.browser_relationship_patterns[RELATIONSHIP_NEXT] =
            [function(doc){
                 return find_element_by_id(doc, 'div', 'nx');
            }];
        buffer.page.local.browser_relationship_patterns[RELATIONSHIP_PREVIOUS] =
            [function(doc){
                return find_element_by_id(doc, 'div', 'pvs');
            }];
    },
    function disable (buffer) {
        remove_hook.call(buffer, "buffer_loaded_hook", oglaf_do_add_title);
        // When we disable the mode, remove the <span>
        var span = buffer.document.getElementById('conkeror:comic-title-text');
        if (span)
            span.parentNode.removeChild(span);
    },
    $display_name = "oglaf");

page_mode_activate(oglaf_mode);
