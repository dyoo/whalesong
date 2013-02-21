(function(window, undefined) {
    // dyoo: this library has been modified slightly so it checks to see
    // if window.jQuery has already been installed.  This is to prevent an ugly issue
    // with regards to a memory leak if one tries to repeatedly load jQuery.
    // NOTE: this portion of the file (jquery-protect-header.js) is intentionally
    // unbalanced.  It'll be closed up by jquery-protect-footer.js.
    if (!window.jQuery) {
