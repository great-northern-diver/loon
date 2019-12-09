import webbrowser
def l_help(page="index"):
    '''
    Open a browser with loon's combined (TCL and R) documentation website

    Description:
        `l_help` opens a browser with the relevant page on the
        official combined loon documentation website at
        http://great-northern-diver.github.io/loon/.
    
    Args:
        page: relative path to a page, the .html part may be omitted
    
    @see `l_web`

    Examples:
            l_help()
            l_help("learn_R_intro")
            l_help("learn_R_display_hist")
            l_help("learn_R_bind")
            l_help("learn_R_bind.html#list-reorder-delete-bindings")

    @namespace loon.l_help
    '''
    if(not page.endswith('.html')):
        page += '.html'
    webbrowser.open('http://great-northern-diver.github.io/loon/l_help/' + page)
