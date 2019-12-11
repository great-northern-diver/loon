from .helper import match_arg
import webbrowser

def l_web(page="index", directory = "home"):
    '''Open a browser with loon's R documentation webpage

    `l_web` opens a browser with the relevant page on the
    official loon documentation website at
    http://great-northern-diver.github.io/loon/.
    
    Args:
        page: relative path to a page, the .html part may be omitted
        directory: if "home" then `page` is ignored and the
                   browser will open at the home page of the official loon documentation website at
                   http://great-northern-diver.github.io/loon/.  If `page` refers to a `loon`
                   manual reference, then directory must be "reference"; if `page` refers to the name
                   of a vignette file, then directory should be "articles"

    See Also: 
        `l_help` 

    Examples:
        >>> l_web()
        >>> l_web(page = "introduction", directory = "articles")
        >>> l_web(page = "l_hist", directory = "reference")
    '''
    dir_options = ["home", "reference", "articles"]
    dir = match_arg(directory,dir_options,'directory')
    loonSite = "http://great-northern-diver.github.io/loon/"
    
    if (dir == "home"):
        loonSite = loonSite + "index.html"
    else:
        if(not page.endswith('.html')):
            page += '.html'
        loonSite = "http://great-northern-diver.github.io/loon/" + dir+ "/"+ page
    webbrowser.open(loonSite)
