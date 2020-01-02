from .onload import *
from .tk import tk
from .l_size import l_size 
import os 
from .helper import file_ext
from sys import exit
def l_export(widget, filename, width = None, height = None):
    '''Export a loon plot as an image

    The supported image formats are dependent on the systemenvironment.
    Plots can always be exported to the PostScript format.
    Exporting displays as '.pdf' is only possible when the command line tool
    epstopdf is installed. Finally, exporting to either `png`, `jpg`,
    `bmp, `tiff` or `gif` requires the Img Tcl extension.
    When choosing one of the formats that depend on the Img extension,
    it is possible to export any Tk widget as an image including inspectors.
    
    Args:
        filename: path of output file
        width: image width in pixels
        height: image height in pixels

    Details:
        Note that the `CTRL-P` key combination opens a dialog to export
        the graphic.
    
    Returns:
        path to the exported file

    See Also: 
        `l_export_valid_formats`

    '''
    if(not isinstance(widget,str)):
        widget = widget.plot
    if (width == None):
        width = l_size(widget)[0]
    if (height == None):
        height = l_size(widget)[1]

    valid_extensions = l_export_valid_formats()

    file_extension = file_ext(filename)

    if (file_extension in valid_extensions):
        fname = tk.tk.call("::loon::export",widget, filename, width, height)
    else:
        if(file_extension == None): 
            return None
        else:
            exit("The file format '" + file_extension + 
                "' is not supported on your system.\nSupported formats are: ",
                ','.join(valid_extensions) +  '.')
    return fname

def l_export_valid_formats():
    '''Return a list of the available image formats when exporting a loon plot

    The supported image formats are dependent on the system
    environment. Plots can always be exported to the Postscript format.
    Exporting displays as .pdfs is only possible when the command line tool
    epstopdf is installed. Finally, exporting to either png, jpg, bmp, tiff or
    gif requires the Img Tcl extension. When choosing one of the formats that
    depend on the Img extension, it is possible to export any Tk widget as an
    image including inspectors.
    
    Returns: 
        a vector with the image formats available for exporting a loon plot.
    
    '''
    valid_extensions = ["ps", "eps"]
    # if (Sys.which('epstopdf') != "") {
    #     valid_extensions <- c(valid_extensions, "pdf")
    # }
    # if (.withTclImg) {
    #     valid_extensions <- c(valid_extensions,
    #                           'jpg','jpeg','png','bmp','tiff','gif')
    # }
    # valid_extensions
    valid_extensions.append('pdf')
    return valid_extensions



filetypes = {'png':"Portable Network Graphics",'jpg':"JPEG",'ps':"Postscript",
            'eps':'Encapsulated Postscript','pdf':"Portable Document Graphics",
            'tiff':"Tagged Image File Format",'bmp':"Bitmap",
            'gif':"Graphics Interchange Format"}

def exportImageDialog(widget):
    if(not isinstance(widget,str)):
        widget = widget.plot
    fm = l_export_valid_formats()
    formats = [x for x in fm if x != 'jpeg']
    fnames = {x: filetypes[x] for x in formats}
    types = ""
    for x,y in fnames.items():
        types = types + '{ {' + y + '} {.' + x + '} } ' 
    types = types[:-1]

    fileName = tk.tk.call('tk_getSaveFile','-initialdir',os.getcwd(),
                          '-initialfile','loon_plot','-title','Export Plot As Image',
                          '-parent',tk.tk.call('winfo','toplevel',widget),
                          '-filetypes',types)
    if (isinstance(fileName,str)):
        l_export(widget, fileName)
    return(fileName)

