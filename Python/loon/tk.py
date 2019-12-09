import tkinter
from pathlib import Path
tk = tkinter.Tk()
tk.withdraw() 
tk.eval('lappend auto_path ' + str(Path(__file__).resolve().parent) + '/Tcl') 
tk.eval('package require loon')
