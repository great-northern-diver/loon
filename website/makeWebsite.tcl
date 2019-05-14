#! /usr/bin/env tclsh

if {[llength $argv] eq 0} {
    set todo all
    ## Delete Old Website
    catch {exec sh -c "exec rm  html/*.html"}
} else {
    set todo $argv
}

if {[set tcl_version] ne "8.6"} {
    proc lmap {_var list body} {
        upvar 1 $_var var
        set res {}
        foreach var $list {lappend res [uplevel 1 $body]}
        set res
    }    
}

set cmd [list pandoc -c style.css\
             -f markdown+header_attributes+fenced_code_blocks+line_blocks+pipe_tables\
             -s --quiet]


if {"all" in $todo || "learn" in $todo} {
    ## Need md/generated directory
    if {![file exists [file join md generated]]} {
        file mkdir [file join md generated]
    }
    
    exec perl keepSections.pl Tcl
    exec perl keepSections.pl R
#    exec perl keepSections.pl Python        
}

if {"all" in $todo || "install" in $todo} {
    ## Install
    foreach topic {install linkingActiveTcl commands} {
        exec {*}$cmd --toc --toc-depth=5\
            {*}[lmap file {title no top_navigation nc}\
                    {format "--include-before-body=headers/%s.html" $file}]\
            -o html/$topic.html md/$topic.md
    }
}

if {"all" in $todo || "home" in $todo} {
    ## Home
    exec {*}$cmd {*}[lmap file {title no top_navigation nc}\
                         {format "--include-before-body=headers/%s.html" $file}]\
        -o html/index.html md/index.md
}

if {"all" in $todo || "ui" in $todo} {
    ## UI
    exec {*}$cmd {*}[lmap file {title no top_navigation nc}\
                         {format "--include-before-body=headers/%s.html" $file}]\
        -o html/UI.html md/UI.md
    
}

if {"all" in $todo || "learn" in $todo} {
    ## Learn
    set languages [list Tcl R]
    set learn_topics [lrange $argv 1 end]
    set learn_topics [list intro states linking bind layer layout]
    
    foreach language $languages {
        foreach topic $learn_topics {
            set file "learn_${language}_${topic}"
            exec {*}$cmd --toc --toc-depth=5\
                {*}[lmap html_file [list title no top_navigation\
                                        learn learn_$language nc]\
                        {format "--include-before-body=headers/%s.html" $html_file}]\
                -o html/${file}.html md/generated/${file}.md
        }
    }
    
    set learn_topics [list display_hist display_plot display_pairs display_serialaxes display_graph display_inspectors]
    
    foreach language $languages {
        foreach topic $learn_topics {
            set file "learn_${language}_${topic}"
            exec {*}$cmd --toc --toc-depth=5\
                {*}[lmap html_file [list title no top_navigation\
                                        learn learn_$language learn_displays nc]\
                        {format "--include-before-body=headers/%s.html" $html_file}]\
                -o html/${file}.html md/generated/${file}.md
        }
    }
    
    
    ## Learn tcltk and tkinter
    exec {*}$cmd --toc --toc-depth=5\
        {*}[lmap file {title no top_navigation learn learn_R nc}\
                {format "--include-before-body=headers/%s.html" $file}]\
        -o html/learn_R_tcltk.html md/learn_R_tcltk.md
    
    exec {*}$cmd --toc --toc-depth=5\
        {*}[lmap file {title no top_navigation learn learn_Python nc}\
                {format "--include-before-body=headers/%s.html" $file}]\
        -o html/learn_Python_tkinter.html md/learn_Python_tkinter.md
    
    exec {*}$cmd --toc --toc-depth=5\
        {*}[lmap file {title no top_navigation learn learn_Tcl nc}\
                {format "--include-before-body=headers/%s.html" $file}]\
        -o html/learn_Tcl_Tk.html md/learn_Tcl_Tk.md    
}


if {"all" in $todo || "videos" in $todo} {
    
    ## Videos
    exec {*}$cmd --toc --toc-depth=5\
        {*}[lmap file {title no top_navigation nc}\
                {format "--include-before-body=headers/%s.html" $file}]\
        -o html/videos.html md/videos.md
}

if {"all" in $todo || "gallery" in $todo} {
    ## Gallery
    exec {*}$cmd {*}[lmap file {title no top_navigation nc}\
                         {format "--include-before-body=headers/%s.html" $file}]\
        -o html/gallery.html md/gallery.md
    

}

exit
    # Manual


if {"all" in $todo || "develop" in $todo} {
    # develop
    
    set develop_files {develop_setup develop_framework}
    
    foreach topic $develop_files {
        set file "learn_${language}_${topic}"
        exec {*}$cmd --toc --toc-depth=5\
            {*}[lmap html_file [list title no top_navigation\
                                    learn learn_$language nc]\
                    {format "--include-before-body=headers/%s.html" $html_file}]\
            -o html/${file}.html md/generated/${file}.md
    }
}




#exec ./reformat_manual_Tcl.tcl
#exec ./createRmanual.tcl
