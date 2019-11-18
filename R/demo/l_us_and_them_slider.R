
local({
    
    ## Example from Hans Rosling Talk
    ## https://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen
    
    data("UsAndThem")
    
    scale01 <- function(x) {(x-min(x))/diff(range(x))}
    
    tt <- tktoplevel()
    tktitle(tt) <- "Us and Them"
    
    colorkey = list('America'='#E5FF2F',
                    'Europe & Central Asia'='#FF982F',
                    'Middle East & North Africa'='#68FF5E',
                    'Sub-Saharan Africa'='#3F4FFF',
                    'South Asia'='#36BEE3',
                    'East Asia & Pacific'='#FF2F2F')
    
    
    sel <- UsAndThem$Year==1962
    p <- with(UsAndThem,
              l_plot(x=Fertility[sel], y=LifeExpectancy[sel],
                     parent=tt,
                     color=unlist(colorkey)[match(Geographic.Region[sel],
                                                  names(colorkey))],
                     size=scale01(Population[sel])*200,
                     title="Us and Them",
                     xlabel="Fertility", ylabel="Life Expectancy",
                     itemLabel=as.character(Country[sel]),
                     glyph='ccircle',
                     linkingKey=as.character(Country[sel])))
    
    updateYear <- function() {
        year <- as.numeric(tclvalue(SliderValue))
        sel <- UsAndThem$Year==year
        if (sum(sel) > 0) {
            with(UsAndThem,
                 l_configure(p,
                             x=Fertility[sel], y=LifeExpectancy[sel],
                             color=unlist(colorkey)[match(Geographic.Region[sel],
                                                          names(colorkey))],
                             size=scale01(Population[sel])*200,
                             itemLabel=as.character(Country[sel]),
                             glyph='ccircle',
                             linkingKey=as.character(Country[sel]),
                             sync='pull'))
            l_configure(tl, text=year)
            tcl("update", "idletasks")
        }
    }
    
    
    tl <- l_layer_text(p, x=2, y=30, text="1962", size=60, index="end", color="gray70")
    slider <- tkscale(tt, from=min(UsAndThem$Year),to=max(UsAndThem$Year),resolution=1, showvalue=TRUE, orient="horizontal" )
    
    SliderValue <- tclVar("1962")
    tkconfigure(slider, variable=SliderValue, command=function(...)updateYear())

    tkpack(slider, fill="x", side='bottom')
    tkpack(p, side='bottom', fill="both", expand=1)
    
})
