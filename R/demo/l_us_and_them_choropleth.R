


if (requireNamespace("rworldmap", quietly = TRUE) &&
    requireNamespace("RColorBrewer", quietly = TRUE)) {
    
    
    local({
        #' Link Points With Map and Vice Versa
        
        scale01 <- function(x) {(x-min(x))/(max(x)-min(x))}
        
        ## Color Gradient to Map Life Expectancy on Map
        colfn <- colorRamp(RColorBrewer::brewer.pal(9, name="YlOrBr"), alpha=FALSE)
        
        data(UsAndThem)
        colorkey = list('America'='#E5FF2F',
                        'Europe & Central Asia'='#FF982F',
                        'Middle East & North Africa'='#68FF5E',
                        'Sub-Saharan Africa'='#3F4FFF',
                        'South Asia'='#36BEE3',
                        'East Asia & Pacific'='#FF2F2F')
        
        
        dat <- subset(UsAndThem, Year == 2002)
        dat$Region.Color <- with(dat, unlist(colorkey[match(Geographic.Region,names(colorkey))]))

        LifeExpectancyColor <- sapply(scale01(dat$LifeExpectancy), function(val) {
            col <- colfn(val)
            rgb(col[1], col[2], col[3], maxColorValue=255)
        } )
        
        
        ## plot of life expectancy versus fertility
        p <- with(dat, l_plot(LifeExpectancy ~ Fertility, color=Region.Color,
                              size=scale01(Population)*120+2,
                              itemLabel=as.character(dat$Country),
                              showItemLabels=TRUE,
                              background='gray20',
                              foreground='gray95',
                              guidesBackground = 'gray20',
                              guidelines = 'gray30',
                              showGuides=TRUE,
                              glyph='ccircle',
                              showScales=TRUE,
                              linkingKey=as.character(Country)))
        
        ## Choropleth Map
        world <- rworldmap::getMap(resolution = "coarse")
        p_map <- l_plot() # background='#71ABDB'
        
        l_map <- l_layer(p_map, world, asSingleLayer=TRUE, index="end", label="world map")
        l_scaleto_world(p_map)
        
        map_countries <- tolower(attr(l_map, "NAME"))
        data_countries <- tolower(as.character(dat$Country))
        
        fromto <- list(c("antigua and barb.","antigua and barbuda"),
                       c("bosnia and herz.", "bosnia and herzegovina"),
                       c("congo (kinshasa)", "congo, dem. rep."),
                       c("congo (brazzaville)", "congo, rep."),
                       c("ivory coast", "cote d'ivoire"),
                       c("eq. guinea", "equatorial guinea"),
                       c("fr. polynesia", "french polynesia"),
                       c("guinea bissau", "guinea-bissau"),
                       c("hong kong", "hong kong, china"),
                       c("n. korea","korea, rep."),
                       c("s. korea", "korea, dem. rep."),
                       c("macau", "macao, china"),
                       c("macedonia", "macedonia, fyr"),
                       c("micronesia", "micronesia, fed. sts."),
                       c("st. vin. and gren.", "saint vincent and the grenadines"),
                       c("slovakia", "slovak republic"),
                       c("solomon is.", "solomon islands"),
                       c("s. sudan", "south sudan"),
                       c("east timor","timor-leste"),
                       c("gaza", "west bank and gaza"),
                       c("west bank", "west bank and gaza"),
                       c("yemen", "yemen, rep."))
        
        for(l in fromto) {
            map_countries[map_countries == l[1]] <- l[2]
        }
        
        map_Country2Wmap <- lapply(data_countries, function(x) {
            which(x == map_countries)
        })
        names(map_Country2Wmap) <- data_countries
        
        
        # grep("slovak", data_countries, value = TRUE)
        # grep("yemen", map_countries, value = TRUE)
        # names(Filter(function(x)length(x)==0, map_Country2Wmap))
        ## Not Found             
        # "guadeloupe"
        # "martinique"
        # "netherlands antilles"
        # "reunion"
        
        
        #    printTags <- function(W) {
        #        print(l_currenttags(W))
        #    }
        #    l_bind_item(p_map, 'all', '<ButtonPress>', printTags)
        #    l_map['tag'] <- attr(l_map, "NAME")
        
        map_Wmap2Country <- match(map_countries, data_countries)
        
        
        LifeExpectancyMapColor <- rep("grey80", l_map['n'])
        invisible(Map(function(i, col){
            if(length(i) > 0) {
                LifeExpectancyMapColor[i] <<- col
            }
        }, map_Country2Wmap, LifeExpectancyColor))
        
        l_map['color'] <- LifeExpectancyMapColor
        
        updateMap_sp <- function() {
            i <- unlist(map_Country2Wmap[p['selected']])
            cols <- LifeExpectancyMapColor
            if(!is.null(i)) {
                cols[Filter(function(x)!is.na(x),i)] <- 'magenta'
            }
            l_map['color'] <- cols
        }
        
        b <- l_bind_state(p, 'selected', function(){updateMap_sp()})
        
        
        updatePlot_sp <- function(add) {
            i <- l_currentindex(p_map)
            if(i !=-1 && !is.na(map_Wmap2Country[i])) {
                if (add) {
                    l_configure(p, selected=TRUE, which=map_Wmap2Country[i])
                } else {
                    sel <- rep(FALSE, p['n'])
                    sel[map_Wmap2Country[i]] <- TRUE
                    p['selected'] <- sel
                }
            }
        }
        
        b2 <- l_bind_item(p_map, paste0('layer&&',l_map), '<ButtonPress-1>',
                          function(){updatePlot_sp(FALSE)})
        b3 <- l_bind_item(p_map, paste0('layer&&',l_map), '<Shift-ButtonPress-1>',
                          function(){updatePlot_sp(TRUE)})
    })
}