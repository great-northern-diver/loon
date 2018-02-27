
## Gapminder Data
local({
    data(UsAndThem)
    
    colorkey = list('America'='#E5FF2F',
                    'Europe & Central Asia'='#FF982F',
                    'Middle East & North Africa'='#68FF5E',
                    'Sub-Saharan Africa'='#3F4FFF',
                    'South Asia'='#36BEE3',
                    'East Asia & Pacific'='#FF2F2F')
    
    scale01 <- function(x) {(x-min(x))/diff(range(x))}
    
    sel <- UsAndThem$Year == 2009
    sum(sel)
    
    LifeExpectancy <- UsAndThem$LifeExpectancy[sel]
    Fertility <- UsAndThem$Fertility[sel]
    Income <- UsAndThem$Income[sel]
    
    region_cols <- unlist(colorkey)[match(UsAndThem$Geographic.Region[sel], names(colorkey))]
    pop_size <- scale01(UsAndThem$Population[sel])*200
    country <- as.character(UsAndThem$Country[sel])
    
    p1 <- l_plot(y=LifeExpectancy, x=Fertility,
                 color=region_cols, size=pop_size,
                 itemLabel=country,
                 linkingGroup="world")
    
    p2 <- l_plot(y=LifeExpectancy, x=Income,
                 itemLabel=country,
                 linkingGroup="world")
    
    h <- l_hist(x=Income/1000, linkingGroup="world")
    
    
    ## Create Time series
    head(UsAndThem)
    
    # split data by country and arrange them by year
    sdat <- split(UsAndThem, UsAndThem$Country)
    sdat2 <- lapply(sdat, function(dat) {
        dat[order(dat$Year), ]
    })
    
    # How many observations per year per country
    vapply(sdat2, nrow, numeric(1))
    
    # Extract data with 52 observations
    sdat3 <- Filter(function(x)nrow(x)==52, sdat2)
    
    # Check if the years are all the same
    tmp.year <- sdat3[[1]]$Year
    any(vapply(sdat3, function(x)any(x$Year!=tmp.year), logical(1)))
    
    # Now create a data frame with years in columns and countries in row
    # for LifeExpectancy
    dat4 <- vapply(sdat3, function(x)x$Income, numeric(52))
    rownames(dat4) <- tmp.year
    
    dat4[,c("Kuwait", "United States")]
    
    dat5 <- as.data.frame(t(dat4))
    
    head(dat5)
    is.data.frame(dat5)
    
    
    sa <- l_serialaxes(data=dat5, linkingGroup="world",
                       scaling="data", axesLayout="parallel",
                       title='Income', itemLabel=rownames(dat5))
    
    sa['linewidth'] <- 2
    
    rownames(dat5)[sa['selected']]
    
    ## Can I keep the optimized linkingKeys?
    ## any(country != rownames(dat5))
    ## no> use new linkingKeys
    l_configure(p1, linkingKey=country, sync="push")
    l_configure(p2, linkingKey=country, sync="push")
    l_configure(h, linkingKey=country, sync="push")
    l_configure(sa, linkingKey=rownames(dat5), sync="pull")
    
})
