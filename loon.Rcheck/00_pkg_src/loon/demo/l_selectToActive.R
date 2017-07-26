
local({
    # Bivariate normal parameterized by principal axis sds and rotation of major axis
    rbvn <- function(n=1, mean=c(0,0), sd = c(1,1), rotation=0 ) {
        data <- matrix(rnorm(n*2),ncol=2)
        rotMat <- matrix(c(cos(rotation), -sin(rotation), 
                           sin(rotation),  cos(rotation)), 
                         ncol=2,byrow=TRUE)
        data[,1] <- data[,1]*sd[1]
        data[,2] <- data[,2]*sd[2]
        data <- data %*% t(rotMat)
        data[,1] <- data[,1]  +  mean[1]
        data[,2] <- data[,2]  +  mean[2]
        data
        
    }
    
    
    
    #
    # Create dependent data for illustration
    n <- 30
    m <- 20
    sd <- c(3,1)
    rotation <- -pi/4
    means <- rbvn(m,sd=c(4,1), rotation = pi/4)
    
    data1 <- matrix(0, nrow=n*m, ncol=2)
    
    for (i in 1:m){
        ##i <- 0
        ##i <- i+1
        from <- (i-1)*n + 1
        to <- i*n
        data1[from:to,] <- rbvn(n,mean=means[i,], sd= sd, rotation=rotation)
    }
    data2 <- matrix(rep(means,each=n),ncol=2,byrow=FALSE) + rbvn(n*m, sd=c(1.1,1.1))
    
    data <- as.data.frame(cbind(data1,data2))
    names(data) <-c("x","y","u","v")
    head(data)
    
    ##
    ## Have a look
    pairs(data[,c("x","y")])      # no relation
    pairs(data[,c("x","u","v")])  # increasing relations
    pairs(data[,c("y","u","v")])  # increasing relations
    pairs(data)
    
    
    #
    # Now with brushing
    #
    pa <- with(data, l_plot(x=x, y=y, main="active here"))
    ps <- with(data, l_plot(x=u, y=v, main = "select here"))
    
    active2selected <- function() {
        ps['selected'] <- pa['active']
    } 
    
    selected2active <- function() {
        pa['active'] <- ps['selected']
    } 
    
    l_bind_state(pa, "active", function()active2selected())
    l_bind_state(ps, "selected", function()selected2active())
    
    ## Brush  on "selected" and watch "active"
    ## long skinny brushes condition on either u (vertical skinny) or v (horizontal skinny)
    ##  ... neither producing that strong a relation in x and y (esp. in the middle)
    ## small square brush conditions on u and v together.
    ##  ... should see (conditionally) negative relation between x and y 
    ##      pretty much everywhere 
    ##
    ##  Now for something more challenging.
    ##
    
    Allx <- l_cget(pa,"x")
    Ally <- l_cget(pa,"y")
    xrange <- range(Allx)
    coef <- lm(Ally ~ Allx)$coef
    yVal <- coef[1] + xrange*coef[2]
    fitAll <- l_layer_line(pa, x=xrange, y=yVal,
                           color="black", linewidth=3, label="fit on all data")
    fitActive <- l_layer_line(pa, x=xrange, y=yVal, color="magenta",
                              linewidth=5, dash=c(10,5), label="fit on active data")
    
    ## redefine selected2active with fit
    selected2active <- function() {
        pa['active'] <- ps['selected']
        x <- l_cget(pa,"x")[l_cget(pa,"active")]
        y <- l_cget(pa,"y")[l_cget(pa,"active")]
        if (length(x) > 1) {
            xrange <- range(x)
            if (diff(xrange)>0){
                coef <- lm(y ~ x)$coef
                yVal <- coef[1] + xrange*coef[2]
                l_configure(fitActive, x=xrange, y=yVal)
            }
        }
    }
})
