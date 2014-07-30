library(shiny)
library(Biostrings)

# I am constructing a table of random numbers, mimicking an MA plot
##7000 fake genes
##10 fake experiments

data <- data.frame(matrix(data = 0, nrow = 7000, ncol = 10))
colnames(data) <- paste("experiment", seq(1,10, by=1), sep="_")
rownames(data) <- paste("gene", seq(1,7000,  by=1), sep="_")
for(i in 1:length(data)){
    data[,i] <- unlist(lapply(1:70, function(i) rnorm(100, mean=2*i*5, sd= 6-0.02*i)))
}
##randomly make outliers
for(i in 1:length(data)){
    mysample <- sample(rownames(data)[1000:4000], 20)
    data[rownames(data) %in% mysample,i] <- data[rownames(data) %in% mysample,i]*rnorm(20, mean=1.3, sd=1)
}

shinyServer(function(input, output) {

    ##I need to be able to get coordinates after a click is made
    location <- reactiveValues(x=NULL, y=NULL, label=NULL)
    observe({
        coords <- input$coords
        isolate({
            location$x <- c(location$x,coords$x)
            location$y <- c(location$y,coords$y)
            location$label <- c(location$label,getLabel(coords))
        })
    })

    ##I need a function that gets the closest point to the click coordinates, within a cutoff
    getLabel <- function(coords){
        if(!is.null(coords$x)){
            xvals <- data[,colnames(data)==input$x.axis]
            yvals <- data[,colnames(data)==input$y.axis]
            x.axis <- log2(xvals+yvals)/2
            y.axis <- log2(yvals/xvals)
            dist <- sqrt((coords$x - x.axis)^2 + (coords$y - y.axis)^2)
            x.range <- dist(range(x.axis[is.finite(x.axis)]))
            y.range <- dist(range(y.axis[is.finite(y.axis)]))
            plot.range <- max(x.range,y.range)
            cutoff <- plot.range*0.05
            closest <- which.min(dist)
            out <- ifelse(dist[closest] < cutoff, as.character(rownames(data)[closest]), "")
            return(out)
        }
        else{return(NULL)}
    }
    
    ##if I click clear, I want to remove the labels
    observe({
        if (input$clear > 0){
            location$x <- NULL
            location$y <- NULL
            location$label <- NULL
        }
    })
    
    ##after a click is made, I want to store the information into a data frame
    makeTable <- function(location){
        if (is.null(location$x)){
            data.frame(x=NULL,y=NULL, label=NULL)
        } else {
            my.coords <- data.frame(x=location$x,y=location$y, label=location$label)
            my.coords <- my.coords[!duplicated(my.coords$label),]
            my.coords
        }
        
    }
    
    ##graphical presentation of the data and labels
    output$mygraph <- renderPlot({
        table <- makeTable(location)
        par(mar=c(5,5,3,2))
        xvals <- data[,colnames(data)==input$x.axis]
        yvals <- data[,colnames(data)==input$y.axis]
        xlab <- input$x.axis
        ylab <- input$y.axis
        plot(log2(xvals+yvals)/2,log2(yvals/xvals), pch=19, cex=2, cex.axis=2,cex.lab=2, cex.main=2,
             main="test plot", col="black", xlab=paste("log2((",xlab, "+", ylab, ")/2)", sep=""), ylab=paste("log2(",ylab,"/",xlab,")", sep=""))
        if(!is.null(table$x)){
            text(table$x, table$y, label=as.character(table$label), cex=1.5)
        }
    }, width=1200, height=800)
})
                                 
        


