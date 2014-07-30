library(shiny)

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


shinyUI(pageWithSidebar(
    headerPanel('interactive plot'),
    sidebarPanel(
        helpText('An MA plot is a representation of change in the data.
    The x-axis is the log2 average between test and control samples.
    The y-axis is the log2 ratio between test and control.
    (i.e. x=log2((control+test)/2)  and y=log2(test/control))
    The MA plot is good way to represent differences between two samples.
    Up-regulated genes are higher on the y-axis and generally more significant when farther down the x-axis.'),
        selectInput("x.axis", label = h3("Select control"), 
                    choices = colnames(data),
                    , selected = "experiment_1"),
        selectInput("y.axis", label = h3("Select test"), 
                    choices = colnames(data),
                    , selected = "experiment_2"),
        actionButton("clear", "Clear Labels")
        ),
    mainPanel(
        fluidRow(
            h3("MAplot: click near a value to get label"),
            plotOutput("mygraph", clickId="coords")
            )
        )
    ))

