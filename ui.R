# mvonderlieth June 2015

ht1 = "The mtcars is a very popular dataset often used in R.  Here we will use it to decompose the formulas that constitute the slope and intercept values of a linear model."
ht2 = paste(
           "First select the inputs below to change what is displayed in the Plot, the Summary Output, and the Formulas Table (below the summary)."
           )
ht3 = paste(
           "Then look at the Summary Output, and then scroll down the Formulas Table below it.",
           "The Formulas Table develops the slope and intecept by evaluating each formula in turn.",
           "The b0 should match the intercept in the fit summary output, and the b1 value should match the x value.",
           "The table also generates additional helpful, including the t statistic which should also match the t value for x in the summary.")

shinyUI(
    fluidPage(
        # page title and main title
        title = "Statistics for a Linear Model and MTCARS",
        titlePanel("Statistics for a Linear Model and MTCARS"),
        
        sidebarLayout(
            sidebarPanel(
                helpText(ht1),
                helpText(ht2),
                helpText(ht3)
            ),
            
            mainPanel(
                helpText("The Red line is based on b0 and b1 as derived in the formulas. The Blue line is the line as plotted by the fit method."),
                plotOutput('plot',click = "plot_click",brush = brushOpts(id = "plot_brush"))
            )
        ),
        
        hr(),
        
        fluidRow(
            column(3,
                   h5("Choose the model Predictor, Response and Fit Method")
            ),
            column(3,
                   selectInput('predictor', 'Predictor', c("wt","disp","hp","drat"),selected="wt")
            ),
            column(3,
                   selectInput('response', 'Response', c("qsec","mpg"),selected="qsec")
            ),
            column(3,
                   selectInput('fitMethod', 'Fit Method', c("lm","loess"),selected="lm")
            )
        ),
        
        hr(),

        fluidRow(
            column(6,
                   h4("Summary Output"),
                   helpText("Not showing summary of loess fit as I don't calculate those values, it's just an option so you can see the red line better!"),
                   verbatimTextOutput('summary')
            ),
            column(6,
                   h4("Selected Cars"),
                   helpText("Click and drag in plot to see which points go with which cars"),
                   # verbatimTextOutput('click_point'),
                   verbatimTextOutput('brush_points')            
            )
        ),

        hr(),
        
        fluidRow(
            column(12,
                   h4("Formulas Table"),
                   helpText("Here we develop and decompose the model formulas"),
                   uiOutput("formulas")
            )
        )
    )
)
