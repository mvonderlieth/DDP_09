
shinyUI(
    fluidPage(
        # page title and main title
        title = "Statistics for a Linear Model and MTCARS",
        titlePanel("Statistics for a Linear Model and MTCARS"),
        # need withMathJax here and at least on uiOutput() to get mathjax to show in formula table!?
        withMathJax(),

        sidebarLayout(
            sidebarPanel(
                uiOutput('help')
            ),
            
            mainPanel(
                helpText("The Red line is based on b0 and b1 as derived in the formulas. The Blue line is the line as plotted by the fit method."),
                plotOutput('plot',click = "plot_click",brush = brushOpts(id = "plot_brush"))
            )
        ),
        
        hr(),
        
        fluidRow(
            column(3,
                   h4("Choose the model Predictor, Response and Fit Method")
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
            column(7,
                   h5("Formulas and Values"),
                   helpText("All numbers rounded to 2 places, except b0, b1 and t-stat so they match the fit summary values."),
                   uiOutput("formulas")
            ),
            column(5,
                   h5("Selected Cars"),
                   helpText("Click and drag in plot to see which points go with which cars"),
                   # verbatimTextOutput('click_point'),
                   verbatimTextOutput('brush_points'),
                   h5("Summary of lm(y~x)"),
                   helpText("Not showing summary of loess fit as I don't calculate those values, it's just an option so you can see the red line better!"),
                   verbatimTextOutput('summary')
            )
        )
    )
)
