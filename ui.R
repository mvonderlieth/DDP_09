
shinyUI(
    fluidPage(
        # page title and main title
        title = "Statistics for a Linear Model and MTCARS",
        titlePanel("Statistics for a Linear Model and MTCARS"),

        sidebarLayout(
            sidebarPanel(
                helpText("Create demographic maps with information from the 2010 US Census.")
            ),
            
            mainPanel(
                plotOutput('plot')
            )
        ),
        
        hr(),
        
        fluidRow(
            column(3,
                   h4("Choose model predictor and response")
            ),
            column(3,
                   selectInput('predictor', 'Predictor', c("wt","disp","hp","drat"),selected="wt")
            ),
            column(3,
                   selectInput('response', 'Response', c("qsec","mpg"),selected="qsec")
            )
        ),
        
        hr(),
        
        fluidRow(
            column(7,
                   h5("Formulas and Values"),
                   tableOutput("view")
            ),
            column(5,
                   h5("Summary"),
                   verbatimTextOutput('summary')
            )
        )
    )
)
