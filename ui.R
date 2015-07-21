# mvonderlieth June 2015

ht1 = "The mtcars is a very popular dataset often used in R.  Here we will use it to deconstruct the formulas that constitute the slope and intercept values of a linear model."
ht2 = paste(
           "First select the inputs below to change what is displayed in the Plot, the Summary Output, and the Formulas Table (below the summary)."
           )
ht3 = paste(
           "Then look at the Summary Output, and then scroll down the Formulas Table below it.",
           "The Formulas Table develops the slope and intecept by evaluating each formula in turn.",
           "The b0 should match the intercept in the fit summary output, and the b1 value should match the x value.",
           "The table also shows additional helpful statistics, including the t value and Multiple R-Squared.")

shinyUI(
    fluidPage(
        # page title and main title
        title = "Statistics for a Linear Model and MTCARS",
        titlePanel("Statistics for a Linear Model and MTCARS"),
        
        sidebarLayout(
            sidebarPanel(
                helpText(ht1),
                helpText(ht2),
                helpText(ht3),
                helpText("author: Mark von der Lieth June 2015"),
                helpText("mtcars help description at bottom of page.")
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
                   helpText("Note: Not showing summary of loess fit as I don't calculate those values, it's just an option so you can see the red line better!"),
                   verbatimTextOutput('summary')
            ),
            column(6,
                   h4("Bonus: Selected Cars"),
                   helpText("Click and drag in plot to see which points go with which cars.  If you select the left most point it should match the values in the table below."),
                   # verbatimTextOutput('click_point'),
                   verbatimTextOutput('brush_points'),
                   tableOutput("actuals")
            )
        ),

        hr(),
        
        fluidRow(
            column(12,
                   h4("Formulas Table"),
                   helpText("Here we develop and deconstruct the model formulas"),
                   uiOutput("formulas")
            ),
            
            hr(),
            
            column(12,
                   h4("mtcars {datasets}	R Documentation"),
                   helpText("Motor Trend Car Road Tests"),
                   helpText(""),
                   helpText("Description"),
                   helpText(""),
                   helpText("The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models)."),
                   helpText(""),
                   helpText("Usage"),
                   helpText(""),
                   helpText("mtcars"),
                   helpText("Format"),
                   helpText(""),
                   helpText("A data frame with 32 observations on 11 variables."),
                   helpText(""),
                   helpText("[, 1]	mpg	Miles/(US) gallon"),
                   helpText("[, 2]	cyl	Number of cylinders"),
                   helpText("[, 3]	disp	Displacement (cu.in.)"),
                   helpText("[, 4]	hp	Gross horsepower"),
                   helpText("[, 5]	drat	Rear axle ratio"),
                   helpText("[, 6]	wt	Weight (lb/1000)"),
                   helpText("[, 7]	qsec	1/4 mile time"),
                   helpText("[, 8]	vs	V/S"),
                   helpText("[, 9]	am	Transmission (0 = automatic, 1 = manual)"),
                   helpText("[,10]	gear	Number of forward gears"),
                   helpText("[,11]	carb	Number of carburetors"),
                   helpText("Source"),
                   helpText(""),
                   helpText("Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411."),
                   helpText(""),
                   helpText("Examples"),
                   helpText(""),
                   helpText("require(graphics)"),
                   helpText("pairs(mtcars, main = \"mtcars data\")"),
                   helpText("coplot(mpg ~ disp | as.factor(cyl), data = mtcars,"),
                   helpText("       panel = panel.smooth, rows = 1")
            )
        )
    )
)
