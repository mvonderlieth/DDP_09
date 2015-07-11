shinyUI(pageWithSidebar(
    headerPanel('Stats for MTCARS'),
    sidebarPanel(
        selectInput('ycol', 'Y Variable', c("qsec","mpg"),selected="qsec"),
        selectInput('xcol', 'X Variable', c("wt","disp","hp","drat"),selected="wt"),
        tableOutput("view")
    ),
    mainPanel(
        plotOutput('plot1'),
        verbatimTextOutput("summary")
    )
))
