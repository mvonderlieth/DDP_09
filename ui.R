shinyUI(pageWithSidebar(
    headerPanel('Stats for MTCARS'),
    sidebarPanel(
        selectInput('ycol', 'Y Variable', c("qsec","mpg"),selected="qsec"),
        selectInput('xcol', 'X Variable', c("disp","wt","drat"),selected="disp"),
        tableOutput("view")
    ),
    mainPanel(
        plotOutput('plot1')
    )
))
