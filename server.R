# See here for how most of this works: http://shiny.rstudio.com/gallery/
# Note to mathjax to work dynamically for the table I had to hack shiny,
#   such as taking the outut of renderTable(df) and hacking it in here.

library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
# library(Cairo, warn.conflicts = F)
library(xtable)
library(htmltools)

tidyCars = mtcars %>% add_rownames()
tidyCars = tidyCars %>% mutate(am=as.factor(am),cyl=as.factor(cyl),vs=as.factor(vs),gear=as.factor(gear),carb=as.factor(carb))
# used below by formulas section
classNames <- "data table table-bordered table-condensed"

shinyServer(function(input, output, session) {
    output$help <- renderUI({
        # mathjax not really needed here
        ht = paste("Select the inputs below to change what is displayed in the plot and the Formulas and Values Table.",
                   "That table develops the slope and intecept by evaluating each formula in turn.",
                   "They b0 should match the intercept in the fit summary output, and the b1 value should match the x value.",
                   "Also additional helpful statistics are generated, including the t statistic which should also match the x t value in the summary.")
        withMathJax(helpText(ht))
    })
    
    # return sorted xy data based on user input
    getCarData <- reactive({
        predictor = input$predictor
        response = input$response
        tc = eval(substitute(arrange(tidyCars, var),list(var = as.name(predictor))))
        tc
    })
    
    # Combine the selected variables into a new data frame
    getStatsData <- reactive({
        carData = getCarData()
        predictor = input$predictor
        response = input$response
        x = carData[,predictor]
        y = carData[,response]
        
        df = data.frame(formula = character(0), value = character(0), stringsAsFactors = F)
        form = "\\(x\\)"
        df[nrow(df) + 1,] = c(form,toString(round(x,2)))
        form = "\\(y\\)"
        df[nrow(df) + 1,] = c(form,toString(round(y,2)))
        
        # compute sums
        df[nrow(df) + 1,] = c("Computed values","=========")
        n = length(x)
        form = "$$n$$"
        df[nrow(df) + 1,] = c(form,toString(round(n,2)))
        
        xsum = sum(x)
        form = "$$xsum=sum(x)$$"
        df[nrow(df) + 1,] = c(form,toString(round(xsum,2)))
        
        ysum = sum(y)
        form = "$$ysum=sum(y)$$"
        df[nrow(df) + 1,] = c(form,toString(round(ysum,2)))
        
        x2 = (x^2)
        form = "$$x2=x^2$$"
        df[nrow(df) + 1,] = c(form,toString(round(x2,2)))
        
        y2 = (y^2)
        form = "$$y2=y^2$$"
        df[nrow(df) + 1,] = c(form,toString(round(y2,2)))
        
        x2sum = sum(x2)
        form="$$x2sum = sum(x2)$$"
        df[nrow(df) + 1,] = c(form,toString(round(x2sum,2)))
        
        y2sum = sum(y2)
        form="$$y2sum = sum(y2)$$"
        df[nrow(df) + 1,] = c(form,toString(round(y2sum,2)))
        
        xy = (x*y)
        form="$$xy = (x*y)$$"
        df[nrow(df) + 1,] = c(form,toString(round(xy,2)))
        
        xysum = sum(xy)
        form="$$xysum = sum(xy)$$"
        df[nrow(df) + 1,] = c(form,toString(round(xysum,2)))
        
        # compute model
        xmean = xsum/n
        form="$$xmean = xsum/n$$"
        df[nrow(df) + 1,] = c(form,toString(round(xmean,2)))
        
        ymean = ysum/n
        form="$$ymean = ysum/n$$"
        df[nrow(df) + 1,] = c(form,toString(round(ymean,2)))
        
        ssxy = xysum - (xsum*ysum)/n
        form="$$ssxy = xysum - (xsum*ysum)/n$$"
        df[nrow(df) + 1,] = c(form,toString(round(ssxy,2)))
        
        ssx = x2sum - xsum^2/n
        form="$$ssx = x2sum - xsum^2/n$$"
        df[nrow(df) + 1,] = c(form,toString(round(ssx,2)))
        
        ssy = y2sum - ysum^2/n
        form="$$ssy = y2sum - ysum^2/n$$"
        df[nrow(df) + 1,] = c(form,toString(round(ssy,2)))
        
        # slope is b1 and intercept is b0
        b1 = ssxy/ssx
        form="$$b1=ssxy/ssx$$"
        df[nrow(df) + 1,] = c(form,toString(round(b1,4)))
        
        b0 = ymean - (b1 * xmean)
        form = "$$b0=ymean - (b1 * xmean)$$"
        df[nrow(df) + 1,] = c(form,toString(round(b0,4)))
        
        # statistics
        df[nrow(df) + 1,] = c("Basic Statistics","==========")
        
        xvar = sum((x - xmean)^2)/(n-1)
        form = "$$xvar=sum((x - xmean)^2)/(n-1)$$"
        df[nrow(df) + 1,] = c(form,toString(round(xvar,2)))
        
        yvar = sum((y - ymean)^2)/(n-1)
        form = "$$yvar=sum((y - ymean)^2)/(n-1)$$"
        df[nrow(df) + 1,] = c(form,toString(round(yvar,2)))
        
        xsd = sqrt(xvar) # sd(x)
        form = "$$xsd = sqrt(xvar)$$"
        df[nrow(df) + 1,] = c(form,toString(round(xsd,2)))
        
        ysd = sqrt(yvar) # sd(y)
        form = "$$ysd = sqrt(yvar)$$"
        df[nrow(df) + 1,] = c(form,toString(round(ysd,2)))
        
        xmse = mean((x - xmean)^2)
        form = "$$xmse = mean((x - xmean)^2)$$"
        df[nrow(df) + 1,] = c(form,toString(round(xmse,2)))
        
        ymse = mean((y - ymean)^2)
        form = "$$ymse = mean((y - ymean)^2)$$"
        df[nrow(df) + 1,] = c(form,toString(round(ymse,2)))
        
        lse = sum((y - (b0 + b1*x))^2)
        form = "$$lse = sum((y - (b0 + b1*x))^2)$$"
        df[nrow(df) + 1,] = c(form,toString(round(lse,2)))
        
        # significance of regression model, r is the normalized, r is same as corXY
        r = ssxy / sqrt(ssx * ssy)
        form = "$$r = ssxy / sqrt(ssx * ssy)$$"
        df[nrow(df) + 1,] = c(form,toString(round(r,2)))
        # linear determination
        r2 = r^2
        form = "$$r2 = r^2$$"
        df[nrow(df) + 1,] = c(form,toString(round(r2,2)))
        
        tStat = (r-0)/ sqrt((1-r2)/(n-2))
        form = "$$tStat = (r-0)/ sqrt((1-r2)/(n-2))$$"
        df[nrow(df) + 1,] = c(form,toString(round(tStat,3)))
        
        df[nrow(df) + 1,] = c("Actual Values from first x and y","==========")
        sampleXActual = carData[1,predictor]
        sampleYActual = carData[1,response]
        sampleYEstimated = b0 - (b1 * sampleXActual)
        df[nrow(df) + 1,] = c("actual x row 1",toString(round(sampleXActual,2)))
        df[nrow(df) + 1,] = c("actual y row 1",toString(round(sampleYActual,2)))
        df[nrow(df) + 1,] = c("estimated y=b0 - (b1 * actual x)",toString(round(sampleYEstimated,2)))
        df
    })
    
    getFit <- reactive({
        carData = getCarData()
        predictor = input$predictor
        response = input$response
        x = carData[,predictor]
        y = carData[,response]
        
        fit = lm(y~x)
    })
    
    output$plot <- renderPlot({
        carData = getCarData()
        predictor = input$predictor
        response = input$response
        fitMethod = input$fitMethod

        statsData = getStatsData()
        
        b0 = as.numeric(statsData[19,2])
        b1 = as.numeric(statsData[18,2])
        
        xLabel = predictor
        yLabel = response
#         titleLabel = paste("The Red line is based on B0 and B1 as derived in the formulas.\n",
#                         "The Blue line is the line as plotted by the fit method.\n",
#                         "Drag the mouse over some points to find out which cars the are!")
        
        g = eval(substitute(ggplot(carData, aes(y=var1,x=var2)),list(var1 = as.name(response),var2 = as.name(predictor))))
        gfm = eval(substitute(geom_smooth(method=var1, formula=y~x),list(var1 = as.name(fitMethod))))

        g = g  + scale_size(range = c(2, 5), guide = "none" )
        g = g + geom_abline(intercept = b0, slope = b1, colour = "red")
        g = g + gfm
        g = g + geom_point()
#         g = g + labs(list(x=xLabel,y=yLabel, title=titleLabel))
        g = g + labs(list(x=xLabel,y=yLabel))
        g = g + theme_bw()
        g
    })
    
    output$summary <- renderPrint({
        fit <- getFit()
        summary(fit)
    })
    
#     output$click_point <- renderPrint({
#         carData = getCarData()
#         nearPoints(carData, input$plot_click, addDist = TRUE)
#     })
#     
    output$brush_points <- renderPrint({
        carData = getCarData()
        brushedPoints(carData, input$plot_brush)
    })
    
    output$formulas <- renderUI({
        df = getStatsData()
        # totally hacked from renderTable(), took the paste(...) stuff and passed it to HTML and passed it to withMathJax!!
        withMathJax(
            HTML(
                paste(
                    capture.output(
                        print(xtable(df), type = "html",html.table.attributes = paste("class=\"",htmlEscape(classNames, TRUE), "\"", sep = ""))
                    ),
                collapse = "\n"
                )
            )
        )
    })
})
