# See here for how most of this works: http://shiny.rstudio.com/gallery/
# Note to mathjax to work dynamically for the table I had to hack shiny,
#   such as taking the outut of renderTable(df) and hacking it in here.

library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(xtable)
library(htmltools)

tidyCars = mtcars %>% add_rownames()
tidyCars = tidyCars %>% select(rowname,mpg,disp,hp,drat,wt,qsec)
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
        
        # store formula labels and values in df for table output
        df = data.frame(Formula = character(0), Rcode = character(0), Values = character(0), stringsAsFactors = F)

        # make room now for b0 and b1, will store them here later
        df[nrow(df) + 1,] = c("","$$Compare\\;b0\\;and\\;b1\\;to\\;Summary\\;Coefficients\\;Estimates$$","")
        b0Row = b0Row = nrow(df) + 1
        df[b0Row,] = c("","","")

        b1Row = b1Row = nrow(df) + 1
        df[b1Row,] = c("","","")

        # first x,y, and n
        df[nrow(df) + 1,] = c("","$$b0\\;and\\;b1\\;Decomposed$$","")
        form = "$$x$$"
        df[nrow(df) + 1,] = c(form,"x",toString(round(x,2)))
        form = "$$y$$"
        df[nrow(df) + 1,] = c(form,"y",toString(round(y,2)))

        n = length(x)
        form = "$$n$$"
        df[nrow(df) + 1,] = c(form,"n",toString(round(n,2)))

        # compute sums
        xsum = sum(x)
        form = "$$xsum=\\sum(x)$$"
        df[nrow(df) + 1,] = c(form,"xsum = sum(x)",toString(round(xsum,2)))
        
        ysum = sum(y)
        form = "$$ysum=\\sum(y)$$"
        df[nrow(df) + 1,] = c(form,"ysum = sum(y)",toString(round(ysum,2)))
        
        x2 = x^2
        form = "$$x2=x^2$$"
        df[nrow(df) + 1,] = c(form,"x2 = x^2",toString(round(x2,2)))
        
        y2 = y^2
        form = "$$y2=y^2$$"
        df[nrow(df) + 1,] = c(form,"y2 = y^2",toString(round(y2,2)))
        
        x2sum = sum(x2)
        form="$$x2sum = \\sum(x2)$$"
        df[nrow(df) + 1,] = c(form,"x2sum = sum(x2)",toString(round(x2sum,2)))
        
        y2sum = sum(y2)
        form="$$y2sum = \\sum(y2)$$"
        df[nrow(df) + 1,] = c(form,"y2sum = sum(y2)",toString(round(y2sum,2)))
        
        xy = x*y
        form="$$xy = x*y$$"
        df[nrow(df) + 1,] = c(form,"xy = x*y",toString(round(xy,2)))
        
        xysum = sum(xy)
        form="$$xysum = \\sum(xy)$$"
        df[nrow(df) + 1,] = c(form,"xysum = sum(xy)",toString(round(xysum,2)))
        
        # compute model
        xmean = xsum/n
        form="$$xmean = \\frac{xsum}{n}$$"
        df[nrow(df) + 1,] = c(form,"xmean = xsum/n",toString(round(xmean,2)))
        
        ymean = ysum/n
        form="$$ymean = \\frac{ysum}{n}$$"
        df[nrow(df) + 1,] = c(form,"ymean = ysum/n",toString(round(ymean,2)))
        
        ssx = x2sum - xsum^2/n
        form="$$ssx = x2sum - \\frac{xsum^2}{n}$$"
        df[nrow(df) + 1,] = c(form,"ssx = x2sum - xsum^2/n",toString(round(ssx,2)))
        
        ssxy = xysum - (xsum*ysum)/n
        form="$$ssxy = xysum - \\frac{(xsum*ysum)}{n}$$"
        df[nrow(df) + 1,] = c(form,"ssxy = xysum - (xsum*ysum)/n",toString(round(ssxy,2)))
        
        # slope is b1 and intercept is b0
        b1 = ssxy/ssx
        form="$$b1 = \\frac{ssxy}{ssx}$$"
        df[b1Row,] = c(form,"b1 = ssxy/ssx",toString(round(b1,4)))
        
        b0 = ymean - (b1 * xmean)
        form = "$$b0 = ymean - (b1 * xmean)$$"
        df[b0Row,] = c(form,"b0 = ymean - (b1 * xmean)",toString(round(b0,4)))
        
        # statistics
        df[nrow(df) + 1,] = c("","$$Basic\\;Statistics$$","")
        ssy = y2sum - ysum^2/n
        form="$$ssy = y2sum - \\frac{ysum^2}{n}$$"
        df[nrow(df) + 1,] = c(form,"ssy = y2sum - ysum^2/n",toString(round(ssy,2)))
        
        xvar = sum((x - xmean)^2)/(n-1)
        form = "$$xvar = \\frac{\\sum(x - xmean)^2}{(n-1)}$$"
        df[nrow(df) + 1,] = c(form,"xvar = sum((x - xmean)^2)/(n-1)",toString(round(xvar,4)))
        
        yvar = sum((y - ymean)^2)/(n-1)
        form = "$$yvar = \\frac{\\sum(y - ymean)^2}{(n-1)}$$"
        df[nrow(df) + 1,] = c(form,"yvar = sum((y - ymean)^2)/(n-1)",toString(round(yvar,4)))
        
        xsd = sqrt(xvar) # sd(x)
        form = "$$xsd = \\sqrt{xvar}$$"
        df[nrow(df) + 1,] = c(form,"xsd = sqrt(xvar)",toString(round(xsd,4)))
        
        ysd = sqrt(yvar) # sd(y)
        form = "$$ysd = \\sqrt{yvar}$$"
        df[nrow(df) + 1,] = c(form,"ysd = sqrt(yvar)",toString(round(ysd,4)))
        
        xmse = mean((x - xmean)^2)
        form = "$$xmse = mean((x - xmean)^2)$$"
        df[nrow(df) + 1,] = c(form,"xmse = mean((x - xmean)^2)",toString(round(xmse,4)))
        
        ymse = mean((y - ymean)^2)
        form = "$$ymse = mean((y - ymean)^2)$$"
        df[nrow(df) + 1,] = c(form,"xmse = mean((x - xmean)^2)",toString(round(ymse,4)))
        
        lse = sum((y - (b0 + b1*x))^2)
        form = "$$lse = \\sum((y - (b0 + b1*x))^2)$$"
        df[nrow(df) + 1,] = c(form,"lse = sum((y - (b0 + b1*x))^2)",toString(round(lse,4)))
        
        # significance of regression model, r is normalized, r is same as corXY
        r = ssxy / sqrt(ssx * ssy)
        form = "$$r = \\frac{ssxy}{\\sqrt{ssx * ssy}}$$"
        df[nrow(df) + 1,] = c(form,"r = ssxy / sqrt(ssx * ssy)",toString(round(r,4)))
        # linear determination
        r2 = r^2
        form = "$$r2 = r^2$$"
        df[nrow(df) + 1,] = c(form,"r2 = r^2",toString(round(r2,4)))
        
        tStat = (r-0)/sqrt((1-r2)/(n-2))
        form = "$$tStat = \\frac{(r-0)}{\\sqrt{\\frac{1-r^2}{n-2}}}$$"
        df[nrow(df) + 1,] = c(form,"tStat = (r-0)/sqrt((1-r2)/(n-2))",toString(round(tStat,3)))
        
        # as per fit summary, Coefficient of determination
        fit = getFit()
        multR2 = 1 - sum(fit$residuals^2)/sum((y - ymean)^2)
        form = "$$multR2 = 1 - \\frac{fit$residuals^2}{\\sum(y-ymean)}$$"
        df[nrow(df) + 1,] = c(form,"multR2 = 1 - sum(fit$residuals^2)/sum((y - ymean)^2)",toString(round(multR2,5)))
        
        df[nrow(df) + 1,] = c("","$$Actual\\;Values\\;from\\;first\\;x\\;and\\;y$$","")
        sampleXActual = carData[1,predictor]
        sampleYActual = carData[1,response]
        sampleYEstimated = b0 + (b1 * sampleXActual)
        estimatedFormula = sprintf("%.3f - (%.3f * %.3f)",b0,b1,sampleXActual)
        df[nrow(df) + 1,] = c(predictor,"carData[1,predictor]",toString(round(sampleXActual,2)))
        df[nrow(df) + 1,] = c(response,"carData[1,response]",toString(round(sampleYActual,2)))
        df[nrow(df) + 1,] = c("estimated y",estimatedFormula,toString(round(sampleYEstimated,2)))
        list(df,b0,b1)
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

        statsDataList = getStatsData()
        statsData = data.frame(statsDataList[1])
        b0 = unlist(statsDataList[2])
        b1 = unlist(statsDataList[3])
        
        xLabel = predictor
        yLabel = response

        g = eval(substitute(ggplot(carData, aes(y=var1,x=var2)),list(var1 = as.name(response),var2 = as.name(predictor))))
        gfm = eval(substitute(geom_smooth(method=var1, formula=y~x),list(var1 = as.name(fitMethod))))

        g = g  + scale_size(range = c(2, 5), guide = "none" )
        g = g + geom_abline(intercept = b0, slope = b1, colour = "red")
        g = g + gfm
        g = g + geom_point()
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
        statsDataList = getStatsData()
        df = data.frame(statsDataList[1])
        df = df[1:32,]
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
    
    output$actuals <- renderTable({
        statsDataList = getStatsData()
        df = data.frame(statsDataList[1])
        actualDf = df[34:36,]
#         predictor = input$predictor
#         response = input$response
#         actualDf[1,1] = predictor
#         actualDf[2,1] = response
        
        actualDf
    })
    
})
