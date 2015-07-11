# https://github.com/rstudio/shiny-examples/tree/master/050-kmeans-example
library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)

tidyCars = mtcars %>% mutate(am=as.factor(am),cyl=as.factor(cyl),vs=as.factor(vs),gear=as.factor(gear),carb=as.factor(carb))
row.names(tidyCars) = row.names(mtcars)

shinyServer(function(input, output, session) {
    # Combine the selected variables into a new data frame
    getStatsData <- reactive({
        x = tidyCars[,input$xcol]
        y = tidyCars[,input$ycol]
        
        df = data.frame(var = character(0), val = character(0), stringsAsFactors = F)
        df[nrow(df) + 1,] = c("x",toString(x))
        df[nrow(df) + 1,] = c("y",toString(y))
        
        # compute sums
        n = length(x)
        df[nrow(df) + 1,] = c("n",toString(n))
        
        sumx = sum(x)
        df[nrow(df) + 1,] = c("sum(x)",toString(sumx))
        
        sumy = sum(y)
        df[nrow(df) + 1,] = c("sum(y)",toString(sumy))
        
        x2 = (x^2)
        df[nrow(df) + 1,] = c("x2",toString(x2))
        
        y2 = (y^2)
        df[nrow(df) + 1,] = c("y^2",toString(y2))
        
        sumx2 = sum(x2)
        df[nrow(df) + 1,] = c("sum(x2)",toString(sumx2))
        
        sumy2 = sum(y2)
        df[nrow(df) + 1,] = c("sum(y2)",toString(sumy2))
        
        xy = (x*y)
        df[nrow(df) + 1,] = c("x*y",toString(xy))
        
        sumxy = sum(xy)
        df[nrow(df) + 1,] = c("sum(xy)",toString(sumxy))
        
        # compute model
        meanx = mean(x)
        df[nrow(df) + 1,] = c("mean(x)",toString(meanx))
        
        meany = mean(y)
        df[nrow(df) + 1,] = c("mean(y)",toString(meany))
        
        sdx = sd(x)
        df[nrow(df) + 1,] = c("sd(x)",toString(sdx))
        
        sdy = sd(y)
        df[nrow(df) + 1,] = c("sd(y)",toString(sdy))
        
        ssxy = sumxy - (sumx*sumy)/n
        df[nrow(df) + 1,] = c("ssxy = sumxy - (sumx*sumy)/n",toString(ssxy))
        
        ssx = sumx2 - sumx^2/n
        df[nrow(df) + 1,] = c("ssx = sumx2 - sumx^2/n",toString(ssx))
        
        ssy = sumy2 - sumy^2/n
        df[nrow(df) + 1,] = c("ssy = sumy2 - sumy^2/n",toString(ssy))
        
        # slope is b1 and intercept is b0
        b1 = ssxy/ssx
        df[nrow(df) + 1,] = c("b1 = ssxy/ssx",toString(b1))
        
        b0 = meany - (b1 * meanx)
        df[nrow(df) + 1,] = c("b0 = meany - (b1 * meanx)",toString(b0))
        
        mse = mean((y - b0 * x) ^2)
        df[nrow(df) + 1,] = c("mse = mean((y - b0 * x) ^2)",toString(mse))
        
        lse = sum((y - (b0 + b1*x))^2)
        df[nrow(df) + 1,] = c("lse = sum((y - (b0 + b1*x))^2)",toString(lse))
        
        # significance of regression model, r is the normalized, r is same as corXY
        r = ssxy / sqrt(ssx * ssy)
        df[nrow(df) + 1,] = c("r = ssxy / sqrt(ssx * ssy)",toString(r))
        # linear determination
        r2 = r^2
        df[nrow(df) + 1,] = c("r2 = r^2",toString(r2))
        
        tStat = (r-0)/ sqrt((1-r2)/(n-2))
        df[nrow(df) + 1,] = c("t statistic=(r-0)/ sqrt((1-r2)/(n-2))",toString(tStat))
        
        sampleXActual = tidyCars[1,xcol]
        sampleYActual = tidyCars[1,ycol]
        sampleYEstimated = b0 - (b1 * sampleXActual)
        df[nrow(df) + 1,] = c("actual x row 1",toString(sampleXActual))
        df[nrow(df) + 1,] = c("actual y row 1",toString(sampleYActual))
        df[nrow(df) + 1,] = c("estimated y=b0 - (b1 * actual x)",toString(sampleYEstimated))
        df
    })
    
    getXYData <- reactive({
        x = tidyCars[,input$xcol]
        y = tidyCars[,input$ycol]
        # fit = lm(y~x)
        data.frame(x,y)
    })
    
    getFit <- reactive({
        xyData = getXYData()
        fit = lm(xyData$y~xyData$x)
    })
    
    output$plot1 <- renderPlot({
#         par(mar = c(5.1, 4.1, 0, 1))
#         plot(selectedData(),
#              col = clusters()$cluster,
#              pch = 20, cex = 3)
#         points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
        xyData = getXYData()
        statsData = getStatsData()
        
        b0 = as.numeric(statsData[20,2])
        b1 = as.numeric(statsData[19,2])
        
        xLabel = input$xcol
        yLabel = input$ycol
        titleLabel = paste(yLabel," as a function of ", xLabel)
        
        g = ggplot(xyData,aes(y = y,x = x))
        g = g  + scale_size(range = c(2, 5), guide = "none" )
        g = g + geom_abline(intercept = b0, slope = b1, colour = "red")
        g = g + geom_smooth(method="lm", formula=y~x)
        g = g + geom_point()
        g = g + labs(list(x=xLabel,y=yLabel, title=titleLabel))
        g = g + theme_bw()
        print(g)
    })
    
    output$summary <- renderPrint({
        fit <- getFit()
        summary(fit)
    })
    
    output$view <- renderTable({
        getStatsData()
    })
})
