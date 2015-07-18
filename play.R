library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)

tidyCars = mtcars %>% add_rownames()
# tidyCars = tidyCars %>% mutate(am=as.factor(am),cyl=as.factor(cyl),vs=as.factor(vs),gear=as.factor(gear),carb=as.factor(carb))
tidyCars = tidyCars %>% select(rowname,mpg,disp,hp,drat,wt,qsec)

xcol = "wt"
ycol = "qsec"

tidyCars = eval(substitute(arrange(tidyCars, var),list(var = as.name(xcol))))

x = tidyCars[,xcol]
y = tidyCars[,ycol]

# xyDf = data.frame(x,y) %>% arrange(x)
# x = xyDf$x
# y = xyDf$y

fit = lm(y~x)

df = data.frame(var = character(0), val = character(0), stringsAsFactors = F)
df[nrow(df) + 1,] = c("x",toString(x))
df[nrow(df) + 1,] = c("y",toString(y))

# compute sums
df[nrow(df) + 1,] = c("Computed values","=========")
n = length(x)
df[nrow(df) + 1,] = c("n",toString(n))

xsum = sum(x)
df[nrow(df) + 1,] = c("xsum=sum(x)",toString(xsum))

ysum = sum(y)
df[nrow(df) + 1,] = c("ysum=sum(y)",toString(ysum))

x2 = (x^2)
df[nrow(df) + 1,] = c("x2=x^2",toString(x2))

y2 = (y^2)
df[nrow(df) + 1,] = c("y2=y^2",toString(y2))

xsum2 = sum(x2)
df[nrow(df) + 1,] = c("xsum2=sum(x2)",toString(xsum2))

ysum2 = sum(y2)
df[nrow(df) + 1,] = c("ysum2=m2=sum(y2)",toString(ysum2))

xy = (x*y)
df[nrow(df) + 1,] = c("xy=x*y",toString(xy))

xysum = sum(xy)
df[nrow(df) + 1,] = c("xysum=sum(xy)",toString(xysum))

# compute model
xmean = xsum/n
df[nrow(df) + 1,] = c("xmean=xsum/n",toString(xmean))

ymean = ysum/n
df[nrow(df) + 1,] = c("ymean=ysum/n",toString(ymean))

ssxy = xysum - (xsum*ysum)/n
df[nrow(df) + 1,] = c("ssxy=xysum - (xsum*ysum)/n",toString(ssxy))

ssx = xsum2 - xsum^2/n
df[nrow(df) + 1,] = c("ssx=xsum2 - xsum^2/n",toString(ssx))

ssy = ysum2 - ysum^2/n
df[nrow(df) + 1,] = c("ssy=ysum2 - ysum^2/n",toString(ssy))

# slope is b1 and intercept is b0
b1 = ssxy/ssx
df[nrow(df) + 1,] = c("b1=ssxy/ssx",toString(b1))

b0 = ymean - (b1 * xmean)
df[nrow(df) + 1,] = c("b0=ymean - (b1 * xmean)",toString(b0))

# statistics
df[nrow(df) + 1,] = c("Basic Statistics","==========")

xvar = sum((x - mean(x))^2)/(n-1)
df[nrow(df) + 1,] = c("var(x)=sum((x - mean(x))^2)/(n-1)",toString(xvar))

yvar = sum((y - mean(y))^2)/(n-1)
df[nrow(df) + 1,] = c("var(y)=sum((y - mean(y))^2)/(n-1)",toString(yvar))

xsd = sqrt(xvar) # sd(x)
df[nrow(df) + 1,] = c("sd(x)=sqrt(xvar)",toString(xsd))

ysd = sqrt(yvar) # sd(y)
df[nrow(df) + 1,] = c("sd(y)=sqrt(yvar)",toString(ysd))

xmse = mean((x - xmean)^2)
df[nrow(df) + 1,] = c("xmse=mean((x - meanx)^2)",toString(xmse))

ymse = mean((y - ymean)^2)
df[nrow(df) + 1,] = c("ymse=mean((y - meany)^2)",toString(ymse))

lse = sum((y - (b0 + b1*x))^2)
df[nrow(df) + 1,] = c("lse=sum((y - (b0 + b1*x))^2)",toString(lse))

# significance of regression model, r is the normalized, r is same as corXY
r = ssxy / sqrt(ssx * ssy)
df[nrow(df) + 1,] = c("r=ssxy / sqrt(ssx * ssy)",toString(r))
# linear determination
r2 = r^2
df[nrow(df) + 1,] = c("r2=r^2",toString(r2))

tStat = (r-0)/ sqrt((1-r2)/(n-2))
df[nrow(df) + 1,] = c("t statistic=(r-0)/ sqrt((1-r2)/(n-2))",toString(tStat))

df[nrow(df) + 1,] = c("Actual Values from first x and y","==========")
sampleXActual = tidyCars[1,xcol]
sampleYActual = tidyCars[1,ycol]
sampleYEstimated = b0 - (b1 * sampleXActual)
df[nrow(df) + 1,] = c("actual x row 1",toString(sampleXActual))
df[nrow(df) + 1,] = c("actual y row 1",toString(sampleYActual))
df[nrow(df) + 1,] = c("estimated y=b0 - (b1 * actual x)",toString(sampleYEstimated))

xLabel = xcol
yLabel = ycol
titleLabel = paste(ycol," as a function of ", xcol)

g = eval(substitute(ggplot(tidyCars, aes(y=var1,x=var2)),list(var1 = as.name(ycol),var2 = as.name(xcol))))
g = g + geom_abline(intercept = b0, slope = b1, colour = "red")
g = g + geom_smooth(method="lm", formula=y~x)
g = g + geom_point()
g = g + labs(list(x=xLabel,y=yLabel, title=titleLabel))
g = g + theme_bw()
g
