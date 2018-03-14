# this is for the Nibbs Creek example



Daily1 <- readNWISdv('02041000',"00060","",Sys.Date())%>%
  filter(X_00060_00003_cd != 'P')%>%select(Date,X_00060_00003)
names(Daily1)[2] <- paste('02041000',"Mean Daily Flow (cfs)",sep=" ")

inputFile <- read.csv('data/NibbsCreek.csv')
inputFile$Date <- as.Date(as.character(inputFile$Date))

gage1 <- Daily1
gageDataCombined <- merge(gage1,inputFile,by='Date')

combine1 <- merge(inputFile,gage1,by.x="Date",by.y="Date")
corresult <- data.frame(Gage=strsplit(names(gage1)[2]," ")[[1]][1],
                        Correlation=cor(combine1[,2],combine1[,3]),
                        n=nrow(combine1))
dat <- combine1
names(dat)[c(2,3)] <- c('StreamName',"GageName")
# this is the old linear model method, I think i just used this to demo the app but 
# what i need is a nls model
ggplotRegression1(lm(StreamName ~ GageName,data=dat))+
  scale_x_log10(breaks=seq(0,100,10),minor_breaks=seq(0,100,10))+
  scale_y_log10(breaks=seq(0,100,10),minor_breaks=seq(0,100,10))

fit <- lm(StreamName ~ GageName,data=dat)

ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))

# NONLINEAR model of same data
fit <- nls(StreamName ~  a*GageName^b,dat,start= list(a=1,b=1))
coefficients(fit)[2][[1]]

ggplot(dat,aes(x = GageName, y = StreamName)) + 
  geom_point() + coord_trans(y = 'log',x = 'log')+#scale_x_log10() + scale_y_log10()
  geom_smooth(method = 'nls',formula = 'y ~  a * x ^ b',
              method.args=list(start=c(a=1,b=1)),se=F) +# predict.nls you can't use se for preditions and needs to be turned off for stat_smooth call
 labs(x=paste('Gage: ','02041000'),
      y=paste('User Data:','Nibbs'))




# now use formula to correct gage statistics for user site
flowstats <- subset(gagestats,SITEID %in% as.character('02041000')) %>%
    dplyr::select(SITEID:HARMEAN) # bc raster package is loaded
gageStats <- subset(gageInfo@data,GageNo %in% as.character('02041000'))%>%
    mutate(SITEID=GageNo)%>%select(-c(name2,GageNo))
  
extraStats <- merge(gageStats,flowstats,by='SITEID')

stats <- subset(extraStats,SITEID %in% as.character('02041000'))%>%
  select(X1Q10:HARMEAN)
stats <- as.data.frame(t(stats))
stats <- cbind(stats,rownames(stats))
names(stats) <- c('Value','Stat')
stats <- mutate(stats,adj=coefficients(fit)[1][[1]]*(Value^coefficients(fit)[2][[1]]))%>%
  select(2,3,1)
names(stats) <- c('Flow Statistic','something',paste('02041000',' Value',sep=''))


# Now try with Interactive metrics graphics example
library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)

mtcars %>%
  mjs_plot(x=wt, y=mpg, width=600, height=500) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")

mtcars %>%
  mjs_plot(x=wt, y=mpg, width=600, height=500) %>%
  mjs_point(least_squares=TRUE) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")

dat %>%
  mjs_plot(x=GageName, y=StreamName)%>%
  mjs_point(least_squares=TRUE)

ggplot(dat,aes(x = GageName, y = StreamName)) + 
  geom_point() + coord_trans(y = 'log',x = 'log')+#scale_x_log10() + scale_y_log10()
  geom_smooth(method = 'nls',formula = 'y ~  a * x ^ b',
              method.args=list(start=c(a=1,b=1)),se=F) +# predict.nls you can't use se for preditions and needs to be turned off for stat_smooth call
  labs(x=paste('Gage: ','02041000'),
       y=paste('User Data:','Nibbs'))

# doesnt seem to be a solution for now:
#  - no log abilities
#  - no nls functionality
#  - no way to pull regression from plot

# neat attempt though!

# Try with (older) plotly package
library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  plotlyOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(iris, aes(Sepal.Width, Petal.Width)) + 
      geom_line() + 
      geom_point()
  })
}

shinyApp(ui = ui, server = server)

p <- plot_ly(dat, x = ~GageName, y = ~StreamName) %>% add_markers()
layout(p, xaxis = list(type = "log"),
       yaxis = list(type = "log"))


p <- ggplot(dat,aes(x = GageName, y = StreamName)) + geom_point() +coord_trans(y = 'log',x = 'log')+
  stat_smooth(method = 'lm', aes(colour = 'Linear Model'), se = FALSE) +
  geom_smooth(method = 'nls',formula = "y ~  a * x ^ b", aes(colour='Power Model'), 
              se = FALSE,  method.args=list(start=c(a=1,b=1)))
              
ggplotly(p)

  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  

ggplot(dat,aes(x = GageName, y = StreamName)) + 
  geom_point() + coord_trans(y = 'log',x = 'log')+#scale_x_log10() + scale_y_log10()
  geom_smooth(method = 'nls',formula = 'y ~  a * x ^ b',
              method.args=list(start=c(a=1,b=1)),se=F) +# predict.nls you can't use se for preditions and needs to be turned off for stat_smooth call
  labs(x=paste('Gage: ','02041000'),
       y=paste('User Data:','Nibbs'))

