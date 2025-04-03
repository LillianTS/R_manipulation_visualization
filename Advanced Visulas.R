# factor():day, month, region
library(datasets)
aq <- airquality%>%
  drop_na() # removes rows with missing values
aqplot <- aq%>%
  ggplot(aes(x=Wind))
aqplot+geom_histogram() # no need to provide y-axis variable
aqplot+geom_histogram(bins=10) # how many columns you want
aqplot + geom_histogram(binwidth=2)
aqplot+geom_density() # know the distribution of data
aq%>%
  filter(Month==6 | Month==7)%>% # | means or
  ggplot(aes(Wind, fill=factor(Month)))+
           geom_density(alpha=.5)
aq%>%
  ggplot(aes(Wind, fill=factor(Month)))+
  geom_density(alpha=.5)
aqplot <- aq%>%
  ggplot(aes(x=factor(Month), y=Wind))
aqplot+geom_point()
aqplot+geom_jitter()
aqplot+geom_boxplot()+geom_jitter()
aqplot+geom_violin() # violin plot
aqplot+geom_sina() # dots scattered inside the shape of violin
aqplot+geom_violin()+geom_sina()
aqplot + geom_beeswarm() # beeswarm plot
# highOz will be either True or False
aq %>% mutate(highOz = Ozone > mean(Ozone)) %>%
  ggplot(aes(x=factor(Month), y=Wind, color=highOz)) +
  geom_beeswarm()
# Fine Detail Comparison
# Q-Q plot: inside aes() use sample=
ggplot(aq, aes(sample=Temp))+geom_qq() # use sample
ggplot(aq, aes(sample=Temp))+geom_qq()+geom_qq_line()
# %$% data.frame()
# sort() 
aq.qq <- aq %$% 
  data.frame(wind=sort(Wind),
             ozone=sort(Ozone),
             solar=sort(Solar.R),
             temp=sort(Temp))
aq.qq %>% ggplot(aes(wind,solar)) + geom_point()
aq.qq <- aq %$%
  data.frame(wind=sort(Wind),
             ozone=sort(Ozone),
             solar=sort(Solar.R),
             temp=rescale(Temp, to=c(0,1)) %>% sort) # %>% sort
aq.qq %>% 
  mutate(wind=(wind-min(wind))/(max(wind)-min(wind))) %>%
  mutate(ozone=rescale(ozone, to=c(0,1))) %>%
  ggplot(aes(wind,ozone)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) # 45 degree line
aq.qq %>% 
  ggplot(aes(x=rescale(wind, to=c(0,1)), y=temp)) + # temp done above
  geom_point() +
  geom_abline(slope=1, intercept=0) # y=x

# Applying Log Transformation
stock <- aapl %>%
  mutate(logPrice = log(adj_price))
stock%>%
  ggplot(aes(date,adj_price))+geom_line()
# becomes jagged
stock%>%
  ggplot(aes(date,logPrice))+geom_line()

# Applying log scales
applePlot <- ggplot(aapl, aes(x = date,y = adj_price)) +
  geom_line()
applePlot+scale_y_log10()
# Get a list of numbers of ten to the power of 0 through 3
10^(0:3) 
# 1 10 100 1000
            
applePlot+
  scale_y_log10(breaks=10^(-2:2), 
                labels=trans_format("log10", math_format(.x)))+
  ylab("Log of Price")
applePlot +
  scale_y_log10(breaks = c(.01,.25,1,20,100,250))

# Controlling Log Bases: trans()
applePlot+
  scale_y_continuous(
  trans = "log2",
  breaks = trans_breaks("log2", function(x) 2^x),
  labels = trans_format("log2", math_format(2^.x)) 
) 
applePlot +
  scale_y_continuous(
    trans = log_trans(),
    breaks = trans_breaks("log", function(x) exp(x)),
    labels = trans_format("log", math_format(e^.x))
  ) 
# Smoothing
# Smoothing with LOESS
applePlot + geom_smooth()
applePlot + geom_smooth(method=lm)
applePlot + geom_ma() # Moving Average
applePlot + 
  geom_ma(ma_fun=SMA, n=100, size=1, color="red") +
  geom_ma(ma_fun=EMA, n=50, size=1, color="blue")