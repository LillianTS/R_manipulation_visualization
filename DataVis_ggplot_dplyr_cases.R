# Need to use intall.packages and library in order to have access to its functions
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("dplyr")
data(Births78)
Births <- mutate(Births78,
                 date=ymd(date), # ymd=year-month-date
                 wd=wday(date), # show as a number
                 wday=wday(date, label=TRUE, abbr=TRUE) # label will show Monday etc
                 )                                      # abbr will show MON
BirthRow <- Births78
head(Births, 2)
qplot(substance, data=HELPrct) # qplot: only need to provide variable names
# categorical variable: Bar Chart
qplot(i2, data=HELPrct)
# numerical variable: Histogram
qplot(date, births, data=Births)
qplot(sex, substance, data=HELPrct)
# geom_point(), geom_line(), geom_density(x & color), stat_density(), geom_boxplot()
# geom_jitter(), coord_flip()
ggplot(data=Births, aes(x=date, y=births, color=wday))+geom_point()
ggplot(data=Births, aes(x=date, y=births, color=wday))+geom_line()
ggplot(data=HELPrct, aes(x=age, color=substance))+geom_density()
ggplot(data=HELPrct, aes(x=age, fill=substance))+stat_density(alpha=.5)
ggplot(data=HELPrct, aes(x=age, fill=substance))+stat_density(geom='density', alpha=.5)#density chart
ggplot(data=HELPrct, aes(x=age, color=substance))+stat_density(geom='line')
ggplot(data=HELPrct, aes(x=sex, y=age))+geom_boxplot(outlier.size=0)+geom_jitter(alpha=.6)+coord_flip()
# swap axes; coord_flip() ; outlier.size=0: show no outlier
# geom_jitter: bring dots
# geom_boxplot()+geom_jitter()
head(EuStockMarkets)
# melt command is for performing pivot, it changes the format of data by pivoting
#multiple columns into one column
#install.packages('reshape')
library(reshape) # for melt command
ds=as.data.frame(EuStockMarkets) 
ds$day=seq(nrow(ds)) 
ds <- ds %>% mutate(num=seq(nrow(ds)))
head(ds,6)
EuStock=melt(ds, id=c('day')) # id parameter here tells what fields to keep
names(EuStock)[2]='Index' 
head(EuStock)
ggplot(data=EuStock, aes(x=day, y=value, color=Index))+geom_line()
ggplot(data=EuStock, aes(x=day, y=value, color=Index))+geom_line(size=3)
ggplot(data=EuStock, aes(x=day, y=value, color=Index))+geom_line(size=1.5, alpha=.5)

library(dplyr)
head(mtcars) # first 6 rows
summary(mtcars) # summary of a dataset: min, Q1, median, mean, Q3, max
mc=as.data.frame(mtcars) # copy the data mtcars to a dataframe
names(mc) # check the column names of dataset mc
names(mc)[1]
names(mc)[1]="Miles per Gallon"
names(mc)
mc$cyl #$ get all the values under column cyl 
mc$cyl < 5 # return boolean values
mc$carb

mycars <- mtcars[mtcars$cyl==4 & mtcars$mpg>18, c("cyl", "mpg", "hp", "wt")] 
#c:specify what columns you want
head(mycars)

mycars$mpgPerWeight=mycars$mpg/mycars$wt #use $ to create new column
head(mycars,11)

#tidyverse(filter, select, mutate) needs to specify data!!!
mycars <- filter(mtcars, cyl==4 & mpg > 18) # filter what conditions you want
mycars <- select(mycars, cyl, mpg, hp, wt) # specify what columns you want 
mycars <- mutate(mycars, mpgPerWeight=mpg/wt) # create new column
head(mycars)
## %>% means "goes into"
x %>% f(a,b) # == f(x,a,b)
x %>% f(a,b) %>% g(c,d) #==g(f(x,a,b), c,d)
mycars <- mtcars%>%
  filter(cyl==4 & mpg > 18) %>%
  select(cyl, mpg, hp, wt) %>%
  mutate(mpgPerWeight=mpg/wt) #mutate funnction: create, alter, delete columns
head(mycars)

# use "cut" function to make variables be discrete
mycars <- mycars %>% 
  mutate(hpfactor=cut(hp, breaks=c(-Inf, 120, 200, Inf), labels=c("low", "medium", "high"))) %>%
  head() 

install.packages("hflights")
library(hflights) 
data(hflights) #create data hflights
flights = hflights

# Group_by, summarise always come together
flights%>%
  group_by(Dest) %>% # summarise always come with group_by
  summarise(avg_delay=mean(ArrDelay, na.rm=TRUE)) 
#na.rm=TRUE: ignore NA value very important!!
flights%>%
  group_by(Dest)%>%
  select(Dest, Cancelled)%>%
  table()%>% # create matrix
  head()
# Pivoting!!! pivot_longer
EuStock=as.data.frame(EuStockMarkets)
head(EuStockMarkets)
EuStock <- EuStock%>%
  mutate(day=1:n())%>%
  pivot_longer(-day, names_to="Index", values_to="Value")
head(EuStock)

# Basic Graph Types
# Line graph
# use tidyverse function 'filter'
# use filter to select which column you want
ggplot(filter(EuStock, Index=="DAX"), aes(day,Value)) + geom_line()
p <- ggplot(data=EuStock, aes(x=day, y=Value))
p+geom_line()
p+geom_line(aes(color=Index)) 
# facet_wrap(~) 
p+geom_line()+facet_wrap(~Index)
# Categorical
mycars <- mtcars%>%
  mutate(gear=factor(gear, levels=c(3,4,5), labels=c("3gear", "4gear", "5gear")))%>%
  mutate(cyl=factor(cyl, levels=c(4,6,8), labels=c("4cyl", "6cyl", "8cyl")))
head(mycars)
# Bar Chart
ggplot(diamonds, aes(x=clarity))+geom_bar()
# 只有在y軸沒有指定的情況下，才可以用geom_bar()
# stacked bar chart
# fill: means sub-categorical variable
ggplot(data=diamonds, aes(x=clarity, fill=cut))+geom_bar()
p <- ggplot(diamonds, aes(x=clarity, fill=cut))
p + geom_bar(position="stack") # stack is default
# side-by-side bar(dodge)
p + geom_bar(position="dodge") 
# percentage stacks
p + geom_bar(position="fill")
# over plotted bars useless
p + geom_bar(position="identity")
# 如果y軸有指定，則需要用geom_col()
ggplot(diamonds, aes(x=clarity, y=price))+geom_col()
p <- ggplot(diamonds, aes(x=clarity, y=price))
p + stat_summary(fun="sum", geom="bar") # fun is used for y-axis variable
p + stat_summary(fun="median", geom="bar")
# Order fct_reorder 從高到低
ggplot(diamonds, aes(x=fct_reorder(clarity, desc(price)), y=price)) +
  stat_summary(fun="median", geom="bar")

# Nested function calls: pipes
diamonds%>%
  mutate(clarity=fct_reorder(clarity, desc(price)))%>%
  ggplot(aes(x=clarity, y=price))+
  stat_summary(fun="median", geom="bar")

# Contigency plot(heatmap) geom_tile is used for heatmap
# 2 categorical variables and 1 numerical variable
ggplot(mtcars, aes(x=cyl, y=gear, fill=wt))+geom_tile()
ggplot(mtcars, aes(x=factor(cyl), y=factor(gear)))+geom_bin2d()
install.packages("treemapify")
library(treemapify)
install.packages("treemap")
library(treemap)

data("GNI2014") 
head(GNI2014)
ggplot(GNI2014, aes(area=GNI, fill=population, label=country, subgroup=continent))+
  geom_treemap()+
  geom_treemap_subgroup_border()+
  geom_treemap_subgroup_text(place="center", grow=TRUE, alpha=.9, color="White", 
                             fontface="italic", min.size=0)+
  geom_treemap_text(color="red", place="topleft", reflow=T)


ggplot(mtcars, aes(x = factor(cyl), y = factor(gear))) +
  geom_bin2d(aes(fill = ..count..)) +  
  scale_fill_gradient(low = "white", high = "blue") +  
  labs(title = "Count of Cars by Cylinders and Gears",
       x = "Number of Cylinders",
       y = "Number of Gears") +
  theme_minimal()
