install.packages("gcookbook")
library(gcookbook)
hw_plot <- ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
hw_plot+labs(title="Age and Height of Schoolchildren")
# subtitle(ggtitle)
hw_plot+ggtitle("Age and Height of Schoolchildren", "11.5 to 17.5 years old")
# scale_x_contionuos is to make changes to x-axis
hw_plot+scale_x_continuous(name="Age in year")
# fill is to color bar
pg_plot <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+
stat_summary(fun="mean", geom="bar")
pg_plot
pg_plot+labs(x="Age in Years", y = "Height in Inches",
             title = "Weight per Group",
             fill = "Condition") # also assign name to fill
pg_plot + ylim(0, 10) # 等於 scale_y_continuos(limits=c(0,10))
pg_plot + scale_y_continuous(limits = c(0, 10))
pg_plot+expand_limits(y=0) # 確保y軸的最小值為0
# x軸只要ctrl 和 trt1 只要兩個bar chart
pg_plot + scale_x_discrete(limits=c("ctrl", "trt1"))
# 等於 xlim("ctrl", "trt1")

# Date Axes
library(scales)
data(economics)
head(economics)
# as.Date: means convert string to Date Format
econ_mod <- economics%>%
  filter(date >=as.Date("1992-05-01") & date < as.Date("1993-06-01"))
# by: breaks
datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by="2 month")
econ_plot <- ggplot(econ_mod, aes(x = date, y = psavert)) + geom_line()
econ_plot
# labels=date_format("%Y %b") %b means month
econ_plot+scale_x_date(breaks=datebreaks, labels=date_format("%Y %b"))+
  theme(axis.text.x=element_text(angle=30, hjust=1)) # hjust=1: align to right

# Themes  axis.tex(title).x = element_text()
hw_plot+theme_bw() # equals to theme_minimal()
hw_plot + theme_minimal()
hw_plot + theme_classic() # no grid lines
hw_plot + theme_grey(base_size = 16)
hw_plot+theme(axis.title.x=element_text(size=16, lineheight=.9, 
                  face="bold.italic", color="red"))
# \n: means goes to next line
hw_plot+ggtitle("Age and Height\nof Schoolchildren")+
  theme(plot.title=element_text(size=rel(1.5), # according to text
                lineheight=.9, face="bold.italic", color="red"))
# Grid: panel.grid.major
# panel.background. panel.border
hw_plot+theme(
  panel.grid.major=element_line(color="red"),
  panel.grid.minor=element_line(color="red", linetype="dashed", size=0.2),
  panel.background=element_rect(fill="lightblue"),
  panel.border=element_rect(color="blue", fill=NA, linewidth=2)
)
# Legend
# legend.background, legend.title, legend.text, legend.key
pg_plot + theme(
    legend.background = element_rect(fill = "grey85", colour = "red", size = 1), 
    legend.title = element_text(colour = "blue", face = "bold", size = 14), 
    legend.text = element_text(colour = "red"),
    legend.key = element_rect(colour = "blue", size = 0.25)
  )
# Title, Axis: axis.title.x; axis.text.x; plot.title
hw_plot+ggtitle("Plot title here")+
  theme(axis.title.x=element_text(color="red", size=14),
        axis.text.x=element_text(color="blue"),
        axis.title.y=element_text(color="red", size=14),
        axis.text.y=element_text(color="blue"),
        plot.title=element_text(color="red", face="bold", size=20))
# facet_grid(rows ~ columns) use strip
hw_plot + facet_grid(sex ~ .) + 
  theme(strip.background = element_rect(fill = "pink"),
        strip.text.y = element_text(size = 14, angle = -90, face = "bold") )
# remove x axis and y axis
# element_blank() very important!!!!!
hw_plot + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank() )
hw_plot + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank() )

# breaks
hw_plot + scale_x_continuous(breaks = c(12, 14, 15, 16, 18))
hw_plot + scale_x_continuous(breaks=seq(12, 18, by=.5))
pg_plot + scale_x_discrete(labels=c("Control", "T1", "T2"),
                   position="top") # x軸的標籤名稱
# Removes legend plot+guides(fill=FALSE)
pg_plot + guides(fill = FALSE) # removes legend
pg_plot + scale_fill_discrete(guide = FALSE) # remove legend
# Legend position
pg_plot + theme(legend.position = "top")
# Change the order of items
pg_plot + scale_fill_discrete(limits = c("trt1", "trt2", "ctrl"))

# hsv: hue, saturation, value
ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(size = 3, color=hsv(0.8,0.8,0.7))
# gradient is for continuos data 
hwplot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, color=weightLb)) +
  geom_point(size = 3)
hwplot
hwplot + scale_color_gradient()
hwplot + scale_color_distiller(palette="BuPu")
hwplot + scale_color_viridis_c() # c for continuos
# Custom gradient color
hwplot + scale_color_gradient(low="black", high=hsv(0.25,0.75,0.9))
# three colors
hwplot + scale_colour_gradient2(
  low = "red",
  mid = "white", # middle color value
  high = "blue",
  midpoint = 100.5, # middle data value
  space= "Lab",
  na.value = "grey50" # null-value'color=grey
)
library(scales)
# muted(color)
hwplot + scale_colour_gradient2(
  low = muted("red"),  # muted: makes less intense versions of color
  mid = "white", # middle color value
  high = muted("blue"),
  midpoint = 100.5, # middle data value
  space = "Lab",
  na.value = "grey50" 
)
# For discrete data: scale_fill
uspopage_plot <- ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup))+
  geom_area() # for time series data
uspopage_plot
uspopage_plot + scale_fill_viridis_d()
uspopage_plot + scale_fill_brewer(palette = "Pastel1")
uspopage_plot + scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_dark() # backgound is black

# color=factor(cyl) is very important 
p <- ggplot(mtcars, aes(mpg, wt, color=factor(cyl)))+geom_point(size=3)
p
# add factor is very important to have 3 different colors
# p is color vector
pal <- c("8"="red", "6"="darkgreen", "4"="blue")
# scale_color_manual(values=顏色向量)
p+scale_color_manual(values=pal)
p+theme(legend.title=element_text(size=16))

# Create map style 
statesmap=map_data("state")
head(statesmap)
# Map group=group: geom_polygon() 
# must have group
ggplot(statesmap, aes(long, lat, group=group)) + 
  geom_polygon(colour='black', fill=NA)

install.packages("crimedata")
library(crimedata)

crimes <- get_crime_data(years = c(2018), type = "sample", output = "tbl")
head(crimes)
# -c: means removing columns you don't want
crimes <- crimes %>%
  select(-c("uid","offense_code","census_block","date_start","date_end"))
head(crimes)
ggplot()+geom_point(data=crimes,
                    aes(longitude,latitude, color=offense_against),
                    size=3)

# combine two different plots together
ggplot() +
  geom_polygon(data=statesmap,
               aes(x=long, y=lat, group=group),
               colour='black',
               fill=NA) +
  geom_point(data=crimes,
             aes(longitude,latitude, color=offense_against),
             size=3)
# rownames_to_column: create new column for 第一行
# name is called rowname
data(USArrests)
crimes <- USArrests %>%
  rownames_to_column() %>%
  mutate(state = tolower(rowname)) %>%  # rowname
  select(-c(rowname)) %>% # delete rowname column
  data.frame() # to create a dataset
head(crimes)
states_map <- map_data("state")
# connect two dataset: left_join: by=c()
crime_map <- left_join(states_map, crimes, by=c("region"="state"))
head(crime_map)
# coord_map("polyconic)
ggplot(crime_map, 
       aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic")
# Cartogram
ggplot(crimes, aes(map_id = state, fill = Assault)) +
  geom_map(map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")

# Dense Timeseries Heatmaps
crashes<- read.csv("/Users/yiching/Downloads/chicago_crashes.csv")
ggplot(crashes, aes(x=CRASH_DAY_OF_WEEK, y=CRASH_HOUR,
                    fill=INJURIES_TOTAL), na.rm=TRUE) + geom_tile()
ggplot(crashes, aes(INJURIES_TOTAL)) + geom_histogram(bins=10) 
# no need to provide y 
summary(crashes$INJURIES_TOTAL) # summary of a column 
# stat_summary_2d: has additional z axis
# use geom_tile()
ggplot(crashes, aes(x=factor(CRASH_DAY_OF_WEEK), y=factor(CRASH_HOUR)),
       na.rm=TRUE) +
  stat_summary_2d(fun="mean", aes(z=INJURIES_TOTAL), geom="tile")

# Adjusting Inflation Rates
moneyOverTime = tibble(y=c(2015,2016,2017,2018), x=c(100,100,100,100))
head(moneyOverTime)
library(readr)  
CPITable <- read.csv("/Users/yiching/Downloads/CPITable.csv")
# Function:
# cpiLook is the function name and y is the parameter name
# use return 
cpiLookup <- function(y) { 
  return(CPITable %>% 
           filter(Year %in% y) %>%
           select(Value) %>% 
           .$Value ) } # .means retrieving the dataframe's column 
# Value's vector
cpiLookup(2010)
moneyOverTime <- moneyOverTime %>% 
  mutate(adjustedX = x * cpiLookup(max(.$y))/cpiLookup(y))
head(moneyOverTime)

# Dealing with dates
priceIndex <- read_csv("/Users/yiching/Downloads/ConsumerPriceIndex.csv")
cIndex <- priceIndex %>%
  select(-c("Avg.", "Dec...13", "Dec...15")) %>%
  pivot_longer(-c(Year, Avg), names_to="Month", values_to="value")
head(cIndex)
# paste to concatenate string
cIndexDateStr <- cIndex %>%
  mutate(date_string = paste(Year, Month, "01", sep="-"))
head(cIndexDateStr)
# unit
altCIdx <- cIndex %>%
  unite("date_string", c(Year, Month), sep="-") # 剩3行
head(altCIdx)
# as_date: convert to date format "%Y-%b-%d"
cIndex <- cIndexDateStr %>%
  mutate(Date = as_date(date_string, "%Y-%b-%d"))
head(cIndex)
timeplot <- ggplot(cIndex, aes(Date, value)) + 
  geom_line() +
  theme_bw()
timeplot
cIdxWD <- cIndex %>%
  mutate(weekday = wday(Date))
head(cIdxWD)

cIdxWD %>% 
  filter(Date > ymd(20060601), Date < ymd("2008-12-31")) %>% 
  ggplot(aes(x=year(Date) %>% factor(),  # use the year part of date
             y=month(Date) %>% factor()), # use the month part of date
         na.rm=TRUE) +
  stat_summary_2d(fun="median", aes(z=value), geom="tile")
cIdxWD %>% # variable(data)
  filter(month(Date) <= 9, month(Date) >= 3) %>% # March through September
  filter(year(Date) >= 2006, year(Date) <= 2008) %>%  # Just a few years
  ggplot(aes(x=month(Date) %>% factor(),
             y=value),
         na.rm=TRUE) +
  facet_wrap(~year(Date)) +
  geom_col()
timeplot + coord_polar()