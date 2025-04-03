#1a: Load PerceptionExperiment data
data <- as.data.frame(PerceptionExperiment)
# Calculate error
data <- data %>%
  mutate(Error=Response-TrueValue)
# data$Error <- data$Response - data$TrueValue
ggplot(data, aes(x = Test, y = Error, fill = Test)) +
  geom_violin(alpha = 0.7, color = "black") +
  geom_jitter(shape = 16, position = position_jitter(0.1), alpha = 0.3, color = "darkblue") +
  ggtitle("Error Distribution by Test") +
  xlab("Test Type") +
  ylab("Error") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Set3")
#1b
data <- data%>%
  mutate(AbsoluteError=abs(Error))
ggplot(data, aes(x = AbsoluteError, fill = Test)) +
  geom_density(alpha = 0.6) +
  ggtitle("Density of Absolute Errors by Test") +
  xlab("Absolute Error") +
  ylab("Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Spectral")
#1c
display_data <- data%>%
  filter(Display %in% c(1,2), Subject >= 56 & Subject <= 73 )
ggplot(display_data, aes(x = factor(Display), y = Response, fill = factor(Display))) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Comparison of Responses for Display 1 and 2 (Subjects 56-73)") +
  xlab("Display") +
  ylab("Response") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = FALSE)
#1d
anomalous_data <- filter(data, Test == "vertical distance, non-aligned" & Response == 1)
# Plot anomalies
ggplot(anomalous_data, aes(x=Trial, y=Response, color=factor(Subject))) +
  geom_point() +
  ggtitle("Anomalous Responses (Vertical Distance, Non-Aligned)") +
  xlab("Trial") +
  ylab("Response") +
  theme_minimal()

#2a 
ggplot(messier_data, aes(x = Messier, y = Distance.LY., size = Size., color = Kind)) +
  geom_point(alpha = 0.7) +
  ggtitle("Distance of Messier Objects by Messier Number, with Size and Kind") +
  xlab("Messier Number") +
  ylab("Distance (light years)") +
  scale_size_continuous(name = "Angular Size (Size)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")  # Adding a nice color palette for clarity
#2b
# Plot distribution of distances by Kind using boxplots
ggplot(messier_data, aes(x = Kind, y = Distance.LY.)) +
  geom_boxplot() +
  ggtitle("Distribution of Distances by Object Kind") +
  xlab("Object Kind") +
  ylab("Distance (light years)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#2c
ggplot(messier_data, aes(x = Apparent.Magnitude, y = Distance.LY.)) +
  geom_point() +
  ggtitle("Distance vs Apparent Magnitude of Messier Objects") +
  xlab("Apparent Magnitude (Higher = Fainter)") +
  ylab("Distance (light years)") +
  theme_minimal()
#2d
ggplot(messier_data, aes(x = Apparent.Magnitude, y = Distance.LY., size = Size., color = Kind)) +
  geom_point(alpha = 0.7) +  # Use some transparency for overlap clarity
  ggtitle("Distance vs Apparent Magnitude with Angular Size and Object Kind") +
  xlab("Apparent Magnitude (Higher = Fainter)") +
  ylab("Distance (light years)") +
  scale_size_continuous(name = "Angular Size (Size)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")  # Use a color palette to differentiate kinds

#3c
# Logarithmic scale plot for population
ggplot(montana_data, aes(x=Year, y=Population)) +
  geom_line() +
  scale_y_log10() +
  ggtitle("Montana Population Growth on Logarithmic Scale") +
  xlab("Year") +
  ylab("Population (log scale)") +
  theme_minimal()
#3b
# Calculate percentage change
montana_data <- montana_data %>%
  arrange(Year) %>%
  mutate(PercentChange = (Population - lag(Population)) / lag(Population) * 100)
#3a Plot percentage change
ggplot(MessierData, aes(x=year, y=Percentage)) +
  geom_line() +
  ggtitle("Percentage Population Change Over the Years") +
  xlab("Year") +
  ylab("Percentage Change") +
  theme_minimal()

#4a: Load AirQuality data
airquality_data <- as.data.frame(AirQuality) 
# Scatter plot with a fit line
ggplot(airquality_data, aes(x=Wind, y=Solar.R)) +
  geom_point() + # use geom_scatter to create a scatterplot
  geom_smooth(method="lm") +
  ggtitle("Wind vs Solar Radiation with Fit Line") +
  xlab("Wind") +
  ylab("Solar Radiation") +
  theme_minimal()
#4b: use density plot
# Plot distribution of Wind
ggplot(airquality_data, aes(x=Wind)) +
  geom_density() +
  ggtitle("Distribution of Wind") +
  xlab("Wind") +
  theme_minimal()
# Plot distribution of Solar Radiation
ggplot(airquality_data, aes(x=Solar.R)) +
  geom_density() +
  ggtitle("Distribution of Solar Radiation") +
  xlab("Solar Radiation") +
  theme_minimal()
#4c
# Reshape data for comparison
airquality_short <- airquality_data%>%
  pivot_longer(-c(Ozone, Temp, Month, Day), names_to="Variables", values_to="Values")
# Plot multiple distributions
ggplot(airquality_long, aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  ggtitle("Comparing Distributions of Wind and Solar Radiation") +
  theme_minimal()
#4d
# QQ plot comparing Wind and Solar.R
qqplot(airquality_data$Wind, airquality_data$Solar.R, main = "QQ Plot of Wind vs Solar Radiation")
abline(0, 1)
ggplot(airquality_data, aes(sample=Wind, sample=Solar.R))+geom_qq()+
  geom_qq_line()


