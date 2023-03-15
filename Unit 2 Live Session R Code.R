# Unit 2 DDS Live Session Soluton R Code

library(plotly)
library(tidyverse)

# read
PB = read.csv(file.choose(), header = TRUE)

# Clean

#F-C and C-F and F-G and G-F
PB$position2 = if_else(PB$position == "C-F","F-C",PB$position)
PB$position2 = if_else(PB$position == "G-F","F-G",PB$position2)

#Identify Missing Position Row ... 
which(PB$position == "")
PB[2143,] #It is George Karl!

#George Karl Point Guard (https://en.wikipedia.org/wiki/George_Karl)
PB[2143,]$position2 = "G"
PB[2143,]$height = "6-2"
PB[2143,]

PB$position2 = factor(PB$position2, levels = c("C","F","F-C","F-G","G"))
PB$height2 = factor(PB$height, levels = c("5-3","5-4","5-5","5-6","5-7","5-8","5-9","5-10","5-11","6-0","6-1","6-2","6-3","6-4","6-5","6-6","6-7","6-8","6-9","6-10","6-11","7-0","7-1","7-2","7-3","7-4","7-5","7-6","7-7"))

summary(PB$position2)
summary(PB$height2)


#Question 1
PB %>% ggplot(aes(x = position2, fill = position2)) + geom_bar() + ggtitle("Positions")


#Question 2

PB %>% ggplot(aes(x = position2, y = weight, fill = position2)) + geom_boxplot() + xlab("Position")

PB %>% filter(position2 == "C" | position2 == "F") %>% ggplot(aes(x = position2,y = weight, fill = position2)) + geom_boxplot() + xlab("Position") + ggtitle("Weight vs C and F")

PB %>% filter(position2 == "C" | position2 == "F") %>% ggplot(aes(x = weight, fill = position2)) + geom_histogram() + facet_wrap(~position2) + ggtitle("Weigth: C vs. F")

# Question 3a

PBPlot = PB %>% filter(position2 == "C" | position2 == "F") %>% ggplot(aes(x = height2, fill = position2)) + geom_bar() + ggtitle("Height: C vs. F") + xlab("Height")
PBPlot

PBPlot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#Question 3b

#New Material: Make Height numeric
PB = PB %>% separate("height",c("Feet","Inches"), sep = "-")
head(PB)
PB$Feet = as.numeric(PB$Feet)
PB$Inches = as.numeric(PB$Inches)

PB$heightInch = 12*PB$Feet + PB$Inches
head(PB)

PBPlot = PB %>% filter(position2 == "C" | position2 == "F") %>% ggplot(aes(x = position2, y = heightInch, fill = position2)) + geom_boxplot() + ggtitle("Height: C vs. F") + xlab("Position") + ylab("Height in Inches")
PBPlot

#Extra .. Question from FLS Draw Mean of C and F superimposed

PB_CandF = PB %>% filter(position2 == "C" | position2 == "F")

CFMeanHeight = mean(PB_CandF$heightInch)

PBPlot+ geom_hline(yintercept= CFMeanHeight, linetype="dashed", color = "red")


# Question 4A

PBPlot = PB %>% ggplot(aes(x = height2, fill = position2)) + geom_bar() + ggtitle("Height: C vs. F") + facet_wrap(~position2) + xlab("Height")
PBPlot

PBPlot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Question 4B

PB %>% ggplot(aes(x = position2, y = heightInch, fill = position2)) + geom_boxplot() + ggtitle("Height: C vs. F") + xlab("Position") +  ylab("Height in Inches")

# Question 5

PB %>% ggplot(aes(x = weight, y = heightInch)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Height vs. Weight") + xlab("Weight") + ylab("Height")

# Question 6

PB %>% ggplot(aes(x = weight, y = heightInch)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Height vs. Weight") + xlab("Weight") + ylab("Height") + facet_wrap(~position2)

# Question 7

PB %>% ggplot(aes(x = year_start, y = heightInch)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Height vs. Year Started") + xlab("Year Started") + ylab("Height")

# Question 7

PB %>% ggplot(aes(x = year_start, y = heightInch)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Height vs. Year Started") + xlab("Year Started") + ylab("Height") + facet_wrap(~position2)

# Question 8

plot_ly(PB, x = ~year_start, y = ~weight, z = ~heightInch, color = ~position2) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Year Started'),
                      yaxis = list(title = 'Weight'),
                      zaxis = list(title = 'Height')))

# Question 9

library(ggplot2)
library(gganimate)
library(gifski)
library(png)

p <- ggplot(
  PB, 
  aes(x = weight, y=heightInch, size = weight, colour = position2)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  labs(x = "Weight", y = "Height")
p
p + transition_time(year_start) +
  labs(title = "Year: {frame_time}")


# Question 10
EI = read.csv(file.choose(), header = TRUE)
EI$Educ = factor(EI$Educ,levels = c("<12","12","13-15","16",">16"))
summary(EI$Educ)

EIBoxPlot = EI %>% ggplot(aes(x = Educ, y = Income2005, fill = Educ)) + geom_boxplot() +ggtitle("Income v. Education Level") + xlab("Education Level") + ylab("Income")
EIBoxPlot

# Add the mean!

EIBoxPlot + stat_summary(fun = mean, colour = "yellow", geom = "point", shape = 18, size = 3)

