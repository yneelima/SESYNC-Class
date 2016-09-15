## Libraries and data

library(dplyr)
library(ggplot2)
surveys <- read.csv("data/surveys.csv", na.strings = "") %>%
  filter(!is.na(species_id), !is.na(sex), !is.na(weight))#removing na species, na sex, and na weight from data, "!" means "not"

## Constructing layered graphics in ggplot

ggplot(data=surveys,
       aes(x=species_id, y = weight)) + # finishes the ggplot call right here - + adds a new call; aes defines an aesthetic mapping
  geom_point()

ggplot(data = surveys,
       aes(x = species_id, y = weight)) +
  geom_boxplot()+               # each geom is its own layer
  geom_point(stat= "summary",   # instead of plotting all points, this tells it to plot a specific statistic
             fun.y = "mean",
             color = "red")
qplot(x=species_id, y= weight, data=surveys, geom= "boxplot") #quick plot single layer plotting


## Exercise 1
surveys_DM<- filter(surveys, species_id=="DM")
ggplot(data=surveys_DM,
       aes(x=year, y=weight))+
  geom_point(stat="summary",
             fun.y="mean",
             aes(color=factor(sex))) #added the aesthetic to geom point object

surveys_DM<- filter(surveys, species_id=="DM")
ggplot(data=surveys_DM,
       aes(x=year, y=weight, color=factor(sex)))+
  geom_point(stat="summary",
             fun.y="mean")

## Adding a regression line

levels(surveys$sex) <- c("Female", "Male") # allows to be able to group on factors - makes the strings factors
surveys_DM <- filter(surveys, species_id=="DM")
ggplot(surveys_DM,
       aes(x = year, y = weight)) +
  geom_point(aes(shape=sex, color = sex),
             size = 3,
             stat = "summary",
             fun.y = "mean") +
  geom_smooth(aes(group=sex, color = sex), method="lm")



ggplot(data = surveys_DM,
       aes(x=year,
           y=weight,
           color=sex)) +  #puts color specifications in the begining
  geom_point(aes(shape = sex),
             size = 3,
             stat = "summary",
             fun.y = "mean") +
  geom_smooth(method = "lm")

# Storing and re-plotting

year_wgt <- ggplot(data = surveys_DM,
                   aes(x = year,
                       y = weight,
                       color = sex)) +
  geom_point(aes(shape = sex),
             size = 3,
             stat = "summary",
             fun.y = "mean") +
  geom_smooth(method = "lm")

year_wgt +
  scale_color_manual(values=c("pink","black"))
                     
year_wgt <- year_wgt +
  scale_color_manual(values=c("light blue","black"))
year_wgt

## Exercise 2

surveys_DM<- filter(surveys, species_id=="DM")
ggplot(data=surveys_DM,
       aes(weight, fill=sex))+
  geom_histogram(
    binwidth=.5) +
  scale_fill_manual(values=c("light blue","black"))

## Axes, labels and themes

histo <- ggplot(data = surveys_dm,
                aes(x = weight, fill = sex)) +
  geom_...
histo

histo <- histo +
  ...(title = "Dipodomys merriami weight distribution",
       x = "Weight (g)",
       y = "Count") +
  scale_x_continuous(limits = c(20, 60),
                     breaks = c(20, 30, 40, 50, 60))
histo

histo <- histo +
  theme_bw() +
  theme(legend.position = c(0.2, 0.5),
        plot.title = ...,
        ... = element_text(...),
        ... = element_text(size = 13, vjust = 0))
histo

## Facets

surveys_dm$month <- as.factor(surveys_dm$month)
levels(surveys_dm$month) <- c("January", "February", "March", "April", "May", "June",
                              "July", "August", "September", "October", "November", "December")

ggplot(data = surveys_dm,
       aes(x = weight)) +
  geom_histogram() +
  ...
  labs(title = "DM weight distribution by month",
       x = "Count",
       y = "Weight (g)")

ggplot(data = surveys_dm,
       aes(x = weight, fill = month)) +
  ...
  facet_wrap( ~ month) +
  labs(title = "DM weight distribution by month",
       x = "Count",
       y = "Weight (g)") +
  guides(fill = FALSE)

## Exercise 3

...

