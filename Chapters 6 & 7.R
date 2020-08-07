#Chapter 6: Introduction to Data Visualization
library(tidyverse)
library(ggplot2)
library(dslabs)
data("murders")
head(murders)

#A picture is worth a thousand words
#In some cases, the visualization is so convincing that
#no follow-up analysis is required.
#Data visualization is the STRONGEST tool of EDA.
#EDA is perhaps the most important part of data analysis;
#Yet it is one that is often overlooked.
#The fact is that it can be difficult or IMPOSSIBLE to notice
#an error just from reported rresults makes data visualization particularly important.



#Chapter 7
murders <- murders %>%
  mutate(population = population/10^6)
head(murders)

p <- murders %>%
  ggplot()

#Base plot
p + geom_point(aes(x = population, y = total))


#geom_label: add text to the plot with a rectanglebehind the text
#geom_text: add text to the plot without a rectangle behind the text
#because the size affects all the points the same way, we do not need it to be included inside
#using nudge_x argument to move the text slighlty to the right
p + geom_point(aes(x = population, y = total), size = 4) +
  geom_text(aes(x = population, y = total, label = abb), nudge_x = 2)


#Global versus Local Aesthetic
p <- murders %>%
  ggplot(aes(x = population, y = total, label = abb))


#Then add geom_point() and geom_text
p + geom_point(size = 5) +
  geom_text(nudge_x = 2)

#Adding color, scales and labels
#since the choice of color is determined by a feature of each observation,
#this is an aesthetic mapping.
p + geom_point(aes(color = region), size = 5) +
  geom_text(nudge_x = 0.15) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")


#Adding abline
r <- murders %>%
  summarise(rate = sum(total)/sum(population)) %>%
  .$rate

#Hence, we get:
p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(color = region), size = 5) +
  geom_text(nudge_x = 0.15) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_brewer(name = "Region", palette = "Set1")


#7.11 Add-on Packages
#packages = ggthemes & ggrepel
library(ggrepel)
library(ggthemes)
p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(color = region), size = 5) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_brewer(name = "Region", palette = "Set1") +
  theme_economist()


#For grid of plots, use grid.arrange() in the gridExtra package















































  
