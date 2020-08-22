#Chapter 10: Data visualization
#Case study: vaccines and infectious diseases
library(tidyverse)
library(RColorBrewer)
library(dslabs)
library(bbplot)
data("us_contagious_diseases")

#Create a temporary obeject containing "Measles"
measles_disease <- "Measles"

dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == measles_disease) %>%
  mutate(rate = ((count*10000*52)/(population*weeks_reporting))) %>%
  mutate(state = reorder(state, rate))


dat %>% tibble() %>%
  filter(state == "Texas" & !is.na(rate)) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept = 1963, lty = 2, colour = "blue", size = 2) +
  geom_text(aes(x = 1963, y = 90, label = "Vaccine Introduced"), size = 7, hjust = 0, colour = "blue") +
  labs(title = "Vaccination works",
       subtitle = "Texas Cases per 10,000") +
  bbc_style()
  

#Displaying the state, year and rate using geom_tile()
Measles_viz <- dat %>% 
  ggplot(aes(x = year, y = state, fill = rate)) +
  geom_tile(colour = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, lty = 2, colour = "blue", size = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 8)) +
  labs(title = "Measles", size = 15) +
  xlab("") +
  ylab("")


#Finalizing Plot
finalise_plot(plot_name = Measles_viz,
              source_name = "Source: The Tycho Project compiled by the dslabs package",
              save_filepath = "Measles_viz.png")





#Plot 2
avg <- us_contagious_diseases %>%
  filter(disease == "Measles") %>%
  group_by(year) %>%
  summarise(us_rate = sum(count, na.rm = TRUE)/
              sum(population, na.rm = TRUE)*10000)
  

Tycho_project <- dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(x = year, y = rate, group = state),
            colour = "grey50", show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(data = avg, aes(x = year, y = us_rate), size = 2) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  labs(title = "Vaccinations work",
    subtitle = "Measles cases per 10,000 by state in the US") +
  xlab("") +
  ylab("") +
  geom_text(aes(x = 1952, y = 55, label = "US Average"), colour = "black", size =10) +
  geom_text(aes(x = 1964, y = 300, label = "Measles Vaccinations\nintroduced"), colour = "blue", size =8, hjust = 0) +
  geom_vline(xintercept = 1963, col = "blue", size = 2, lty = 2) +
  bbc_style()


#Finalizing Plot
finalise_plot(plot_name = Tycho_project,
              source_name = "Source: The Tycho Project compiled by the dslabs package",
              save_filepath = "Tycho_project.png")









  
  
  
  
  

