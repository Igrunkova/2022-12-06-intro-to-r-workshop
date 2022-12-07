# # Visualising data with ggplot2

# Based on: https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Plotting with ggplot2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# load ggplot
#library(ggplot2) #this is included in tidyverse
library(tidyverse)

# load data

surveys_complete <- read_csv("data_out/surveys_complete.csv")

# empty plot (data= -is not nesessary)

ggplot(data=surveys_complete)
ggplot(surveys_complete)

# empty plot with axes

ggplot(data = surveys_complete, mapping = aes(x = hindfoot_lenght, y = weight))
  
# data appears on the plot
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_lenght))+ 
  geom_point()


# assign a plot to an object
survey_plot

splot <- ggplot(data=surveys_complete, mapping = aes(x= weight, y= hindfoot_lenght))
# display the ggplot object (plus add an extra geom layer)

splot +
  geom_point()


# ------------------------
# Exercise/Challenge 1
# ------------------------
# Change the mappings so weight is on the y-axis and hindfoot_length is on the x-axis

ggplot(data=surveys_complete, mapping = aes(x= hindfoot_length, y= weight)) +
  geom_point()
install.packeges ("hexbin")
library (hexbin)

splot +
  geom_hex()

ggplot(data=surveys_complete, mapping = aes(x= hindfoot_length)) +
  geom_histogram(binwidth=10)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Building plots iteratively
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# alpha = transparancy (0;1)

splot +
  geom_point(alpha = .1, colour = 135)
splot +
  geom_point(alpha = .1, colour = 565)
splot +
  geom_point(alpha = .1, aes (colour = species_id))




# ------------------------
# Exercise/Challenge 2
# ------------------------
#
# Use what you just learned to create a scatter plot of weight over species_id 
# with the plot type showing in different colours. 
# Is this a good way to show this type of data?

ggplot(data=surveys_complete)

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_point(alpha = .1, aes(colour = plot_id))
#better visualisation
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = .1, aes(colour = plot_id))

#not a great way to display this data...


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Boxplots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

# one discrete, one continuous variable

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot()

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 3, colour = "tomato")

#or 
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 3, colour = "tomato") +
  geom_boxplot(alpha = 0) 

# ------------------------
# Exercise/Challenge 3
# ------------------------

# Notice how the boxplot layer is behind the jitter layer? What do you need to change in the code to put the boxplot in front of the points such that it's not hidden?



# ------------------------
# Exercise/Challenge 4
# ------------------------

# Boxplots are useful summaries but hide the shape of the distribution. 
# For example, if there is a bimodal distribution, it would not be observed 
# with a boxplot. An alternative to the boxplot is the violin plot 
# (sometimes known as a beanplot), where the shape (of the density of points) 
# is drawn.
# 
#Replace the box plot with a violin plot
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 3, colour = "tomato") +
  geom_boxplot(alpha = 0) +
  geom_violin(alpha = 0)




# ------------------------
# Exercise/Challenge 5
# ------------------------

# So far, we've looked at the distribution of weight within species. Make a new 
# plot to explore the distribution of hindfoot_length within each species.
# Add color to the data points on your boxplot according to the plot from which 
# the sample was taken (plot_id).

# Hint: Check the class for plot_id. Consider changing the class of plot_id from 
# integer to factor. How and why does this change how R makes the graph?

# with a color scale
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 3, aes(colour = plot_id)) +
  geom_boxplot(alpha = 0)

class(surveys_complete$plot_id)

surveys_complete$plot_id <- as.factor(surveys_complete$plot_id)

#now run again, and there are discrete colors:
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = .3, aes(colour = plot_id)) +
  geom_boxplot(alpha = 0)

# alternately, we can change the class of plot_id on the fly (without changing data object)

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = .3, aes(colour = as.factor(plot_id)))  +
  geom_boxplot(alpha = 0)



# ------------------------
# Exercise/Challenge 6
# ------------------------

# In many types of data, it is important to consider the scale of the 
# observations. For example, it may be worth changing the scale of the axis to 
# better distribute the observations in the space of the plot. Changing the scale
# of the axes is done similarly to adding/modifying other components (i.e., by 
# incrementally adding commands). 
# Make a scatter plot of species_id on the x-axis and weight on the y-axis with 
# a log10 scale.

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = .3, aes(colour = as.factor(plot_id)))  +
                geom_boxplot(alpha = 0) +
                scale_y_log10()
              


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Plotting time series data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 

# counts per year for each genus
year_counts <- surveys_complete %>%
  count(year,genus)

ggplot(data=year_counts, mapping = aes(x=year, y=n, group=genus)) +
  geom_line()

# ------------------------
# Exercise/Challenge 7
# ------------------------
# Modify the code for the yearly counts to colour by genus so we can clearly see the counts by genus. 


ggplot(data=year_counts, mapping = aes(x=year, y=n, colour=genus)) +
  geom_line()


# OR alternately
# integrating the pipe operator with ggplot (no need to make a separate dataframe)

surveys_complete %>%
  count(year, sex, genus) %>%
  ggplot(mapping = aes(x=year, y=n, colour=genus)) +
  geom_line() +
  labs(y="number of observations")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Faceting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Split one plot into Multiple plots
ggplot(data=year_counts, mapping = aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

# organise rows and cols to show sex and genus
year_sex <-surveys_complete %>%
  count (year,sex,genus)

ggplot(data = year_sex, mapping = aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

surveys_complete %>%
  count (year,sex,genus) %>%
  ggplot(mapping= aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))
  
# organise rows by genus only
surveys_complete %>%
  count(year, sex, genus) %>%
  ggplot(mapping = aes(x=year, y=n, colour=genus)) +
  geom_line() +
  labs(y="number of observations")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Faceting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Split one plot into Multiple plots
ggplot(data=year_counts, mapping = aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

# organise rows and cols to show sex and genus

ggplot(data = year_sex, mapping = aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_grid(row = vars(sex), col = vars(genus))

# organise rows by genus only
ggplot(data = year_sex, mapping = aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_grid(row = vars(genus))

# ------------------------
# Exercise/Challenge 8
# ------------------------
# How would you modify this code so the faceting is organised into only columns 
# instead of only rows?

ggplot(data = year_sex, mapping = aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_grid(col = vars(genus))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Themes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# themes set a look

ggplot(data=year_sex, mapping = aes(x=year, y=n, colour = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  theme_bw()


# ------------------------
# Exercise/Challenge 9
# ------------------------
# Put together what you've learned to create a plot that depicts how the average 
# weight of each species changes through the years.
# Hint: need to do a group_by() and summarize() to get the data before plotting
year_weight <- surveys_complete %>%
  group_by(year,species_id) %>%
  summarize(mean_weight = mean(weight))
year_weight %>%
ggplot(mapping = aes(x=year, y=mean_weight, colour=species_id)) +
  geom_line() +
  facet_wrap(vars(species_id)) +
  theme_bw()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Customisation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Makinging it your own
year_sex %>%
  ggplot(mapping = aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "observed genera through time",
       x= "year of observation",
       y= "number of ingividuals") +
theme_bw()  + #removes grey background
  theme(text = element_text(size=16))


my_plot <- year_sex %>%
  ggplot(mapping = aes(x=year, y=n, colour=sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "observed genera through time",
       x= "year of observation",
       y= "number of ingividuals") +
  theme_bw()  + #removes grey background
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "grey20", 
                                   size=12, 
                                   angle=90,
                                   hjust = 0.5, 
                                   vjust = 0.5),
        strip.text = element_text(face="italic"))
  

# save theme configuration as an object



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Topic: Exporting plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggsave("figures/my_plot_tester1.pdf", my_plot, width = 15, height = 10)
ggsave("figures/my_plot_tester1.tiff", my_plot, width = 15, height = 10)
