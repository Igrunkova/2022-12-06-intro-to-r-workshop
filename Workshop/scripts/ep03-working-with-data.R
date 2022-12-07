
#    Get started with dplyr and tidyr packages from the TidyVerse
#    Extract columns from datasframes using select()
#    Extract rows datasframes filter()
#    Link the output of one dplyr function to the input of another function with the 'pipe' operator
#    Add new columns to dataframes using functions of existing columns with mutate()
#    Use summarize(), group_by(), and count() to split a data frame into groups of observations, 
#    Apply summary statistics to groups
#    Understand wide vs long table formats and why these formats are useful
#    Describe key-value pairs
#    Reshape a data frame from long to wide format and back with the pivot_wider() and pivot_longer()
#    Save a dataframes to .csv files
# Data is available from the following link (we should already have it)
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

# Read some data from a CSV file
s <- read.csv("data_raw/portal_data_joined.csv")


# lets remind ourselves what the dataframe looks like with str(), view() etc ...
view(s)
s   #summary

# https://dplyr.tidyverse.org/
# Load up the required "dplyr" library from the TidyVerse
library(dplyr)
# Some common dplyr functions - select(), filter(), mutate(), group_by(), summarize()
select(s,year)   #select year column
select(s,sex)
# include particular columns: eg plot_id, species_id and weight
select(s,year,sex)
select(s,plot_id,species_id,weight)
# exclude particular columns, eg record_id and species_id using a '-'
select(s,-record_id)
select(s,-record_id, -month)


# filter() - subset of rows (observations)
# == is logical comparison
# all rows where year is 1995
filter(s,year==1995)
# oldest year observation rows (hint max(year, ))
filter(s,max(year) ==year)
filter(s,mean(year)<=year)
# longest hindfoot_length
max(s,hindfoot_length)   #problem with NA
max(s$hindfoot_length[!is.na(s$hindfoot_length)])


# Pipes - plumbing thing together to create pipelines
# using temp dataframes, get rows where weight greater then 5 and show only species_id, sex and weight
filter(s,weight >5)
f <- filter(s,weight >5)
select(f, species_id,sex, weight)   # and we can store/assign the final result to an object
select(filter(s,weight >5),species_id,sex, weight)  #alternatively
# %>% is pipe
filter(s, weight>5) %>% select(species_id, sex, weight)




# alternative way filter(s, weight>5) %>% select(species_id, sex, weight)

s %>%
  filter(weight>5) %>%
  select(species, sex, weight)
# CHALLENGE 1
# Using pipes, subset 'surveys' dataframe to extract animals collected before 1995 and 
# retain only the columns called year, sex, and weight.
s %>%
  filter (year < 1995) %>%
  select(year, sex, weight)

# mutate() - add columns to a dataframe, lets add a column called weight_kg by dividing the weight by 1000
mutate(s, weight_kg=weight/1000)
s %>%   #won't add to the dataframe, just on display 
  mutate(weight_kg=weight/1000)

# but we need to assign the new column to the dataframe 
s <- s %>%     # add to dataframe
  mutate(weight_kg=weight/1000)

# we can also add multiple columns at the same time - ie, we cloud also add weight_lb by multiplying by 2.2
s <- s %>% 
  mutate(weight_kg=weight/1000, weight_lb=weight_kg*2.2)

# using head() can be useful now 
s %>%
  filter (year < 1995) %>%
  select(year, sex, weight) %>%
  head()    #() for first 6 records

s %>%
  filter (year < 1995) %>%
  select(year, sex, weight) %>%
  tail(10)  #look at the last 10 records
head(1)   # display the first record

# NA means "not available". We check is a data value is NA with the function is.na() and ! means 'not'
# Create a new data frame from the surveys dataframe that meets the following criteria:
# contains only the species_id column and a new column called hindfoot_cm containing 
# the hindfoot_length  values in millimeters converted to centimeters.
# The hindfoot_cm column, should have no NA's and all values need to be less than 3.
# Hint: think about how the commands should be ordered to produce this data frame!

s <- s %>% 
  mutate(hindfoot_cm = hindfoot_length/10) %>%
  filter( !is.na(hindfoot_cm), 
          hindfoot_cm<3, 
          na.rm=TRUE) %>%
  select(species_id, hindfoot_cm)

# group_by() - collect like things together to allow us to summarise them
s %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  tail() ## NOTE: Richard should have added this tail() so we could see rows with calculations

s %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))


#move on to tidyyr
library(tidyr) 
# A "Wide" dataframe
#     day   tesla  ford   kia mazda    <---- Names
#     -----------------------------
#     sat       2     1     3     6    <---- Values
#     sun      63    71    95    12    <---- Values
cars_wide <- data.frame (
  day   = c("sat", "sun"),
  tesla = c(2, 63),
  ford  = c(1,71),
  kia   = c(3, 95),
  mazda = c(6,12)
) 

# better format the table this way for R
# Same information represented in a "Long" dataframe
#          Key   Value
#     day  make  qty
#    +----+-----+----+
#    sat  tesla  2
#    sun  tesla  63
#    sat  ford   1
#    sun  ford   71
#    sat  kai     3
#    sun  kai     95
#    sat  mazda   6
#    sun  mazda   12
# tidyr's pivot_longer() can do this for us
pivot_longer(cars_wide, names_to = "make", values_to = "qty" , col = -day )   # - tells that day column is excluded - not part of the pivot 

cars_wide %>% 
  pivot_longer(names_to = "make", values_to = "qty" , col = -day)

# and the reverse 
cars_long %>%
  pivot_wider(names_from = make, values_from = qty)

# now we can apply to our surveys data
surveys_long <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarize(mean_weight = mean(weight))
#Creating a new data.frame

surveys_complete <- s %>%
  filter(!is.na(weight),       	# remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))            	# remove missing sex

## Extract the most common species_id
species_counts <- surveys_complete %>%
  count(species_id) %>%
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, file = "data_out/surveys_complete.csv")


