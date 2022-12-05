#    ____                                 _             _   _             
#   |  _ \  ___ _ __ ___   ___  _ __  ___| |_ _ __ __ _| |_(_) ___  _ __  
#   | | | |/ _ \ '_ ` _ \ / _ \| '_ \/ __| __| '__/ _` | __| |/ _ \| '_ \ 
#   | |_| |  __/ | | | | | (_) | | | \__ \ |_| | | (_| | |_| | (_) | | | |
#   |____/ \___|_| |_| |_|\___/|_| |_|___/\__|_|  \__,_|\__|_|\___/|_| |_|
#                                                                         

#
# A very quick demonstration of the sorts of things you can do with R
#

# Read a CSV file
diamonds <- read.csv(file = "data_raw/diamond.csv")

# Load the GGPLOT2 library
library(ggplot2)

# Generate a Plot 
ggplot(diamonds) + 
  geom_point(aes(x=carat, y=price, color=cut)) +
  geom_smooth(aes(x=carat, y=price))





#    ____                       ____                          _ _       ____            _     
#   / ___|  __ ___   _____     / ___|___  _ __ ___  _ __ ___ (_) |_    |  _ \ _   _ ___| |__  
#   \___ \ / _` \ \ / / _ \   | |   / _ \| '_ ` _ \| '_ ` _ \| | __|   | |_) | | | / __| '_ \ 
#    ___) | (_| |\ V /  __/_  | |__| (_) | | | | | | | | | | | | |_ _  |  __/| |_| \__ \ | | |
#   |____/ \__,_| \_/ \___( )  \____\___/|_| |_| |_|_| |_| |_|_|\__( ) |_|    \__,_|___/_| |_|
#                         |/                                       |/                         

# save file changes

# commit changes to git version control

# push commits (changes) to GitHub using a token

# Step 1
#
# Create a new Personal Access Token (PAT)
usethis::create_github_token(scopes = 'repo', description = 'R Workshop')

# Step 2
#
# Save our Personal Access Token (PAT)
gitcreds::gitcreds_set(url = "https://github.com")