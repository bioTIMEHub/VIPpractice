############################################################
########## little data wrangling intro #####################
########## VB, 21022023                #####################
############################################################


## useful resources here:
# https://support--rstudio-com.netlify.app/resources/cheatsheets/
# https://ourcodingclub.github.io/links.html (data manipulation)



##  load libraries ##
require(dplyr)
require(tidyr)

# Create a folder that will be your working enviroment
# In your working environment, create a folder called "data" 
# In data, paste the files we are going to read now.

getwd() # see where the working directory and fix it

### read two datasets:
## the first dataset has 5 columns: "site","years","taxa","abundance", "temperature"
## the second dataset has 3 columns: "site", "longitude", "latitude"

## read dataset 1
records <- read.csv("./data/records.csv")

names(records)
dim(records)
head(records)
str(records)
#as we don't need to do charachter manipulation we can change characters into factors
records$site <- as.factor(records$site)
records$taxon <- as.factor(records$taxon)

## and think pipes instead of base R

## only retain relevant columns (we don't need temperature) with select()
records %>%
  select(site, year,taxon,abundance)

# you can attribute this to an element, like in base R
records <- records %>%
  select(site, year,taxon,abundance)

names(records)

## counts() and group_by() help you tally your data
# how many times each taxon was registered?
records %>%
  count(taxon)
# how many records each year has?
records %>%
  group_by(year) %>%
  count() #or tally()
# how many records each site has?
records %>%
  group_by(site) %>%
  count()
# this tallies are not grouped. 
# we can also count how many taxon were observed eack year by using group_by()
records %>%
  group_by(year) %>%
  count(taxon)%>%
  View() # this open a table in another tab - useful for longer tables
# how many sites were observed each year?
records %>%
  group_by(year) %>%
  count(site)

## filter some records with filter()
records %>%
  filter(year == 1000)

records %>%
  filter(taxon %in% c("corallo viola", "pesciolino nero"))

## split a character column into 2 with separate()

records_g_sp <- records %>%
  separate(taxon, c("genus", "species"), sep = " ", remove = FALSE)%>%
  select(-taxon)

## with base R it would be less readable
# library(stringr)
# genus <- str_split_fixed(records$taxon, pattern = " ", n = 2)[,1]
# species <- str_split_fixed(records$taxon, pattern = " ", n = 2)[,2]
# recs_baseR <- cbind(records,genus, species)

### switch between long and wide formats
# gather() and spread()
# e.g have years as columns and store abundance of each case
records_wide <- records_g_sp %>%
  spread(key = year,value = abundance, fill = 0)
records_wide
# bring it back to what it was (remember to remove 0 abundances)
records_long <- records_wide %>%
  gather(key = "year", value = "abundance", 4:9)%>%
  filter(abundance != 0)
records_long

dim(records_long)
dim(records_g_sp)

## join tables
# read the other dataframe provided
sites <- read.csv("./data/sites.csv")
sites
# it has more sites than we have in the records dataframe
names(sites)
dim(sites)
str(sites)
sites$sites <- as.factor(sites$sites)

##there's a lot of different ways to join tables ()
# join left retains all the cases (rows) of x and adds variables in y that match

records_coords <- left_join(x = records_g_sp,y = sites,by=c("site" = "sites"))
dim(records_coords)

## save this dataframe 
write.csv(records_coords, "clean_madeup_data.csv")

##some other functions
## make new variables with mutate()
records_coords2 <- records_coords %>%
  mutate(genus_species = paste(genus, species, sep = "_"))
records_coords2

# add total abundance across taxa per site per year
records_coords3 <- records_coords2 %>%
  group_by(year,site) %>%
  mutate(tot_ab = sum(abundance))

records_coords3 %>%
  select(site, year, tot_ab)%>%
  distinct()
