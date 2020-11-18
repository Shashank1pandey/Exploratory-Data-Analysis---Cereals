library(ggplot2)
library(dplyr)
library(plyr)
library(DMwR)
library(hexbin)
library(GGally)

setwd("C:/Users/Shashank/Desktop/project")
getwd()
project <- read.csv("cereals_data.csv")

colnames(project) # Column names in the cereals data file
ncol(project) # Total number of columns in the data file
str(project)

# Vitamins and shelf are factors so we need to change its data type
project <- transform(project, shelf=as.factor(shelf), vitamins=as.factor(vitamins))

colnames(project)
# Changing column names for better understanding of visualizations
colnames(project) <- c("Product's Name","Manufacturer","Type","Calories","Protein","Fat",
                       "Sodium","Fiber","Carbohydrates","Sugars","Potass","Vitamins","Shelf","Weight",
                       "Cups","Rating")

# Changing h and c in type column as hot and cold
temp <- project
project$Type <- revalue(project$Type, c("H"="Hot","C"="Cold"))

# Changing the companies name to real name
project$Manufacturer <- revalue(project$Manufacturer,c("N"="Nabisco","Q"="Quaker Oats","K"="Kelloggs", "R"="Ralston Purina","G"="General Mills","P"="Post","A"="AHFP"))
unique(project$Manufacturer)

# Dealing with missing values
# Total number of missing values
sum(is.na(project))

# Columns containing the missing values
colSums(is.na(project))
# Looking at the results, there are 2 missing value in Potass, 1 in Sugars and 1 in Carbohydrates

# Checking the structure of project to ensure the data type of each column
str(project)

# Since the data contains factors as hot and cold
na_type <- project %>% select(Type,Carbohydrates,Potass,Sugars) %>% group_by(Type) %>% summarise_each(funs(sum(is.na(.))))
na_type
write.csv(na_type,"Missing_Value.csv")

# Plotting histogram/density plot to check its distribution
hist(project$Carbohydrates)
plot(density(project$Carbohydrates,na.rm=T), main="Density Plot of Carbohydrates")

# Plotting density plot based on type group
ggplot(project, aes(x=Calories)) + geom_density(size=2,alpha=.4, color="blue", fill="blue") + theme_bw()

# Since the distribution of carbo in hot doesn't follow normal distribution, filtering the records so that it only contain required fields and Hot type
hot_ <- project %>% filter(Type=='Hot')
hot_

# Density plot
mfr_count <- project %>% 
  select(Manufacturer, Calories) %>%
  group_by(Manufacturer) %>%
  tally()

# Histogram
ggplot(project, aes(x=Calories)) +
  geom_histogram(aes(fill=Manufacturer))

# Using KNN imputation for replacing missing values
project <- knnImputation(project[ , c(-1,-2,-3)])
sum(is.na(project))

# Verifying there are no missing values in the dataset
# Box plot for protein, fat, fiber, carbo, sugars because they have similar range
boxplot(project[ , c(2,3,5,6,7)], col = topo.colors(5))

# Box plot for sodium, potass and rating
boxplot(project[ , c(4,8,13)], col = terrain.colors(3))
ggplot(data = project) +
  geom_hex(mapping = aes(x = Vitamins, y=Calories), na.rm = TRUE) + theme_dark()

# Correlation matrix among continuous data
project %>%
  select(Calories, Protein, Fat, Sodium, Fiber, Carbo = Carbohydrates, Sugars, Potass, Weight, Rating) %>%
  ggcorr(label = TRUE)
pairs(~ Calories + Protein + Fat + Sodium + Fiber + Carbohydrates + Sugars + Potass + Weight + Rating, data = project)

# Scatter plot of weight and calories
# Hex chart
project %>%
  ggplot(aes(x=Weight, y=Calories)) +
  geom_hex(size = 3) + theme_bw()
# Shelf
project %>%
  ggplot(aes(x=Weight, y=Calories, color = Shelf)) +
  geom_point(size = 3) + theme_bw()
# Vitamins
project %>%
  ggplot(aes(x=Weight, y=Calories, color =Vitamins)) +
  geom_point(size = 3) + theme_bw()

# Scatter plot for calories and rating
# Sodium
project %>%
  ggplot(aes(x=Sodium, y=Calories)) +
  geom_hex(size = 3) + theme_bw()
# Sugars
project %>%
  ggplot(aes(x=Sugars, y=Calories)) +
  geom_hex(size = 3) + theme_bw()
# Fat
project %>%
  ggplot(aes(x=Fat, y=Calories)) +
  geom_hex(size = 3) + theme_bw()

# Box plot for different categories
# Shelf
project %>%
  ggplot(aes(y = Calories, x = Shelf, fill = Shelf)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)
# Vitamins
project %>%
  ggplot(aes(y = Calories, x = Vitamins, fill = Vitamins)) +
  geom_boxplot() + geom_jitter(alpha = 0.6, width = 0.1)