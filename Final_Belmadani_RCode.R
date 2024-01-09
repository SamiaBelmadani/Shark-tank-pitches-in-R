####################
###Import Dataset### 
####################

shark_df = read.csv("C:\\Users\\Belma\\Downloads\\Shark tank pitches.csv")
attach(shark_df)
View(shark_df)

#########################
###DATA PRE-PROCESSING###
#########################

# Convert 'deal' column to binary
shark_df$deal = as.numeric(shark_df$deal)

# Convert 'Multiple Entreprenuers' column to binary
shark_df$Multiple.Entreprenuers = as.numeric(shark_df$Multiple.Entreprenuers)

# Create a city and state column 
library(tidyr)

# Separate 'location' into 'city' and 'state' columns
shark_df = separate(shark_df, location, into = c("city", "state"), sep = ",\\s*")

##############
## Category###
##############

# Count the number of unique values in the 'category' column
num_unique_categories <- length(unique(shark_df$category))

# Display the number of unique values in 'category' column
print(num_unique_categories)

# Get unique values in the 'category' column
unique_categories <- unique(shark_df$category)

# Print the unique categories
print(unique_categories)

# Load the required library
library(dplyr)

# Load the required library
library(dplyr)

# Create a function to map categories to broader categories
categorize <- function(category) {
  case_when(
    category %in% c("Men and Women's Apparel", "Men's Accessories", "Women's Accessories",
                    "Fashion Accessories", "Women's Apparel", "Women's Shoes","Men and Women's Accessories",
                    "Men and Women's Shoes", "Undergarments and Basics", "Costumes") ~ "Apparel and Accessories",
    
    category %in% c("Baby and Child Care", "Baby and Children's Entertainment",
                    "Baby and Children's Apparel and Accessories", "Baby and Children's Bedding",
                    "Baby and Children's Food") ~ "Baby and Children",
    
    category %in% c("Personal Care and Cosmetics", "Homeopathic Remedies", "Health and Well-Being") ~ "Health, Beauty, and Wellness",
    
    category %in% c("Novelties", "Specialty Food", "Home Accessories", "Home Improvement",
                    "Furniture", "Storage and Cleaning Products", "Maternity", "Weddings",
                    "Water Bottles", "Wine Accessories", "Holiday Cheer", "Party Supplies",
                    "Toys and Games", "Pet Products", "Gardening", "Pest Control", "Kitchen Tools") ~ "Home and Lifestyle",
    
    category %in% c("Productivity Tools", "Online Services", "Electronics", "Mobile Apps",
                    "Home Security Solutions", "Automotive") ~ "Electronics and Technology",
    
    category %in% c("Non-Alcoholic Beverages", "Alcoholic Beverages") ~ "Food and Beverages",
    
    category %in% c("Fitness Equipment", "Fitness Programs", "Fitness Apparel and Accessories",
                    "Golf Products", "Outdoor Recreation", "Cycling") ~ "Sports, Fitness, and Recreation",
    
    category %in% c("Professional Services", "Consumer Services", "Education") ~ "Professional Services",
    
    category %in% c("Music", "Entertainment") ~ "Entertainment Industry",
    
    TRUE ~ "Other"  # Assign any other categories not specified to 'Other' but there isn't
  )
}

# Load required libraries
library(dplyr)


# Apply the categorize function to create a new column 'wider_category'
shark_df <- shark_df %>%
  mutate(wider_category = sapply(category, categorize))

# Get unique categories in the 'wider_category' column
unique_categories <- unique(shark_df$wider_category)

# Create dummy variables for 'wider_category'
for (category in unique_categories) {
  shark_df[[paste0( category)]] <- as.integer(shark_df$wider_category == category)
}

###########
###State###
###########

# Count the number of unique values in the 'category' column
num_unique_state <- length(unique(shark_df$state))

# Display the number of unique values in 'category' column
print(num_unique_state)

# Get unique values in the 'category' column
unique_state <- unique(shark_df$state)

# Print the unique categories
print(unique_state)

# Load required libraries
library(dplyr)

# Divide State into US Regions 

shark_df <- shark_df %>%
  mutate(region = case_when(
    state %in% c("NJ", "NY", "PA", "MA", "CT", "RI", "NH", "VT", "ME") ~ "Northeast",
    state %in% c("MN", "IL", "OH", "IA", "IN", "MI", "WI", "KS", "MO", "NE") ~ "Midwest",
    state %in% c("GA", "FL", "NC", "TX", "MD", "SC", "TN", "VA", "AL", "AR", "DE", "DC", "KY", "LA", "MS", "OK", "WV") ~ "South",
    state %in% c("CA", "NV", "OR", "CO", "WA", "AZ", "ID", "MT", "UT", "AK", "HI") ~ "West",
    TRUE ~ "Other"
  ))

# Get unique regions
unique_regions <- unique(shark_df$region)

# Create dummy variables for each unique region
for (region in unique_regions) {
  shark_df[[paste0(region)]] <- as.integer(shark_df$region == region)
}

###########
###Title###
###########

shark_df$title_length <- nchar(shark_df$title)

# Drop the 'wider_category' column
shark_df = shark_df[, !names(shark_df) %in% "title"]

###################
###Entrepreneurs###
###################

# Replace empty strings in 'entrepreneurs' column with NA
shark_df$entrepreneurs[shark_df$entrepreneurs == ""] <- NA

# Drop rows with NAs in the 'entrepreneurs' column
#shark_df <- subset(shark_df, !is.na(entrepreneurs))

# Function to count distinct entrepreneurs based on multiple separators
count_entrepreneurs <- function(names) {
  # Define patterns for various separators: 'and', '&', ',', ', ', and ', '
  pattern <- "\\s*\\b(?:and|&|,\\s*|,\\s+|,)\\b\\s*"
  
  # Split the string using the defined patterns
  split_names <- unlist(strsplit(names, pattern, perl = TRUE))
  
  # Remove leading/trailing whitespace and empty strings
  split_names <- trimws(split_names[split_names != ""])
  
  # Return the count of unique names
  length(unique(split_names))
}

# Apply the function to create the 'number_of_entrepreneurs' column
shark_df$nb_entrepreneurs <- sapply(shark_df$entrepreneurs, count_entrepreneurs)

# Drop the 'entrepreneurs' column
shark_df = shark_df[, !names(shark_df) %in% "entrepreneurs"]

############
###Sharks###
############

# Count the number of unique values in the 'shark' column
num_unique_shark <- length(unique(shark_df$shark4))

# Display the number of unique values in 'shark' column
print(num_unique_shark)

# Get unique values in the 'category' column
unique_shark <- unique(shark_df$shark4)

# Print the unique categories
print(unique_shark)

sorted_frequency <- sort(table(shark_df$shark1), decreasing = TRUE)
print(sorted_frequency)

# List of different shark names
sharks <- c("Barbara Corcoran", "Kevin O'Leary", "Steve Tisch", "Robert Herjavec", 
            "Lori Greiner", "Daymond John", "Jeff Foxworthy", "Mark Cuban", 
            "Kevin Harrington", "John Paul DeJoria", "Nick Woodman")

# Iterate through each shark name and create a dummy variable column
for (shark in sharks) {
  shark_df[[shark]] <- apply(shark_df[, paste0("shark", 1:5)], 1, function(row) {
    as.numeric(shark %in% row)
  })
}

# View the updated dataframe with the new dummy variable columns
head(shark_df)

######################
###Data Exploration###
######################

#Density Plot NUmerical Variables 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Original column names and their new titles
new_titles <- c(
  askedFor = "Amount Asked For",
  exchangeForStake = "Stake Exchange",
  nb_entrepreneurs = "Number of Entrepreneurs",
  title_length = "Title Length",
  valuation = "Valuation"
)

# Filter to include only the selected columns
shark_df_selected <- shark_df %>% select(all_of(names(new_titles)))

# Reshape data to long format for faceting
shark_df_long <- shark_df_selected %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Update factor levels to have custom facet titles
shark_df_long$variable <- factor(shark_df_long$variable, levels = names(new_titles), labels = new_titles)

# Create a combined density plot and histogram for each variable using ggplot2
p <- ggplot(shark_df_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = 'skyblue', colour = 'black', alpha = 0.7) +
  geom_density(color = '#00008B', fill = '#00008B', alpha = 0.2) +
  facet_wrap(~variable, scales = 'free') +
  labs(title = 'Density Plots & Histograms') +
  theme_minimal()

# Print the plot
print(p)


#Boxplot Numerical Variables

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#  excluding 'Multiple Entrepreneurs'
new_titles <- c(
  askedFor = "Amount Asked For",
  exchangeForStake = "Stake Exchange",
  nb_entrepreneurs = "Number of Entrepreneurs",
  title_length = "Title Length",
  valuation = "Valuation"
)

# Filter your data frame to include only the selected columns
shark_df_selected <- shark_df %>% select(all_of(names(new_titles)))

# Reshape data to long format for faceting
shark_df_long <- shark_df_selected %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Update factor levels to have custom facet titles
shark_df_long$variable <- factor(shark_df_long$variable, levels = names(new_titles), labels = new_titles)

# Create a box plot for each variable using ggplot2
p <- ggplot(shark_df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = 'skyblue', colour = 'black', alpha = 0.7) +
  facet_wrap(~variable, scales = 'free') +
  labs(title = 'Box Plots') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) # Hide x-axis labels

# Print the plot
print(p)

##Boxplots for Categorical Variables 

#Boxplot of Stake by Category Buckets
ggplot(shark_df, aes(x = wider_category, y = exchangeForStake, fill = wider_category)) +
  geom_boxplot() +
  labs(
    title = "Stake by Category Buckets",
    x = "Category Buckets",
    y = "Stake",
    fill = "Category Buckets" 
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

#Boxplot of Asked For by Category Buckets
ggplot(shark_df, aes(x = wider_category, y = askedFor, fill = wider_category)) +
  geom_boxplot() +
  labs(
    title = "Asked Amount by Category Buckets",
    x = "Category Buckets",
    y = "Asking Amount",
    fill = "Category Buckets" 
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

#Boxplot of Valuation by Category Buckets
ggplot(shark_df, aes(x = wider_category, y = valuation, fill = wider_category)) +
  geom_boxplot() +
  labs(
    title = "Valuation by Category Buckets",
    x = "Category Buckets",
    y = "Valuation",
    fill = "Category Buckets" 
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


#Bar chart of Stake by Category Buckets
ggplot(shark_df, aes(x = wider_category)) +
  geom_bar(fill = 'skyblue') +
  labs(title = 'Bar Chart of Category Buckets', x = 'Category Buckets', y = 'Count')


ggplot(shark_df, aes(x = factor(`Home and Lifestyle`), y = valuation, fill = factor(`Home and Lifestyle`))) +
  geom_boxplot() +
  labs(
    title = "Valuation by Home and Lifestyle",
    x = "Home and Lifestyle",
    y = "Valuation",
    fill = "Home and Lifestyle" 
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

ggplot(shark_df, aes(x = factor(`Home and Lifestyle`), y = askedFor, fill = factor(`Home and Lifestyle`))) +
  geom_boxplot() +
  labs(
    title = "Amount Asked for Home and Lifestyle",
    x = "Home and Lifestyle",
    y = "Asking Amount",
    fill = "Home and Lifestyle" 
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

ggplot(shark_df, aes(x = factor(`Home and Lifestyle`), y = exchangeForStake, fill = factor(`Home and Lifestyle`))) +
  geom_boxplot() +
  labs(
    title = "Stake Eschange for Home and Lifestyle",
    x = "Home and Lifestyle",
    y = "Stake",
    fill = "Home and Lifestyle" 
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

#Box Plot for Binary Variables 

# Original column names and their new titles
new_titles <- c(
  askedFor = "Amount Asked For",
  exchangeForStake = "Stake Exchange",
  valuation = "Valuation"
)

# Filter  to include only the selected columns
shark_df_selected <- shark_df %>% select(all_of(c("deal", names(new_titles))))

# Reshape data to long format for faceting
shark_df_long <- shark_df_selected %>% 
  pivot_longer(cols = -"deal", names_to = "variable", values_to = "value")

# Update factor levels to have custom facet titles
shark_df_long$variable <- factor(shark_df_long$variable, levels = names(new_titles), labels = new_titles)

# Create a box plot for each variable using ggplot2
p <- ggplot(shark_df_long, aes(x = factor(`deal`), y = value, fill = factor(`deal`))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = 'free') +
  labs(
    title = "Box Plots by Deal",
    x = "deal",
    y = "",
    fill = "deal"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Print the plot
print(p)

##############
###Outliers###
##############

# Set graphical parameters to increase the size of the plot
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.1)  # Adjust the 'mfrow' and 'mar' values as needed

# Create the Studentized Residual Plot
reg = lm(exchangeForStake ~ askedFor + valuation + title_length + nb_entrepreneurs, data = shark_df)
qqPlot(reg, enveloppe = list(style = "none"))

#Bonferroni Test 
outlierTest(reg)

#Remove Observations
shark_df = shark_df[-c(31,215,216),]

########################
###Correlation Matrix###
########################

#Drop KEVIN O'LEARY column because it is a constant column
shark_df <- shark_df[, !(names(shark_df) == "Kevin O'Leary")]

# Install Packages
install.packages("reshape2")
install.packages("ggplot2")
library(reshape2)
library(ggplot2)

# Rename columns directly using names function which is more reliable
names(shark_df)[names(shark_df) == "nb_entrepreneurs"] <- "Number of Entrepreneurs"
names(shark_df)[names(shark_df) == "title_length"] <- "Title Length"
names(shark_df)[names(shark_df) == "Multiple.Entreprenuers"] <- "Multiple Entrepreneurs"
names(shark_df)[names(shark_df) == "valuation"] <- "Valuation"
names(shark_df)[names(shark_df) == "exchangeForStake"] <- "Exchange for Stake"
names(shark_df)[names(shark_df) == "askedFor"] <- "Asked For"
names(shark_df)[names(shark_df) == "deal"] <- "Deal"

# Subset only numeric columns for correlation analysis
quantvars <- shark_df[,c(1,8,9,10,18,20,21,22,23,24,25,26,27,28,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)]

# Compute the correlation matrix
cor_matrix <- cor(quantvars)

# Print the correlation matrix
print(cor_matrix)

# Melt the correlation matrix for use with ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Create a heatmap using ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")

# Calculate the correlation between "Multiple Entrepreneurs" and "Number of Entrepreneurs"
correlation_value <- cor(shark_df[["Multiple Entrepreneurs"]], shark_df[["Number of Entrepreneurs"]], use = "complete.obs")

# Print out the correlation
print(correlation_value) #0.92

# Calculate the correlation between "Valuation" and "Asked For"
correlation_value <- cor(shark_df[["Valuation"]], shark_df[["Asked For"]], use = "complete.obs")

# Print out the correlation
print(correlation_value) #0.76<0.85 (rule of thumb)

#Drop Multiple Entrepreneurs column because it is a constant column
shark_df <- shark_df[, !(names(shark_df) == "Multiple Entrepreneurs")]

#########
###PCA###
#########

shark_labels = shark_df[, c(2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16, 17, 18, 28)]
shark_vars = shark_df[, c(1, 8, 9, 10, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44)]

install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(shark_vars)
install.packages("ggfortify")
library(ggfortify)

pca=prcomp(shark_vars, scale=TRUE)
pca

autoplot(pca, data = shark_vars, loadings = TRUE, loadings.label = TRUE )

#PCA for different regions 
library(ggfortify)
library(patchwork)

# Create individual PCA plots for each region with titles
pca_plot_west <- autoplot(pca, data = shark_vars, loadings = TRUE,
                          col = ifelse(shark_labels$region == "West", "blue", "transparent"),
                          loadings.label = TRUE) + 
  ggtitle("PCA West")

pca_plot_midwest <- autoplot(pca, data = shark_vars, loadings = TRUE,
                             col = ifelse(shark_labels$region == "Midwest", "blue", "transparent"),
                             loadings.label = TRUE) + 
  ggtitle("PCA Midwest")

pca_plot_northeast <- autoplot(pca, data = shark_vars, loadings = TRUE,
                               col = ifelse(shark_labels$region == "Northeast", "blue", "transparent"),
                               loadings.label = TRUE) + 
  ggtitle("PCA Northeast")

pca_plot_south <- autoplot(pca, data = shark_vars, loadings = TRUE,
                           col = ifelse(shark_labels$region == "South", "blue", "transparent"),
                           loadings.label = TRUE) + 
  ggtitle("PCA South")

# Combine the plots with layout
combined_pca_plots <- pca_plot_west + pca_plot_midwest + pca_plot_northeast + pca_plot_south +
  plot_layout(nrow = 2, ncol = 2)

# Print the combined plot
print(combined_pca_plots)


#PCA for different categories
library(ggfortify)
library(patchwork)

# Create individual PCA plots for each season with titles
pca_plot_home <- autoplot(pca, data = shark_vars, loadings = TRUE,
                          col = ifelse(shark_labels$wider_category == "Home and Lifestyle", "blue", "transparent"),
                          loadings.label = TRUE) + 
  ggtitle("PCA Home and Lifestyle")

pca_plot_baby <- autoplot(pca, data = shark_vars, loadings = TRUE,
                          col = ifelse(shark_labels$wider_category == "Baby and Children", "blue", "transparent"),
                          loadings.label = TRUE) + 
  ggtitle("PCA Baby and Children")

pca_plot_professional <- autoplot(pca, data = shark_vars, loadings = TRUE,
                          col = ifelse(shark_labels$wider_category == "Professional Services", "blue", "transparent"),
                          loadings.label = TRUE) + 
  ggtitle("PCA Professional Services")

pca_plot_apparel <- autoplot(pca, data = shark_vars, loadings = TRUE,
                          col = ifelse(shark_labels$wider_category == "Apparel and Accessories", "blue", "transparent"),
                          loadings.label = TRUE) + 
  ggtitle("PCA Apparel and Accessories")

pca_plot_electronics <- autoplot(pca, data = shark_vars, loadings = TRUE,
                          col = ifelse(shark_labels$wider_category == "Electronics and Technology", "blue", "transparent"),
                          loadings.label = TRUE) + 
  ggtitle("PCA Electronics and Technology")

pca_plot_entertainment <- autoplot(pca, data = shark_vars, loadings = TRUE,
                                 col = ifelse(shark_labels$wider_category == "Entertainment Industry", "blue", "transparent"),
                                 loadings.label = TRUE) + 
  ggtitle("PCA Entertainment Industry")

pca_plot_sports <- autoplot(pca, data = shark_vars, loadings = TRUE,
                                   col = ifelse(shark_labels$wider_category == "Sports, Fitness and Recreation", "blue", "transparent"),
                                   loadings.label = TRUE) + 
  ggtitle("PCA Sports, Fitness and Recreation")

pca_plot_food <- autoplot(pca, data = shark_vars, loadings = TRUE,
                                   col = ifelse(shark_labels$wider_category == "Food and Beverages", "blue", "transparent"),
                                   loadings.label = TRUE) + 
  ggtitle("PCA Food and Beverages")

pca_plot_health <- autoplot(pca, data = shark_vars, loadings = TRUE,
                                   col = ifelse(shark_labels$wider_category == "Health, Beauty and Wellness", "blue", "transparent"),
                                   loadings.label = TRUE) + 
  ggtitle("PCA Health, Beauty and Wellness")

# Combine the plots with layout
combined_pca_plots <- pca_plot_home + pca_plot_baby + pca_plot_professional + pca_plot_apparel + 
  pca_plot_electronics + pca_plot_entertainment + pca_plot_sports + pca_plot_food + pca_plot_health
  plot_layout(nrow = 2, ncol = 2)

# Print the combined plot
print(combined_pca_plots)



pca_plot_baby <- autoplot(pca, data = shark_vars, loadings = TRUE,
                                   col = ifelse(shark_labels$wider_category == "Baby and Children", "blue", "transparent"),
                                   loadings.label = TRUE) + 
  ggtitle("PCA Baby and Children")


print(pca_plot_baby)

#To find the percentage of variance explained in the dataset
pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
print(pve)


####################
###Map of the US####
####################

#Exchange for Stake Map 

install.packages("usmap")
install.packages("ggplot")
library(usmap)
library(ggplot2)

plot_usmap(data = shark_df, values = "exchangeForStake", color = "black") + 
  scale_fill_continuous(name = "exchangeForStake", low = "white", high ="red",label = scales::comma) + 
  labs(title = "U.S. States",subtitle = "This a map of all the states that got an exchange for stake") +
  theme(legend.position = "right")

#Deals per state map 

library(usmap)
library(ggplot2)
library(dplyr)

deals_per_state <- shark_df %>%
  group_by(state) %>%
  summarize(deals_count = sum(exchangeForStake, na.rm = TRUE))

plot_usmap(data = deals_per_state, values = "deals_count", color = "black") + 
  scale_fill_continuous(name = "Deal per State", low = "white", high ="blue", label = scales::comma) + 
  labs(title = "U.S. States", subtitle = "Map of States with Number of Deals") +
  theme(legend.position = "right")

#########################
###Logistic Regression###
#########################
attach(shark_df)
# Convert 'Deal' to a factor with valid level names
shark_vars$Deal <- as.factor(shark_vars$Deal)
levels(shark_vars$Deal) <- make.names(levels(shark_vars$Deal))

# Library of stargazer
library(stargazer)

# Logistic Regression
logit=glm(Deal ~ `Asked For` + `Valuation` + `Exchange for Stake` + `Home and Lifestyle`
          + `Baby and Children` + `Professional Services` + `Apparel and Accessories`
          + `Electronics and Technology` + `Entertainment Industry`
          + `Food and Beverages`+`Health, Beauty, and Wellness` +`West` + `Northeast` + `South` 
          + `Title Length` + `Number of Entrepreneurs` + `Barbara Corcoran` 
          + `Lori Greiner`  + `Mark Cuban` 
          + `Kevin Harrington` ,family='binomial',data = shark_df)

summary(logit)
stargazer(logit, type="html", out="logit_model.html")


#############################
###Prediction for Ventures###
#############################

new_data <- expand.grid(
  `Asked For` = 10000,
  `Valuation` = 40000,
  `Exchange for Stake` = 10,
  `Home and Lifestyle` = 0,
  `Baby and Children` = 0,
  `Professional Services` = 0,
  `Apparel and Accessories` = 0,
  `Electronics and Technology` = 1,
  `Entertainment Industry` = 0,
  `Food and Beverages` = 0,
  `Health, Beauty, and Wellness` = 0,
  `West` = 0,
  `Northeast` = 1,
  `South` = 0,
  `Title Length` = 16,
  `Number of Entrepreneurs` = 1,
  `Barbara Corcoran` = 1,
  `Lori Greiner` = 1,
  `Mark Cuban` = 0,
  `Kevin Harrington` = 1
)


# Make predictions (log-odds)
predict(logit, newdata = new_data, type = "response")

############################
###Train and Test Dataset###
############################
install.packages("caret")
library(caret)

set.seed(123)  # Setting seed for reproducibility
partition <- createDataPartition(shark_df$Deal, p = 0.7, list = FALSE)

training_set <- shark_df[partition, ]
test_set <- shark_df[-partition, ]

print(dim(training_set))
print(dim(test_set))

#Confusion Matrix and Accuracy
predicted_classes <- predict(logit, newdata = test_set, type = "response")
predicted_classes <- ifelse(predicted_classes > 0.5, 1, 0)  # Converting probabilities to class labels

conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_set$Deal))
print(conf_matrix)

# Precision, Recall, and F1 Score
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Recall']
f1_score <- 2 * ((precision * recall) / (precision + recall))

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))



