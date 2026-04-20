# Loading Necessary Libraries 
library(arules) 
library(arulesViz) 
library(tidyverse) 
library(readr)

# Reading and Inspecting the FAOSTAT Dataset 
FAOSTAT <- read_csv("C:/Users/User/Downloads/FAOSTATCrops and livestock 
products.csv") 
head(FAOSTAT) 
str(FAOSTAT) 
summary(FAOSTAT)

#Data Cleaning and Selection of Relevant Columns 
selected_FAOSTAT <- FAOSTAT %>% 
  select(Area, Item, Element) %>% 
  na.omit() %>% 
  distinct() 

# Transforming Data into a Transactions Format 
transactions <- as(split(selected_FAOSTAT$Item, selected_FAOSTAT$Area), 
                   "transactions") 

# Exploring Transaction Characteristics 
summary(transactions) 
inspect(transactions[1:5])

#Filtering Infrequent Items 
item_freq <- itemFrequency(transactions, type = "absolute") 
items_to_keep <- names(item_freq[item_freq >= 3]) 
filtered_transactions <- transactions[, colnames(transactions) %in% items_to_keep] 

#Generating Association Rules Using the Apriori Algorithm 
rules <- apriori( 
  filtered_transactions, 
  parameter = list(supp = 0.05, conf = 0.5, minlen = 2, maxlen = 3) 
)


# Eliminating Redundant Rules 
rules <- rules[!is.redundant(rules)] 

#Sorting Rules by Importance Metrics 
rules_sorted <- sort(rules, by = c("confidence", "lift"), decreasing = TRUE)

#Viewing the Top Rules 
inspect(head(rules_sorted, 10)) 

#Visualizing Rules with a Graph 
top_rules <- head(rules_sorted, 20) 
plot(top_rules, method = "graph", control = list(type = "items")) 

