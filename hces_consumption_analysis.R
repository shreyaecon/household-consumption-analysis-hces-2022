# ========================================
# 1. SETUP AND DATA LOADING
# ========================================

rm(list = ls())
# load libraries
library(rmarkdown)
library(knitr)
library(rnaturalearthhires)
library(sf)         # For shapefile handling
library(rnaturalearth)  # To get India's map
library(rnaturalearthdata)
library(viridis) 
library(forcats)
library(scales)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(gapminder)
library(psych)
library(tidyverse)
library(gcookbook)
library(stargazer)
library(tidyverse)
library(gridExtra)
library(patchwork)
setwd("D:/APU/communicating econ")
library(readxl)

# Load the dataset and select columns "st" and "stn"
StateList <- read_excel("tabulation_state_code.xlsx") %>%
  dplyr::select(st, stn)
StateList$st <- as.numeric(StateList$st)

# Load Level 3 data
hces_3 <- read_dta("hces22_lvl_3.dta")
hces_3$state<-as.numeric(hces_3$state)

# Load Level 14 data
hces_14 <- read_dta("hces22_lvl_14.dta")
hces_14$state<-as.numeric(hces_14$state)

# Load Level 15 data
hces_15 <- read_dta("hces22_lvl_15.dta")
hces_15$state<-as.numeric(hces_15$state)
hces_3_new<- hces_3 %>% 
  mutate(Weights=mult/100,
         HHID = paste0(fsu,b1q1pt11,b1q1pt12),
         StateName=factor(state,
                          levels= StateList$st,
                          labels=StateList$stn))
hces_14_new<- hces_14 %>% 
  mutate(Weights=mult/100,
         HHID = paste0(fsu,b1q1pt11,b1q1pt12),
         StateName=factor(state,
                          levels= StateList$st,
                          labels=StateList$stn))
hces_15_new<- hces_15 %>% 
  mutate(Weights=mult/100,
         HHID = paste0(fsu,b1q1pt11,b1q1pt12),
         StateName=factor(state,
                          levels= StateList$st,
                          labels=StateList$stn))
# ========================================
# 2. RURAL VS URBAN EXPENDITURE ANALYSIS
# ========================================
# Subset for Questionnaire F (Food)
FoodSummary <- hces_14_new %>% filter(questionaire_no == "F")

# Subset for Questionnaire C (Consumables)
ConsumablesSummary <- hces_14_new %>% filter(questionaire_no == "C")

# Subset for Questionnaire D (Durables)
DurablesSummary <- hces_14_new %>% filter(questionaire_no == "D")

Food1<-FoodSummary %>% 
  filter(ba1b1c1_2 %in% c(129,139,159,179)) %>% 
  group_by(HHID) %>% 
  summarise(
    SumF1=sum(ba1b1c1_3)
  )
Food2<-FoodSummary %>% 
  filter(ba1b1c1_2 %in% c(169,219,239,249,199,189,269,279,289,299)) %>% 
  group_by(HHID) %>% 
  summarise(
    SumF2=sum(ba1b1c1_3)*(30/7)
  )
Food <- full_join(Food1, Food2, by = "HHID") %>% 
  mutate(
    Sum1=replace_na(SumF1,0),
    Sum2=replace_na(SumF2,0),
    FoodExpense= Sum1+Sum2
  ) %>% 
  select(HHID,FoodExpense)
Consumable2<-ConsumablesSummary %>% 
  filter(ba1b1c1_2 %in% c(349,459,479,429,519,499,439,529)) %>% 
  group_by(HHID) %>% 
  summarise(
    SumC2=sum(ba1b1c1_3),
  )
Consumable1<-ConsumablesSummary %>% 
  filter(ba1b1c1_2 %in% c(309,319,329)) %>% 
  group_by(HHID) %>% 
  summarise(
    SumC1=sum(ba1b1c1_3)*(30/7)
  )
Consumable3<-ConsumablesSummary %>% 
  filter(ba1b1c1_2 %in% c(409,419,899)) %>% 
  group_by(HHID) %>% 
  summarise(
    SumC3=sum(ba1b1c1_3)*(30/365)
  )
Consumables <- reduce(list(Consumable1, Consumable2, Consumable3), full_join, by = "HHID") %>% 
  mutate(
    SumC1=replace_na(SumC1,0),
    SumC2=replace_na(SumC2,0),
    SumC3=replace_na(SumC3,0),
    ConsumablesExpense= SumC1+SumC2+SumC3
  ) %>% 
  select(HHID,ConsumablesExpense)
Durables<-DurablesSummary %>% 
  group_by(HHID) %>% 
  summarise(
    DurablesExpense=sum(ba1b1c1_3)*(30/365)
  )
AllExpenses<- Food %>% full_join(Consumables) %>% full_join(Durables)
AllExpenses[is.na(AllExpenses)]<-0
FoodHH <- hces_15_new %>%
  filter(questionaire_no == "F") %>%
  select(HHID, ba2b2c2q9)
ConsumablesHH <- hces_15_new %>%
  filter(questionaire_no == "C") %>%
  select(HHID, ba2b2c2q9)
DurablesHH <- hces_15_new %>%
  filter(questionaire_no == "D") %>%
  select(HHID, ba2b2c2q9)

Additional <- hces_15_new %>%
  filter(questionaire_no == "F") %>%
  select(HHID, Weights, sector, state, StateName)
AdditionalInfo <- Additional %>%
  full_join(FoodHH, by = "HHID") %>%
  rename(Household.Size.F = ba2b2c2q9)
AdditionalInfo <- AdditionalInfo %>%
  full_join(ConsumablesHH, by = "HHID") %>%
  rename(Household.Size.C = ba2b2c2q9)
AdditionalInfo <- AdditionalInfo %>%
  full_join(DurablesHH, by = "HHID") %>%
  rename(Household.Size.D = ba2b2c2q9)
AdditionalInfo<- AdditionalInfo %>% 
  full_join(hces_3_new, by= "HHID") %>% 
  rename(Household.Size.FDQ =b2q2pt1)
Results <- AllExpenses %>%
  full_join(AdditionalInfo, by = "HHID")
Results <- Results %>%
  mutate(
    FoodExpense = as.numeric(FoodExpense),
    ConsumablesExpense = as.numeric(ConsumablesExpense),
    DurablesExpense = as.numeric(DurablesExpense),
    Household.Size.F = as.numeric(Household.Size.F),
    Household.Size.C = as.numeric(Household.Size.C),
    Household.Size.D = as.numeric(Household.Size.D)
  )

Results <- Results %>%
  mutate(
    TE = FoodExpense + (ConsumablesExpense / Household.Size.C) * Household.Size.F +
      (DurablesExpense / Household.Size.D) * Household.Size.F,
    
    MPCE = TE / Household.Size.F
  )

sum(Results$TE*Results$Weights.x)/sum(Results$Household.Size.F*Results$Weights.x)

summary_data_1 <- Results %>%
  group_by(sector.x) %>%
  summarise(
    Monthly_per_capita_Consumption_Expenditure = sum(TE * Weights.x) / sum(Household.Size.F * Weights.x),
    Monthly_HH_Consumption_Expenditure = sum(TE * Weights.x) / sum(Weights.x)
  )

# Convert sector values to labels
summary_data_1$sector.x <- factor(summary_data_1$sector.x, levels = c(1, 2), labels = c("Rural", "Urban"), ordered = TRUE)
# Plot Monthly Household Consumption Expenditure
p1 <- ggplot(summary_data_1, aes(x = fct_rev(factor(sector.x)), y = Monthly_HH_Consumption_Expenditure, fill = sector.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0("₹", comma(round(Monthly_HH_Consumption_Expenditure)))),
            vjust = 0.5, hjust = 1.2, size = 5, color = "white", fontface = "bold") +
  coord_flip() +  # Flip for horizontal bars
  scale_fill_manual(values = c("#1b4f72", "#154360")) +
  labs(title = "Monthly Household Consumption Expenditure (mean)",
       x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none"
        )

# Plot Monthly Per Capita Consumption Expenditure with Caption
p2 <- ggplot(summary_data_1, aes(x = fct_rev(factor(sector.x)), y = Monthly_per_capita_Consumption_Expenditure, fill = sector.x)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0("₹", comma(round(Monthly_per_capita_Consumption_Expenditure)))),
            vjust = 0.5, hjust = 1.2, size = 5, color = "white", fontface = "bold") +
  coord_flip() +  # Flip for horizontal bars
  scale_y_continuous(limits=c(0,25000), labels=scales::comma) +
  scale_fill_manual(values = c("#1b4f72", "#154360")) +
  labs(title = "Monthly Per Capita Consumption Expenditure (mean)",
       x = "", y = "")+
  theme_minimal() +
  theme(legend.position = "none")


# Combine both plots
combined_plot <- (p1 / p2) +
  plot_annotation(
    title = "Household and Per Capita Monthly Expenditure (2023)",
    subtitle = "This includes spending on all goods and services, from food to rent, as reported by survey respondents",
    caption = "Excludes income tax, insurance premium payments, purchase or construction of land and real estate, charity and gifts\n\n*Source: NSS Round 79, Household Consumption Expenditure Survey (2022-23), National Sample Survey Office*"
  ) &
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 12, color = "gray30"),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray40", face = "italic")
  )
  

# Display the final plot
combined_plot

######################################################################################
# ========================================
# 3. QUINTILE ANALYSIS
# ========================================
# Compute quintiles separately for each sector
Results_quintile_final <- Results %>%
  group_by(sector.x) %>%  # Group by sector
  mutate(Quintile = cut(MPCE,
                        breaks = quantile(MPCE, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE),
                        labels = c("Lowest Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Highest Quintile"),
                        include.lowest = TRUE)) %>%
  ungroup()  

# Compute weighted average MPCE for each quintile
Results_quintile_final <- Results_quintile_final %>%
  group_by(sector.x, Quintile) %>%
  summarise(
    Average_MPCE = sum(TE * Weights.x, na.rm = TRUE) / sum(Household.Size.F * Weights.x, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(sector.x = case_when(
    sector.x == 1 ~ "Rural",
    sector.x == 2 ~ "Urban",
    TRUE ~ as.character(sector.x)
  ))

# Reverse factor levels so Lowest Quintile appears at the top
Results_quintile_final$Quintile <- factor(
  Results_quintile_final$Quintile, 
  levels = rev(c("Lowest Quintile", "Second Quintile", "Middle Quintile", "Fourth Quintile", "Highest Quintile"))  
)
print(Results_quintile_final)
# Define colors for Rural and Urban
color_map <- c("Rural" = "#10375c", "Urban" = "#f4a261")  # Dark blue and orange

# Create the plot
ggplot(Results_quintile_final, aes(x = Average_MPCE, y = Quintile, color = sector.x)) +
  geom_point(size = 4) +  # Large dots
  geom_text(aes(label = scales::comma(Average_MPCE, prefix = "₹")),  # Add labels near points
            vjust = -1, size = 3.5, color = "black") +
  scale_color_manual(values = color_map) +  # Custom colors for Rural/Urban
  scale_x_continuous(labels = scales::comma_format(prefix = "₹")) +  # Format x-axis labels
  theme_minimal() +
  labs(
    title = "Monthly per capita expenditure among consumption classes",
    subtitle = "People in the top 20% of urban India spend nearly five times every month as those in the bottom 20%",
    caption = "Each quintile represents a fifth of the population. Numbers represent average MPCE in that quintile.\nSource: NSS Round 79, Household Consumption Expenditure Survey (2022-23), National Sample Survey Office"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),  # Left-align caption
    legend.position = "top",  # Place legend at top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10),
    axis.title = element_blank(),  # Remove axis titles
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, face = "bold")
  )

###########################################################
Results_1 <- Results %>%
  filter(b4q4pt12 != 0) %>%  # Remove cases where b4q4pt12 is 0
  mutate(
    Social_group = case_when(
      b4q4pt12 == 1 ~ "Scheduled Tribes (ST)",
      b4q4pt12 == 2 ~ "Scheduled Castes (SC)",
      b4q4pt12 == 3 ~ "Other Backward Classes (OBC)",
      b4q4pt12 == 9 ~ "Others (General)"
    )
  ) %>%
  group_by(Social_group) %>%
  summarise(
    Average_MPCE = sum(TE * Weights.x, na.rm = TRUE) / sum(Household.Size.F * Weights.x, na.rm = TRUE)
  )
Results_2 <- Results %>%
  filter(!b4q4pt11 %in% c(0, 7, 9)) %>%  # Exclude 0, 7, and 9
  mutate(
    Religion = case_when(
      b4q4pt11 == 1 ~ "Hinduism",
      b4q4pt11 == 2 ~ "Islam",
      b4q4pt11 == 3 ~ "Christianity",
      b4q4pt11 == 4 ~ "Sikhism",
      b4q4pt11 == 5 ~ "Jainism",
      b4q4pt11 == 6 ~ "Buddhism"
    )
  ) %>%
  group_by(Religion) %>%
  summarise(
    Average_MPCE = sum(TE * Weights.x, na.rm = TRUE) / sum(Household.Size.F * Weights.x, na.rm = TRUE)
  )
# Combine both datasets for plotting
Results_1 <- Results_1 %>% mutate(Category = "Social_Group")
Results_2<- Results_2 %>% mutate(Category = "Religion")

Results_1_combined <- bind_rows(
  rename(Results_1, Group = Social_group),
  rename(Results_2, Group = Religion)
)

Results_1_combined$Category <- factor(Results_1_combined$Category, levels = c("Social_Group", "Religion"))
print(Results_1_combined)
#plot using ggplot2
ggplot(Results_1_combined, aes(x = reorder(Group, Average_MPCE), y = Average_MPCE, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0("₹", comma(round(Average_MPCE)))),
            vjust = 0.5, hjust = 0, size = 4, color = "white", fontface = "bold") +
  coord_flip() +
  facet_wrap(~Category, scales = "free_y") +
  scale_fill_manual(values = c("#0E4D92", "#0E4D91")) +
  labs(
    title = "Average monthly per capita expenditure by social group and religion (2023)",
    x = "", y = "",
    caption = "Includes spending on all goods and services, from food to rent, as reported by survey respondents. \nExcludes income tax, insurance premium payments, purchase or construction of land and real estate, charity and gifts. \nSource: NSS Round 79, Household Consumption Expenditure Survey (2022-23), National Sample Survey Office"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0), # Align title left
    plot.title.position = "plot", # Ensure title follows margin alignment
    plot.margin = margin(l = 5, t = 10, r = 10, b = 10), # Reduce left margin
    strip.text = element_text(size = 14, face = "bold"),  # Facet labels styling
    strip.text.x = element_text(hjust = 0),  # Align facet labels left
    plot.caption = element_text(size = 10, hjust = 0, face = "italic") # Align caption left
  )


###################################################################################
MPCE_by_state <- Results %>%
  group_by(state.x, StateName.x) %>%
  summarise(
    Mean_MPCE = sum(TE * Weights.x) / sum(Household.Size.F * Weights.x),
    .groups = 'drop'
  ) %>%
  rename(state = state.x, StateName = StateName.x)

india_map <- st_read("in.shp")

state_codes_manual <- data.frame(
  name = c("Andaman and Nicobar", "Telangana", "Andhra Pradesh", "Arunachal Pradesh",
           "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
           "Dādra and Nagar Haveli and Damān and Diu", "Delhi", "Goa", "Gujarat",
           "Haryana", "Himachal Pradesh", "Jharkhand", "Karnataka",
           "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur",
           "Meghalaya", "Mizoram", "Nagaland", "Orissa",
           "Puducherry", "Punjab", "Rajasthan", "Sikkim",
           "Tamil Nadu", "Tripura", "Uttar Pradesh", "Uttaranchal",
           "West Bengal", "Lakshadweep", "Jammu and Kashmir", "Ladakh"),
  state = c(35, 36, 28, 12,
            18, 10, 4, 22,
            25, 7, 30, 24,
            6, 2, 20, 29,
            32, 23, 27, 14,
            17, 15, 13, 21,
            34, 3, 8, 11,
            33, 16, 9, 5,
            19, 31, 1, 37))

india_map <- india_map %>%
  left_join(state_codes_manual, by = "name")

map_data <- left_join(india_map, MPCE_by_state)
#
ggplot(map_data) +
  geom_sf(aes(fill = Mean_MPCE)) +
  scale_fill_gradient(
    low = "#e3eef9",   # Light blue
    high = "#10375c",  # Dark blue
    name = "Monthly Per Capita\nExpenditure",
    labels = scales::comma
  ) +
  theme_minimal() +
  labs(title = "Average Monthly Per Capita Expenditure by State (2023)") +
  theme(
    plot.title = element_text(size = 16)
  )

#plot bonus question
ggplot(map_data) +
  geom_sf(aes(fill = Mean_MPCE)) +
  scale_fill_gradient(
    low = "#e3eef9",   
    high = "#10375c",  
    limits = c(2000, 7000),  
    oob = scales::squish,    
    labels = scales::comma,
    guide = guide_colorbar(
      title = NULL,        
      barwidth = 12,       # Reduce width to avoid overlapping
      barheight = 0.6      # Slightly thicker
    )
  ) +
  theme_minimal() +
  labs(
    title = "Average Monthly Per Capita Expenditure by State (2023)",
    subtitle = "Household spending is the lowest in the central parts of the country",
    caption = "Includes spending on all goods and services, from food to rent, as reported by survey respondents.\nExcludes income tax, insurance premium payments, purchase or construction of land and real estate, charity and gift. \nSource: NSS Round 79, Household Consumption Expenditure Survey (2022-23), NSSO"
  ) +
  theme(
    plot.title = element_text(size = 16),
    legend.position = c(0.8, 0.85),  # Move slightly lower
    legend.direction = "horizontal",
    legend.title = element_blank(),  
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),        
    axis.ticks = element_blank(),        
    axis.title = element_blank()
  )
    