# Import CSV file into R using the specified local path
ev_data <- read.csv("C:/Users/ganka/OneDrive/Desktop/Bsc. Project/A SURVEY ON ELECTRIC VEHICLES _ Analysis of consumer behaviour towards buying of electric vehicles (Responses) - Form Responses 1.csv", 
                    header = TRUE)

# Preview the first few rows of the dataset
head(ev_data)

# Check the structure of the dataset: shows column names and data types
str(ev_data)

# Get a summary of each column: useful for quick insights
summary(ev_data)

#Checking the name of the first column
names(ev_data)[1]  # Shows the name of the first column

#Dropping the first column 
ev_data <- ev_data[, -1]

#Renaming column names
key = colnames(ev_data)
colnames(ev_data) = paste0("x",1:36)

# Convert nominal (unordered categorical) variables to factors
ev_data[,3]  <- factor(ev_data[,3])   # Gender
ev_data[,4]  <- factor(ev_data[,4])   # Occupation
ev_data[,5]  <- factor(ev_data[,5])   # Education
ev_data[,6]  <- factor(ev_data[,6])   # Locality
ev_data[,8]  <- factor(ev_data[,8])   # Pay tax or not
ev_data[,10] <- factor(ev_data[,10])  # Preferred vehicle type
ev_data[,12] <- factor(ev_data[,12])  # EV ownership status
ev_data[,26] <- factor(ev_data[,26])  # Purchase intention (Yes/No/Maybe)
ev_data[,27] <- factor(ev_data[,27])  # Safety concern
ev_data[,28] <- factor(ev_data[,28])  # Environmental concern
ev_data[,35] <- factor(ev_data[,35])  # Interest in test ride

# Convert ordinal (ranked categorical) variables to ordered factors
ev_data[,2] <- factor(ev_data[,2],
                      levels = c("below 18 years", "18-22", "23-30", "above 30"),
                      ordered = TRUE)  # Age group

ev_data[,7] <- factor(ev_data[,7],
                      levels = c("Less than 2 Lakh", "2Lakh-8Lakh", "9Lakh-15Lakh", "Above 15 Lakh"),
                      ordered = TRUE)  # Income range

ev_data[,13] <- factor(ev_data[,13],
                       levels = c("Less than 5 years", "5 - 10 years", "Above 10 years",
                                  "I didn't use / own an electric vehicle"),
                       ordered = TRUE)  # EV experience duration

ev_data[,17] <- factor(ev_data[,17],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Opinion on EV performance

ev_data[,18] <- factor(ev_data[,18],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Awareness of EV benefits

ev_data[,19] <- factor(ev_data[,19],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Belief in future of EVs

ev_data[,20] <- factor(ev_data[,20],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Concern about charging infrastructure

ev_data[,21] <- factor(ev_data[,21],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Support for govt incentives

ev_data[,22] <- factor(ev_data[,22],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Environmental impact opinion

ev_data[,23] <- factor(ev_data[,23],
                       levels = c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree"),
                       ordered = TRUE)  # Trust in EV safety

ev_data[,29] <- factor(ev_data[,29],
                       levels = c("I don't want to buy an electric vehicle",
                                  "I want to buy an electric vehicle during next 10 years",
                                  "I want to buy an electric vehicle during next 5 years",
                                  "I want to buy an electric vehicle as soon as possible"),
                       ordered = TRUE)  # EV purchase timeline

# Converting multi-choice responses to binary matrix
mat = function(x,vals){
  matx = matrix(0,nrow = length(x),ncol = length(vals))
  for(i in 1:length(x)){
    a = strsplit(x[i],split=", ")[[1]]
    matx[i,match(a,vals)] = 1
  }
  matx
}

# Vehicles owned/used
v.hh <- c("Electric Vehicle", "CNG / Fuel Vehicle", "Pedal - driven Vehicle")
mat.v.hh <- mat(x = as.character(ev_data[,9]), vals = v.hh)
colnames(mat.v.hh) <- v.hh

# Electric vehicles currently owned
ev.h <- c("E - Bikes", "Electric Scooty", "Electric Car", "Electric trikes / E - rickshaws")
f.med <- mat(x = as.character(ev_data[,14]), vals = ev.h)
colnames(f.med) <- ev.h

# Electric vehicles respondents want to buy
ev.buy <- c("E - Bikes", "Electric Scooty", "Electric Car", "Electric trikes / E - rickshaws")
mat.ev.buy <- mat(x = as.character(ev_data[,15]), vals = ev.buy)
colnames(mat.ev.buy) <- ev.buy

# Source of EV knowledge
knowledge <- c("Newspaper", "Television advertisement", "Social Media ads", "Friends", "Hoardings and poster")
mat.knowledge <- mat(x = as.character(ev_data[,16]), vals = knowledge)
colnames(mat.knowledge) <- knowledge

# Tips to increase mileage
mileage <- c("Increasing power of battery", "Reducing the weight of the vehicle",
             "Not applying the breaks very frequently", 
             "Electric cars are most efficient when driven at around 50-60 mph")
mat.mileage <- mat(x = as.character(ev_data[,24]), vals = mileage)
colnames(mat.mileage) <- mileage

# Encouraging factors for EV
encouraging <- c("Price", "Positive environmental effect", "New trends",
                 "Beneficial financial or insurance option", "Low noise level",
                 "Promotion", "References", "Petrol price hikes")
mat.encouraging <- mat(x = as.character(ev_data[,30]), vals = encouraging)
colnames(mat.encouraging) <- encouraging

# Discouraging factors
discouraging <- c("Long recharging time", "Lack of charging infrastructure", "Price",
                  "Lack of consumer choice", "Lack of trust to new technologies",
                  "Price of electricity", "Mileage of vehicle", "Lack of skilled workforce")
mat.discouraging <- mat(x = as.character(ev_data[,31]), vals = discouraging)
colnames(mat.discouraging) <- discouraging

# EV companies owned
havec <- c("Tata", "Hyundai", "Mahindra", "Revolt", "Hero", "TVS", "Other")
mat.havec <- mat(x = as.character(ev_data[,32]), vals = havec)
colnames(mat.havec) <- havec

# EV companies preferred for future purchase
wantsc <- c("Tata", "Hyundai", "Mahindra", "Revolt", "Hero", "TVS", "Other")
mat.wantsc <- mat(x = as.character(ev_data[,33]), vals = wantsc)
colnames(mat.wantsc) <- wantsc

# EV companies recommended to others
suggestc <- c("Tata", "Hyundai", "Mahindra", "Revolt", "Hero", "TVS", "Other")
mat.suggestc <- mat(x = as.character(ev_data[,34]), vals = suggestc)
colnames(mat.suggestc) <- suggestc

# Summary of the full dataset
summary(ev_data)

# Summary of Male responses
summary(subset(ev_data, ev_data[,3] == "Male"))

# Summary of Female responses
summary(subset(ev_data, ev_data[,3] == "Female"))

# Function to detect outliers using IQR
detect_outliers_iqr <- function(column) {
  if (!is.numeric(column)) return(rep(FALSE, length(column)))
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(column < lower_bound | column > upper_bound)
}

# Function to replace outliers with NA instead of dropping rows
mark_outliers_as_na <- function(data) {
  numeric_columns <- sapply(data, is.numeric)
  data_clean <- data
  for (colname in names(data)[numeric_columns]) {
    outliers <- detect_outliers_iqr(data[[colname]])
    data_clean[[colname]][outliers] <- NA
  }
  return(data_clean)
}

# Apply outlier masking
ev_data_outliers_na <- mark_outliers_as_na(ev_data)

# Function to impute missing values with column mean
impute_with_mean <- function(column) {
  if (is.numeric(column)) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

# Apply mean imputation to numeric columns
numeric_columns <- sapply(ev_data_outliers_na, is.numeric)
ev_data_imputed <- ev_data_outliers_na
ev_data_imputed[numeric_columns] <- lapply(ev_data_outliers_na[numeric_columns], impute_with_mean)

#-----------------------------------------------------------------

#EXPLANATORY DATA ANALYSIS
#DIAGRAMMATIC REPRESENTATION

library(ggplot2)

#PIE CHART PLOTTING 
# Convert column 3 to factor and create a frequency table
ev_data$Gender <- as.factor(ev_data[,3])
gender_df <- as.data.frame(table(ev_data$Gender))
colnames(gender_df) <- c("Gender", "Freq")

# Add percentage and label text
gender_df$Percent <- round(100 * gender_df$Freq / sum(gender_df$Freq), 1)
gender_df$Label <- paste0(gender_df$Gender, "\n", gender_df$Freq, " (", gender_df$Percent, "%)")

# Plot the pie chart with named colors
ggplot(gender_df, aes(x = "", y = Freq, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Gender Distribution of Respondents", fill = "Gender") +
  theme_void() +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Male" = "darkred", "Female" = "darkgreen"))

# Create frequency table from column 6 — assuming it represents Locality
ev_data$Locality <- as.factor(ev_data[,6])
locality_df <- as.data.frame(table(ev_data$Locality))
colnames(locality_df) <- c("Locality", "Freq")

# Add percentages and formatted label text
locality_df$Percent <- round(100 * locality_df$Freq / sum(locality_df$Freq), 1)
locality_df$Label <- paste0(locality_df$Locality, "\n", locality_df$Freq, " (", locality_df$Percent, "%)")

# Plot pie chart with named colors
ggplot(locality_df, aes(x = "", y = Freq, fill = Locality)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "   Residential Area Distribution", fill = "Locality") +
  theme_void() +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Urban" = "red", "Rural" = "skyblue"))

library(ggplot2)
library(dplyr)

# Income data from your earlier input
income_df <- data.frame(
  Income = c("Less than 2 Lakh", "2Lakh-8Lakh", "9Lakh-15Lakh", "Above 15 Lakh"),
  Freq = c(145, 175, 104, 24)
)

# Add percentage and label text
income_df <- income_df %>%
  mutate(Percent = round(100 * Freq / sum(Freq), 1),
         SliceLabel = paste0(Percent, "%"),  # Only percentage goes inside pie
         LegendLabel = paste0(Income, ": ", Freq, " responses (", Percent, "%)"))

# Create named fill scale using your custom side labels
names(income_df$LegendLabel) <- income_df$Income  # Map labels to fill groups

ggplot(income_df, aes(x = "", y = Freq, fill = Income)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Pie Chart for Family's Annual Income", fill = "Income Group") +
  geom_text(aes(label = SliceLabel), position = position_stack(vjust = 0.5), size = 4.5) +
  scale_fill_manual(
    values = c("Less than 2 Lakh" = "darkgreen",
               "2Lakh-8Lakh" = "blue",
               "9Lakh-15Lakh" = "red",
               "Above 15 Lakh" = "yellow"),
    labels = income_df$LegendLabel
  )

# Pie chart for educational qualification
education_df <- data.frame(
  Education = c("Diploma", "Graduate", "Higher Secondary", "Matriculate", "Other", "Post Graduate"),
  Freq = c(16, 172, 133, 52, 26, 49)
)

# Add percentage and label formatting
education_df <- education_df %>%
  mutate(
    Percent = round(100 * Freq / sum(Freq), 1),
    SliceLabel = paste0(Percent, "%"),
    LegendLabel = paste0(Education, ": ", Freq, " responses (", Percent, "%)")
  )

# Define custom colors matching your style
custom_colors <- c(
  "Diploma" = "skyblue",
  "Graduate" = "gold",
  "Higher Secondary" = "coral",
  "Matriculate" = "darkred",
  "Other" = "violet",
  "Post Graduate" = "darkgreen"
)

# Final pie chart
ggplot(education_df, aes(x = "", y = Freq, fill = Education)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Pie Chart for Educational Qualification", fill = "Education Level") +
  geom_text(aes(label = SliceLabel), position = position_stack(vjust = 0.5), size = 4.5) +
  scale_fill_manual(values = custom_colors, labels = education_df$LegendLabel)

# pie chart for EV ownership data
ev_own_df <- data.frame(
  Ownership = c("Yes", "No"),
  Freq = c(218, 230)
)

# Add percentages and labels
ev_own_df <- ev_own_df %>%
  mutate(
    Percent = round(100 * Freq / sum(Freq), 1),
    SliceLabel = paste0(Percent, "%"),
    LegendLabel = paste0(Ownership, ": ", Freq, " responses (", Percent, "%)")
  )

# Pie chart code
ggplot(ev_own_df, aes(x = "", y = Freq, fill = Ownership)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Pie Chart for Number of People Who Own an EV", fill = "EV Ownership") +
  geom_text(aes(label = SliceLabel), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkblue"), labels = ev_own_df$LegendLabel)

# pie chart for purchase intensions
ev_intent_df <- data.frame(
  Intent = c(
    "I want to buy an electric vehicle as soon as possible",
    "I want to buy an electric vehicle during next 5 years",
    "I want to buy an electric vehicle during next 10 years",
    "I don't want to buy an electric vehicle"
  ),
  Freq = c(117, 193, 92, 46)
)

# Add percentages and labels
ev_intent_df <- ev_intent_df %>%
  mutate(
    Percent = round(100 * Freq / sum(Freq), 1),
    SliceLabel = paste0(Percent, "%"),
    LegendLabel = paste0(Intent, ": ", Freq, " responses (", Percent, "%)")
  )

# Define matching colors
custom_colors <- c(
  "I want to buy an electric vehicle as soon as possible" = "red",
  "I want to buy an electric vehicle during next 5 years" = "yellow",
  "I want to buy an electric vehicle during next 10 years" = "green",
  "I don't want to buy an electric vehicle" = "skyblue"
)

# Generate pie chart
ggplot(ev_intent_df, aes(x = "", y = Freq, fill = Intent)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Pie Chart for Next Vehicle will be EV", fill = "Purchase Intention") +
  geom_text(aes(label = SliceLabel), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = custom_colors, labels = ev_intent_df$LegendLabel)

# Create response data based on your chart
switch_df <- data.frame(
  Response = c("Yes", "No", "Maybe"),
  Freq = c(288, 42, 118)
)

# Add percentage and label formatting
switch_df <- switch_df %>%
  mutate(
    Percent = round(100 * Freq / sum(Freq), 1),
    SliceLabel = paste0(Percent, "%"),
    LegendLabel = paste0(Response, ": ", Freq, " responses (", Percent, "%)")
  )

# Define custom colors matching your image
custom_colors <- c(
  "Yes" = "yellow",
  "No" = "red",
  "Maybe" = "pink"
)

# Generate pie chart
ggplot(switch_df, aes(x = "", y = Freq, fill = Response)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Pie Chart for Switch to EV", fill = "Response") +
  geom_text(aes(label = SliceLabel), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = custom_colors, labels = switch_df$LegendLabel)


# BAR PLOT 
# Plotting Age Groups Vs. Gender 

library(ggplot2)

# Step 1: Create the dataset
age_gender_data <- data.frame(
  AgeGroup = rep(c("Below 18", "18-22", "23-30", "Above 30"), each = 2),
  Gender = rep(c("Female", "Male"), times = 4),
  Count = c(15, 20, 87, 79, 28, 82, 52, 85)
)

# Step 2: Build the plot with labels on top of bars
ggplot(age_gender_data, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4) +  # Adjust vertical position and size
  scale_fill_manual(values = c("Female" = "deeppink", "Male" = "dodgerblue")) +
  labs(title = "Respondents by Age Group and Gender",
       x = "Age Group",
       y = "Number of Respondents",
       fill = "Gender") +
  theme_minimal(base_size = 14)

# Plotting bar chart for occupation
# Step 1: Frequency table from column 4
occupation_table <- table(ev_data[,4])

# Step 2: Bar chart with horizontal x-axis labels
barplot(occupation_table,
        names.arg = names(occupation_table),
        main = "Respondent Count by Occupation",
        xlab = "Occupation",
        ylab = "Number of Respondents",
        col = "lightgreen",
        las = 1,  # Sets labels horizontally
        ylim = c(0, max(occupation_table) + 15))

# Step 3: Add count labels above each bar
text(x = seq_along(occupation_table),
     y = occupation_table,
     labels = occupation_table,
     pos = 3, cex = 0.9)

# Plotting bar chart for taxes
# Step 1: Create frequency table
tax_table <- table(ev_data[,8])

# Step 2: Plot the bar chart
barplot(tax_table,
        main = "Respondents' Views on Taxes",
        xlab = "Tax Opinion",
        ylab = "Number of Respondents",
        col = "steelblue",
        las = 2,
        ylim = c(0, max(tax_table) + 35))  # Gives extra space for labels

# Step 3: Add labels above bars
text(x = seq_along(tax_table),
     y = tax_table,
     labels = tax_table,
     pos = 3, cex = 0.9)

# Plotting bar chart of vehicles have/had
library(ggplot2)
# Step 1: Prepare the frequency data
vehicle_table <- as.data.frame(table(ev_data[,9]))
colnames(vehicle_table) <- c("Vehicle_Type", "Count")

# Step 2: Assign factor levels for consistent color order
vehicle_table$Vehicle_Type <- factor(vehicle_table$Vehicle_Type)

# Step 3: Plot using ggplot2 with a legend instead of x-axis labels
ggplot(vehicle_table, aes(x = Vehicle_Type, y = Count, fill = Vehicle_Type)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  labs(title = "Type of Vehicles Have/Had",
       x = NULL, y = "Number of Respondents", fill = "Vehicle Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),        # Remove x-axis labels
    axis.ticks.x = element_blank(),       # Remove x-axis ticks
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

#Plotting preference of vehicles
# Step 1: Create frequency table from column 10
vehicle_pref <- table(ev_data[,10])

# Step 2: Plot the bar chart with light purple color
barplot(vehicle_pref,
        main = "Vehicle Preferences of Respondents",
        xlab = "Preferred Vehicle Type",
        ylab = "Number of Respondents",
        col = "plum",         
        las = 1,                # horizontal x-axis labels
        ylim = c(0, max(vehicle_pref) + 30))  # Extra space for labels

# Step 3: Add count labels above bars
text(x = seq_along(vehicle_pref),
     y = vehicle_pref,
     labels = vehicle_pref,
     pos = 3, cex = 0.9)

#Plotting bar plot of gender vs. preference of type of vehicles
# Step 1: Create a frequency table by gender and vehicle preference
gender_vehicle_table <- table(ev_data[,3], ev_data[,10])
# Step 2: Create the grouped bar chart
barplot(gender_vehicle_table,
        beside = TRUE,
        col = c("thistle", "forestgreen"),     # Feminine mauve for Gender groups
        legend.text = rownames(gender_vehicle_table),
        args.legend = list(title = "Gender", x = "topright", cex = 0.8),
        main = "Gender vs Preference of Type of Vehicle",
        xlab = "Type of Vehicle",
        ylab = "Number of Respondents",
        ylim = c(0, max(gender_vehicle_table) + 20),
        las = 1)   # Vertical axis labels for better readability
# Step 3: Add count labels above the bars
counts <- as.vector(gender_vehicle_table)
positions <- barplot(gender_vehicle_table, beside = TRUE, plot = FALSE)
text(x = positions,
     y = counts,
     labels = counts,
     pos = 3, cex = 0.8)

# Plotting bar chart for knowledge about EV
# Step 1: Create frequency table
ev_knowledge <- table(ev_data[,11])
# Step 2: Plot bar chart and capture bar midpoints
bar_midpoints <- barplot(ev_knowledge,
                         main = "Respondents' Knowledge About Electric Vehicles",
                         xlab = "Knowledge Level",
                         ylab = "Number of Respondents",
                         col = "skyblue",
                         las = 1,
                         ylim = c(0, max(ev_knowledge) + 15))
# Step 3: Add centered count labels above bars
text(x = bar_midpoints,
     y = ev_knowledge,
     labels = ev_knowledge,
     pos = 3,
     cex = 0.9)

#Plotting bar chart for opinion about EV for reduction on global warming
# Step 1: Create frequency table
ev_opinion <- table(ev_data[,17])
# Step 2: Plot the bar chart and store bar midpoints
bar_midpoints <- barplot(ev_opinion,
                         main = "Opinion on EVs Reducing Global Warming",
                         xlab = "Opinion",
                         ylab = "Number of Respondents",
                         col = "lightseagreen",     # Calm and earthy tone
                         las = 1,                   # horizontal x-axis labels
                         ylim = c(0, max(ev_opinion) + 30))  # Extra space for labels
# Step 3: Add centered count labels above bars
text(x = bar_midpoints,
     y = ev_opinion,
     labels = ev_opinion,
     pos = 3,
     cex = 0.9)

#Plotting bar chart for opinion about EV better than regular Cars
# Step 1: Create frequency table
ev_vs_regular <- table(ev_data[,18])
# Step 2: Plot bar chart with space for labels
bar_midpoints <- barplot(ev_vs_regular,
                         main = "Opinion on EVs Being Better than Regular Cars",
                         xlab = "Opinion",
                         ylab = "Number of Respondents",
                         col = "mediumorchid",   # Soft purple tone
                         las = 1,                # Horizontal x-axis labels
                         ylim = c(0, max(ev_vs_regular) + 20))  # Extra space for count labels
# Step 3: Add count labels centered on top of bars
text(x = bar_midpoints,
     y = ev_vs_regular,
     labels = ev_vs_regular,
     pos = 3,
     cex = 0.9)

#Plotting bar chart for opinion about EV can save a lot of money
# Step 1: Create frequency table
ev_savings_opinion <- table(ev_data[,19])
# Step 2: Plot vertical bar chart
bar_midpoints <- barplot(ev_savings_opinion,
                         main = "Opinion on EVs Saving Money",
                         xlab = "Opinion",
                         ylab = "Number of Respondents",
                         col = "green3",       # Grass green tone
                         las = 1,              # Horizontal x-axis labels
                         ylim = c(0, max(ev_savings_opinion) + 20))
# Step 3: Add centered count labels on top of bars
text(x = bar_midpoints,
     y = ev_savings_opinion,
     labels = ev_savings_opinion,
     pos = 3,
     cex = 0.9)

#Plotting bar chart for opinion about EV are expensive
# Step 1: Create frequency table
ev_savings_opinion_20 <- table(ev_data[,20])
# Step 2: Plot vertical bar chart
bar_midpoints <- barplot(ev_savings_opinion_20,
                         main = "Opinion on EVs Saving Money",
                         xlab = "Opinion",
                         ylab = "Number of Respondents",
                         col = "yellow",       # Grass green tone
                         las = 1,              # Horizontal x-axis labels
                         ylim = c(0, max(ev_savings_opinion_20) + 15))
# Step 3: Add centered count labels on top of bars
text(x = bar_midpoints,
     y = ev_savings_opinion_20,
     labels = ev_savings_opinion_20,
     pos = 3,
     cex = 0.9)

#Plotting Bar Chart for opinion about EV costs about the same to buy a Fuel/CNG vehicles
# Step 1: Create frequency table
ev_cost_opinion <- table(ev_data[,23])
# Step 2: Plot bar chart and capture bar midpoints
bar_midpoints <- barplot(ev_cost_opinion,
                         main = "Opinion on EV Cost vs Fuel/CNG Vehicles",
                         xlab = "Opinion",
                         ylab = "Number of Respondents",
                         col = "red",           # Bold red color
                         las = 1,               # Horizontal x-axis labels
                         ylim = c(0, max(ev_cost_opinion) + 30))
# Step 3: Add count labels centered on top of bars
text(x = bar_midpoints,
     y = ev_cost_opinion,
     labels = ev_cost_opinion,
     pos = 3,
     cex = 0.9)

#Plotting Bar chart for government incentives influenced decision to buy EV
# Step 1: Create frequency table
gov_incentive_opinion <- table(ev_data[,28])
# Step 2: Match responses to custom colors
response_colors <- c("Yes" = "forestgreen",
                     "No" = "red",
                     "Maybe" = "royalblue")
# Step 3: Plot bar chart and capture midpoints
bar_midpoints <- barplot(gov_incentive_opinion,
                         main = "Did Government Incentives Influence EV Purchase?",
                         xlab = "Opinion",
                         ylab = "Number of Respondents",
                         col = response_colors[names(gov_incentive_opinion)],
                         las = 1,
                         ylim = c(0, max(gov_incentive_opinion) + 25))
# Step 4: Add centered count labels above bars
text(x = bar_midpoints,
     y = gov_incentive_opinion,
     labels = gov_incentive_opinion,
     pos = 3,
     cex = 0.9)

#----------------------------------------------------
# Chi square test of association
# Contingency table of Gender vs Preference of Type of Vehicle
# Extract relevant columns
gender <- ev_data[,3]
vehicle_pref <- ev_data[,10]

# Create contingency table 
contingency_table <- table(gender, vehicle_pref)
# --- Display formatted table ---
cat("Gender vs Preference of Type of Vehicle\n\n")
vehicle_types <- colnames(contingency_table)
genders <- rownames(contingency_table)

# Print header
cat(sprintf("%-10s", "Gender"), paste(sprintf("%-22s", vehicle_types), collapse = ""), "TOTAL\n")
cat(rep("-", 10 + length(vehicle_types)*22 + 7), "\n")

# Print rows with totals
for (i in 1:nrow(contingency_table)) {
  row_values <- contingency_table[i, ]
  row_total <- sum(row_values)
  cat(sprintf("%-10s", genders[i]), paste(sprintf("%-22d", row_values), collapse = ""), sprintf("%-5d\n", row_total))
}

# Print column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-10s", "TOTAL"), paste(sprintf("%-22d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# --- Chi-square Test Output ---
cat("Chi-Square Test of Independence Results:\n")
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

# Contingency table of occupation with respect to gender
# Step 1: Extract relevant columns
gender <- ev_data[,3]
occupation <- ev_data[,4]

# Step 2: Create contingency table
contingency_table <- table(occupation, gender)

# Step 3: Display formatted contingency table with totals
cat("Occupation vs Gender Contingency Table\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Header
cat(sprintf("%-15s", "Occupation"), paste(sprintf("%-12s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 15 + length(col_titles)*12 + 7), "\n")

# Row values
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-15s", row_titles[i]), paste(sprintf("%-12d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-15s", "TOTAL"), paste(sprintf("%-12d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Chi-square test of independence
cat("Chi-Square Test Results:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

#Contingency table of Battery replacement of EV is expensive with next vehicle will be EV 
# Step 1: Extract relevant columns
battery_cost_opinion <- ev_data[,25]
next_vehicle_ev <- ev_data[,29]

# Step 2: Create contingency table
contingency_table <- table(battery_cost_opinion, next_vehicle_ev)

# Step 3: Display formatted contingency table
cat("Battery Replacement Cost vs Next Vehicle Choice\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Print header
cat(sprintf("%-30s", "Battery Replacement Opinion"), paste(sprintf("%-20s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 30 + length(col_titles)*20 + 7), "\n")

# Print row values with totals
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-30s", row_titles[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Print column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-30s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Chi-square test of independence
cat("Chi-Square Test Results:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

#Contingency table of cost to buy an EV is same as CNG/Fuel Vehicle with next vehicle will be EV 
# Step 1: Extract relevant columns
ev_cost_opinion <- ev_data[,23]
next_ev_decision <- ev_data[,29]

# Step 2: Create contingency table
contingency_table <- table(ev_cost_opinion, next_ev_decision)

# Step 3: Display formatted contingency table with totals
cat("Contingency Table: EV Cost vs Next Vehicle Decision\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Header row
cat(sprintf("%-35s", "EV Cost Opinion"), paste(sprintf("%-20s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 35 + length(col_titles)*20 + 7), "\n")

# Row values
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-35s", row_titles[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-35s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Chi-square test of independence
cat("Chi-Square Test Result:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

#  Contingency table of occupation with EV is expensive
# Step 1: Extract relevant columns
occupation <- ev_data[,4]
ev_expensive <- ev_data[,20]

# Step 2: Create contingency table
contingency_table <- table(occupation, ev_expensive)

# Step 3: Display formatted contingency table with totals
cat("Contingency Table: Occupation vs EV Expense Opinion\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Header row
cat(sprintf("%-25s", "Occupation"), paste(sprintf("%-20s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 25 + length(col_titles)*20 + 7), "\n")

# Row values
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-25s", row_titles[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-25s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Chi-square test of independence
cat("Chi-Square Test Result:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

# Contingency table of age group with next vehicle will be EV 
# Step 1: Extract relevant columns
age_group <- ev_data[,2]
next_vehicle_ev <- ev_data[,27]

# Step 2: Create contingency table
contingency_table <- table(age_group, next_vehicle_ev)

# Step 3: Display formatted contingency table with totals
cat("Contingency Table: Age Group vs Next Vehicle Will Be EV\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Header row
cat(sprintf("%-20s", "Age Group"), paste(sprintf("%-20s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 20 + length(col_titles)*20 + 7), "\n")

# Row values
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-20s", row_titles[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-20s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Chi-square test
cat("Chi-Square Test Result:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

# Contingency table of influenced by gov.incentives with next vehicle will be EV 
# Step 1: Extract relevant columns
gov_incentive <- ev_data[,28]
next_vehicle_ev <- ev_data[,29]

# Step 2: Create contingency table
contingency_table <- table(gov_incentive, next_vehicle_ev)

# Step 3: Display formatted contingency table with totals
cat("Contingency Table: Influenced by Gov Incentives vs Next Vehicle Will Be EV\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Header row
cat(sprintf("%-25s", "Gov Incentive"), paste(sprintf("%-20s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 25 + length(col_titles)*20 + 7), "\n")

# Row values
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-25s", row_titles[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-25s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Chi-square test
cat("Chi-Square Test Result:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

# Contingency table of influenced by gov.incentives with switch to EV
# Step 1: Extract relevant columns
gov_influence <- ev_data[,28]
switch_to_ev <- ev_data[,35]

# Step 2: Create contingency table
contingency_table <- table(gov_influence, switch_to_ev)

# Step 3: Display formatted contingency table with totals
cat("Contingency Table: Govt Incentive Influence vs Switch to EV\n\n")
row_titles <- rownames(contingency_table)
col_titles <- colnames(contingency_table)

# Print header
cat(sprintf("%-30s", "Gov Incentive"), paste(sprintf("%-20s", col_titles), collapse = ""), "TOTAL\n")
cat(rep("-", 30 + length(col_titles)*20 + 7), "\n")

# Print row values
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-30s", row_titles[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Print column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-30s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Step 4: Perform Chi-square test of independence
cat("Chi-Square Test Result:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

# Contingency table of residential area with switch to EV 
# Extract the relevant columns
res_area <- ev_data[,6]
switch_to_ev <- ev_data[,35]

# Create contingency table
contingency_table <- table(res_area, switch_to_ev)

# Display formatted contingency table with row and column totals
cat("Contingency Table: Residential Area vs Switch to EV\n\n")
row_names <- rownames(contingency_table)
col_names <- colnames(contingency_table)

# Print header
cat(sprintf("%-25s", "Residential Area"), paste(sprintf("%-20s", col_names), collapse = ""), "TOTAL\n")
cat(rep("-", 25 + length(col_names)*20 + 7), "\n")

# Print row data
for (i in 1:nrow(contingency_table)) {
  row_vals <- contingency_table[i, ]
  row_total <- sum(row_vals)
  cat(sprintf("%-25s", row_names[i]), paste(sprintf("%-20d", row_vals), collapse = ""), sprintf("%-5d\n", row_total))
}

# Print column totals
col_totals <- colSums(contingency_table)
cat(sprintf("%-25s", "TOTAL"), paste(sprintf("%-20d", col_totals), collapse = ""), sprintf("%-5d\n\n", sum(col_totals)))

# Chi-square test
cat("Chi-Square Test Result:\n")
chi_result <- chisq.test(contingency_table)
print(chi_result)

#--------------------------------------------------------------------
# ANALYSIS OF VARIANCE
# Load required libraries
library(dplyr)
library(ggplot2)
library(multcomp)

# Create a synthetic dataset from group summaries
set.seed(123)  # For reproducibility

group_data <- list(
  G1 = rnorm(n = 166, mean = 1.7771, sd = sqrt(0.42876304)),
  G2 = rnorm(n = 110, mean = 1.8909, sd = sqrt(0.24482704)),
  G3 = rnorm(n = 137, mean = 1.9343, sd = sqrt(0.10595025)),
  G4 = rnorm(n = 35,  mean = 1.9429, sd = sqrt(0.17313921))
)

# Combine into one data frame
vehicle_pref_df <- bind_rows(
  data.frame(AgeGroup = "Below 18", Preference = group_data$G1),
  data.frame(AgeGroup = "18–22",     Preference = group_data$G2),
  data.frame(AgeGroup = "23–30",     Preference = group_data$G3),
  data.frame(AgeGroup = "Above 30",  Preference = group_data$G4)
) %>%
  mutate(AgeGroup = factor(AgeGroup, levels = c("Below 18", "18–22", "23–30", "Above 30")))

# One-way ANOVA
anova_model <- aov(Preference ~ AgeGroup, data = vehicle_pref_df)
summary(anova_model)

# Fisher's Individual Tests (Tukey HSD)
fisher_test <- glht(anova_model, linfct = mcp(AgeGroup = "Tukey"))
summary(fisher_test)
confint(fisher_test)

# Visualization: Boxplot
ggplot(vehicle_pref_df, aes(x = AgeGroup, y = Preference)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Vehicle Preference by Age Group", x = "Age Group", y = "Preference Score") +
  theme_minimal()

# CONFUSION MATRIX 
# MODEL 1
# Recreate the data frame
data <- data.frame(
  Gender = gender,
  Age = factor(age),
  Occupation = factor(occupation),
  Education = factor(education),
  Income = factor(income),
  NextEV = factor(next_ev),
  Incentive = factor(govt_incentive)
)

# Refit the model
model <- glm(Gender ~ Age + Occupation + Education + Income + NextEV + Incentive,
             data = data, family = binomial)

# Predict probabilities
prob <- predict(model, newdata = data, type = "response")

# Confirm prediction length
length(prob)  # Should be 448

# Predict probabilities from the refitted model
prob <- predict(model, newdata = data, type = "response")

# Combine actual and predicted values into a single data frame
results <- data.frame(
  Actual = data$Gender,
  PredictedProb = prob
)

# Remove any rows with NA (just in case)
results <- na.omit(results)

# Convert predicted probabilities to class labels using threshold
threshold <- 0.5
results$PredictedClass <- ifelse(results$PredictedProb > threshold, 1, 0)

# Generate confusion matrix
conf_matrix <- table(Predicted = results$PredictedClass, Actual = results$Actual)
print("Confusion Matrix:")
print(conf_matrix)

# Extract values from confusion matrix
TP <- conf_matrix["1", "1"]
TN <- conf_matrix["0", "0"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]

# Compute performance metrics
accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)  # Sensitivity
specificity <- TN / (TN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("\nModel Performance Metrics:\n")
cat("Accuracy    :", round(accuracy, 3), "\n")
cat("Precision   :", round(precision, 3), "\n")
cat("Recall      :", round(recall, 3), "\n")
cat("Specificity :", round(specificity, 3), "\n")
cat("F1 Score    :", round(f1_score, 3), "\n")

# MODEL 2
# Load required libraries
library(dplyr)

# Recreate data frame for Model 2
n <- 448
residential_area <- c(rep(1, 178), rep(0, 270))
age <- c(rep("Below 18", 35), rep("18-22", 166), rep("23-30", 110), rep("Above 30", 137))
occupation <- c(rep("Home Maker", 45), rep("Others", 119), rep("Self Employed", 87), rep("Student", 197))
education_raw <- c(rep("Diploma", 16), rep("Matriculate", 52), rep("Higher Secondary", 133),
                   rep("Graduation", 172), rep("Post Diploma", 49), rep("Others", 29))
education <- education_raw[1:n]
income <- c(rep("<2L", 145), rep("2L-8L", 175), rep("9L-15L", 104), rep(">15L", 24))
global_warming <- c(rep("Strongly Disagree", 1), rep("Disagree", 6), rep("Don't Know", 15),
                    rep("Agree", 116), rep("Strongly Agree", 310))[1:n]
next_ev <- c(rep("No EV", 46), rep("EV in 10 years", 92), rep("EV in 5 years", 193), rep("ASAP", 117))

data <- data.frame(
  Area = residential_area,
  Age = factor(age),
  Occupation = factor(occupation),
  Education = factor(education),
  Income = factor(income),
  GlobalWarming = factor(global_warming),
  NextEV = factor(next_ev)
)

# Fit logistic regression model
model <- glm(Area ~ Age + Occupation + Education + Income + GlobalWarming + NextEV,
             data = data, family = binomial)

# Predict class labels using 0.5 threshold
prob <- predict(model, type = "response")
pred_class <- ifelse(prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = pred_class, Actual = data$Area)
print("Confusion Matrix:")
print(conf_matrix)

# Extract values
TP <- conf_matrix["1", "1"]
TN <- conf_matrix["0", "0"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]

# Compute metrics
accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("\nModel Performance Metrics:\n")
cat("Accuracy     :", round(accuracy, 3), "\n")
cat("Precision    :", round(precision, 3), "\n")
cat("Recall       :", round(recall, 3), "\n")
cat("Specificity  :", round(specificity, 3), "\n")
cat("F1 Score     :", round(f1_score, 3), "\n")

# MULTIPLE LOGISTIC REGRESSION MODEL
# Model 1:
# Load required libraries
library(pROC)
library(ggplot2)

# Total respondents
n <- 448

# Gender response (1 = Female, 0 = Male)
gender <- c(rep(1, 182), rep(0, 266))

# Age group distribution (total 448)
age <- c(
  rep("Below 18", 35),
  rep("18-22", 166),
  rep("23-30", 110),
  rep("Above 30", 137)
)

# Occupation distribution (total 448)
occupation <- c(
  rep("Home Maker", 45),
  rep("Others", 119),
  rep("Self Employed", 87),
  rep("Student", 197)
)

# Education distribution (total 451 → trim to 448)
education_raw <- c(
  rep("Diploma", 16),
  rep("Matriculate", 52),
  rep("Higher Secondary", 133),
  rep("Graduation", 172),
  rep("Post Diploma", 49),
  rep("Others", 29)
)
education <- education_raw[1:n]

# Income distribution (total 448)
income <- c(
  rep("<2L", 145),
  rep("2L-8L", 175),
  rep("9L-15L", 104),
  rep(">15L", 24)
)

# Next vehicle preference (total 448)
next_ev <- c(
  rep("No EV", 46),
  rep("EV in 10 years", 92),
  rep("EV in 5 years", 193),
  rep("ASAP", 117)
)

# Government incentive influence (total 448)
govt_incentive <- c(
  rep("Maybe", 91),
  rep("Yes", 261),
  rep("No", 96)
)

# Combine into data frame
data <- data.frame(
  Gender = gender,
  Age = factor(age),
  Occupation = factor(occupation),
  Education = factor(education),
  Income = factor(income),
  NextEV = factor(next_ev),
  Incentive = factor(govt_incentive)
)

# Fit logistic regression model
model <- glm(Gender ~ Age + Occupation + Education + Income + NextEV + Incentive,
             data = data, family = binomial)

# Print AIC
aic_value <- AIC(model)
print(paste("AIC:", aic_value))

# Predict probabilities
prob <- predict(model, type = "response")

# ROC analysis
roc_obj <- roc(data$Gender, prob)

# Plot ROC curve with AUC
ggplot(data.frame(
  TPR = roc_obj$sensitivities,
  FPR = 1 - roc_obj$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  annotate("text", x = 0.6, y = 0.2, label = paste("AUC =", round(auc(roc_obj), 3)), size = 5) +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

# MODEL 2 
# Load required libraries
library(pROC)
library(ggplot2)

# Total respondents
n <- 448

# Binary response: Residential Area (1 = Urban, 0 = Rural)
residential_area <- c(rep(1, 178), rep(0, 270))

# Age group distribution
age <- c(
  rep("Below 18", 35),
  rep("18-22", 166),
  rep("23-30", 110),
  rep("Above 30", 137)
)

# Occupation distribution
occupation <- c(
  rep("Home Maker", 45),
  rep("Others", 119),
  rep("Self Employed", 87),
  rep("Student", 197)
)

# Education distribution
education_raw <- c(
  rep("Diploma", 16),
  rep("Matriculate", 52),
  rep("Higher Secondary", 133),
  rep("Graduation", 172),
  rep("Post Diploma", 49),
  rep("Others", 29)
)
education <- education_raw[1:n]

# Family income distribution
income <- c(
  rep("<2L", 145),
  rep("2L-8L", 175),
  rep("9L-15L", 104),
  rep(">15L", 24)
)

# Belief in EV reducing global warming
global_warming <- c(
  rep("Strongly Disagree", 1),
  rep("Disagree", 6),
  rep("Don't Know", 15),
  rep("Agree", 116),
  rep("Strongly Agree", 310)
)
global_warming <- global_warming[1:n]

# Next vehicle preference
next_ev <- c(
  rep("No EV", 46),
  rep("EV in 10 years", 92),
  rep("EV in 5 years", 193),
  rep("ASAP", 117)
)

# Combine into data frame
data <- data.frame(
  Area = residential_area,
  Age = factor(age),
  Occupation = factor(occupation),
  Education = factor(education),
  Income = factor(income),
  GlobalWarming = factor(global_warming),
  NextEV = factor(next_ev)
)

# Fit logistic regression model
model <- glm(Area ~ Age + Occupation + Education + Income + GlobalWarming + NextEV,
             data = data, family = binomial)

# Print AIC
aic_value <- AIC(model)
print(paste("AIC:", aic_value))

# Predict probabilities
prob <- predict(model, type = "response")

# ROC analysis
roc_obj <- roc(data$Area, prob)

# Plot ROC curve with diagonal reference line and AUC
ggplot(data.frame(
  TPR = roc_obj$sensitivities,
  FPR = 1 - roc_obj$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  annotate("text", x = 0.6, y = 0.2, label = paste("AUC =", round(auc(roc_obj), 3)), size = 5) +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()


#---------------------------------------------
# Principle Component Analysis
# Load necessary library
library(stats)

# Simulate data based on your summary
set.seed(123)  # For reproducibility
n <- 448  # Total respondents

# Age group encoding: 1 = <18, 2 = 18–22, 3 = 23–30, 4 = >30
age <- sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(35, 166, 110, 137)/448)

# Gender encoding: 0 = Female, 1 = Male
gender <- sample(c(0, 1), size = n, replace = TRUE, prob = c(182, 266)/448)

# Occupation encoding: 1 = Homemaker, 2 = Self-employed, 3 = Student, 4 = Other
occupation <- sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(45, 87, 197, 119)/448)

# Education encoding: 1 = Matriculate, 2 = Higher Secondary, 3 = Diploma, 4 = Graduation, 5 = Post Diploma, 6 = Other
education <- sample(c(1, 2, 3, 4, 5, 6), size = n, replace = TRUE, prob = c(52, 133, 16, 172, 49, 29)/448)

# Income encoding: 1 = <2L, 2 = 2–8L, 3 = 9–15L, 4 = >15L
income <- sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(145, 175, 104, 24)/448)

# Combine into a data frame
survey_data <- data.frame(age, gender, occupation, education, income)

# Perform PCA on the correlation matrix
pca_result <- prcomp(survey_data, scale. = TRUE)

# Extract eigenvalues
eigenvalues <- pca_result$sdev^2

# Calculate proportions and cumulative variance
proportion <- eigenvalues / sum(eigenvalues)
cumulative <- cumsum(proportion)

# Format output to match your desired result
eigen_output <- data.frame(
  Eigenvalue = round(eigenvalues[1:5], 4),
  Proportion = round(proportion[1:5], 3),
  Cumulative = round(cumulative[1:5], 3)
)

# Print with heading
cat("Eigen analysis of the Correlation Matrix\n\n")
print(eigen_output)

# Load necessary library
library(stats)

# Set seed for reproducibility
set.seed(123)

# Total number of respondents
n <- 448

# Simulate variables based on your proportions

# Gender: 0 = Female, 1 = Male
gender <- sample(c(0, 1), size = n, replace = TRUE, prob = c(182, 266)/448)

# Age group: 1 = <18, 2 = 18–22, 3 = 23–30, 4 = >30
age <- sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(35, 166, 110, 137)/448)

# Occupation: 1 = Homemaker, 2 = Self-employed, 3 = Student, 4 = Other
occupation <- sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(45, 87, 197, 119)/448)

# Education: 1 = Matriculate, 2 = Higher Secondary, 3 = Diploma, 4 = Graduation, 5 = Post Diploma, 6 = Other
education <- sample(c(1, 2, 3, 4, 5, 6), size = n, replace = TRUE, prob = c(52, 133, 16, 172, 49, 29)/448)

# Income: 1 = <2L, 2 = 2–8L, 3 = 9–15L, 4 = >15L
income <- sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(145, 175, 104, 24)/448)

# Combine into a data frame
survey_data <- data.frame(
  Gender = gender,
  Age = age,
  Occupation = occupation,
  Highest_educational_qualification = education,
  Family_income = income
)

# Perform PCA on the correlation matrix
pca_result <- prcomp(survey_data, scale. = TRUE)

# Extract eigenvectors (loadings)
eigenvectors <- round(pca_result$rotation[, 1:5], 3)

# Format output
eigenvectors_df <- data.frame(Variable = rownames(eigenvectors), eigenvectors)
colnames(eigenvectors_df) <- c("Variable", "PC1", "PC2", "PC3", "PC4", "PC5")

# Print with heading
cat("Principal Component Loadings Table\n\n")
print(Principal_Component_Loadings_Table_df, row.names = FALSE)

# Plotting scree plot
# Your eigenvalues from PCA
eigenvalues <- c(1.1453, 1.1172, 0.9775, 0.9547, 0.8052)

# Component numbers
components <- 1:length(eigenvalues)

# Scree plot
plot(components, eigenvalues, type = "b", pch = 19, col = "blue",
     xlab = "Component Number", ylab = "Eigenvalue",
     main = "Scree Plot of Principal Components")
grid()

# Plotting Score plot
# Assuming you've already run PCA and stored it in pca_result
# If not, you can recreate it using your simulated data

# Extract scores (principal component values for each observation)
scores <- pca_result$x  # This contains PC1, PC2, ..., PCn for each respondent

# Plot PC1 vs PC2
plot(scores[, 1], scores[, 2],
     xlab = "First Component (PC1)",
     ylab = "Second Component (PC2)",
     main = "Score Plot of First Two Principal Components",
     pch = 19, col = "darkgreen")
grid()

# Plotting Loading Plot
# Define loadings for PC1 and PC2
loadings <- matrix(c(
  -0.079, -0.246,  # Gender
  0.151,  0.648,  # Age
  -0.458,  0.596,  # Occupation
  -0.715,  0.061,  # Highest educational qualification
  -0.500, -0.400   # Family income
), ncol = 2, byrow = TRUE)

rownames(loadings) <- c("Gender", "Age", "Occupation", 
                        "Highest educational qualification", "Family income")
colnames(loadings) <- c("PC1", "PC2")

# Expand plot limits to accommodate long labels
xlim_range <- range(loadings[,1]) * 1.5
ylim_range <- range(loadings[,2]) * 1.5

# Create the plot
plot(loadings[,1], loadings[,2],
     xlim = xlim_range, ylim = ylim_range,
     xlab = "First Component",
     ylab = "Second Component",
     main = "Loading Plot",
     type = "n")

# Add reference lines
abline(h = 0, v = 0, col = "gray", lty = 2)

# Draw arrows
arrows(0, 0, loadings[,1], loadings[,2], length = 0.1, col = "blue")

# Add labels with slight offset to avoid overlap
text(loadings[,1]*1.2, loadings[,2]*1.2, 
     labels = rownames(loadings), col = "darkred", cex = 0.9)

# Optional: add unit circle
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 0.5, fg = "lightgray")

q()