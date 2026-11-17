#===============================================================================
#READ ME
#Please import the code to this before you run the code.
#===============================================================================

#I keep package and library in order to remember
#installed package from library to proceed
install.packages("dplyr")
install.packages("imputeTS")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("randomForest")
install.packages("scales")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("treemap")
install.packages("RColorBrewer")
install.packages("grid")
install.packages("car")
install.packages("broom")
install.packages("MASS")

#call installed package from library to proceed
library(dplyr)          #for data manipulation
library(imputeTS)       #for value imputation
library(ggplot2)        #for charts
library(tidyverse)      #for data analysis
library(corrplot)       #for correlation matrix visualization
library(randomForest)   #for importance analysis and classification
library(scales)         #for formatting (e.g dollar, percent)
library(gridExtra)      #for arranging multiple plots
library(reshape2)       #for reshaping data
library(treemap)        #for tree map visuals
library(RColorBrewer)   #for color palettes
library(grid)           #for graphs layout
library(car)            #for regression
library(broom)          #for model outputs
library(MASS)           #for applied statistic functions

#import data set 
#read hotel_bookings
View(hotel_bookings)

#===============================================================================
###   1. Part A: Data Preparation
#===============================================================================
###   1.1. Data Quality Issues
#===============================================================================
###   1.1.1. Missing Value

# --- NA Values ---
# Check for NA in each column
na_counts <- colSums(is.na(hotel_bookings))
cat("NA counts by column:\n")
print(na_counts)

# Impute children with 0, assuming that there are no children
hotel_bookings$children[is.na(hotel_bookings$children)] <- 0

# Show that NAs in 'children' were replaced
cat("\nChildren column after NA imputation:\n")
print(table(hotel_bookings$children, useNA = "ifany"))

# --- NULL Values ---
# Count "NULL" in each column
null_counts <- sapply(hotel_bookings, function(col) sum(col == "NULL", na.rm = TRUE))
cat("\n'NULL' string counts by column:\n")
print(null_counts)

# Replace "NULL" with "Unknown"
hotel_bookings_cleaned <- hotel_bookings
hotel_bookings_cleaned <- hotel_bookings_cleaned %>%
  mutate(across(where(is.character), ~ replace(., . == "NULL", "Unknown")))

# Confirm that "NULL" was replaced
cat("\nChecking for 'NULL' after replacement:\n")
print(sapply(hotel_bookings_cleaned, function(col) sum(col == "NULL", na.rm = TRUE)))

#---'Undefined' Value---
# Count "Undefined" in each column
undefined_counts <- sapply(hotel_bookings_cleaned, function(col) sum(as.character(col) == "Undefined", na.rm = TRUE))
cat("\n'Undefined' string counts by column:\n")
print(undefined_counts)

# Replace in specific columns
hotel_bookings_cleaned$meal[hotel_bookings_cleaned$meal == "Undefined"] <- "No Meals"
hotel_bookings_cleaned$market_segment[hotel_bookings_cleaned$market_segment == "Undefined"] <- "Unknown"
hotel_bookings_cleaned$distribution_channel[hotel_bookings_cleaned$distribution_channel == "Undefined"] <- "Unknown"

# Confirm replacements
cat("\nUnique values in 'meal' column:\n")
print(unique(hotel_bookings_cleaned$meal))

cat("\nUnique values in 'market_segment' column:\n")
print(unique(hotel_bookings_cleaned$market_segment))

cat("\nUnique values in 'distribution_channel' column:\n")
print(unique(hotel_bookings_cleaned$distribution_channel))

#===============================================================================
###   1.1.2. Inconsistencies Value

# Find unique entries in 'country' column that have only 2 characters
alpha2_codes <- unique(hotel_bookings_cleaned$country[nchar(as.character(hotel_bookings_cleaned$country)) == 2])
cat("Country codes with 2 characters (likely Alpha-2):\n")
print(alpha2_codes)

# Find unique entries in 'country' column that have only 1 character
alpha1_codes <- unique(hotel_bookings_cleaned$country[nchar(as.character(hotel_bookings_cleaned$country)) == 1])
cat("\nCountry codes with 1 character (possible data issue):\n")
print(alpha1_codes)

# Replace "CN" with "CHN" (CHN = China, Alpha-3 code)
hotel_bookings_cleaned$country[hotel_bookings_cleaned$country == "CN"] <- "CHN" #CHN because it represents China (according to Alpha-3 code)

# Confirm that "CN" was replaced
cat("\nUnique country codes after replacing 'CN' with 'CHN':\n")
print(sort(unique(hotel_bookings_cleaned$country)))  

#===============================================================================

###   1.1.3. Negative Values
# Count how many ADR values are <= 0 (to identify negative or zero ADRs)
adr_problem_count <- sum(hotel_bookings_cleaned$adr <= 0, na.rm = TRUE)
cat("Number of ADR values <= 0 BEFORE cleaning:", adr_problem_count, "\n")

# Fix ADR: if reservation_status is "Check-Out", make ADR positive (abs)
hotel_bookings_cleaned$adr <- ifelse(
  hotel_bookings_cleaned$reservation_status == "Check-Out",
  abs(hotel_bookings_cleaned$adr),  # ensure it's positive
  hotel_bookings_cleaned$adr        # leave unchanged for others
)
cat("ADR values converted to positive where Check-Out occurred.\n")

# Calculate median ADR for valid positive Check-Outs (used to impute 0 ADR values)
valid_adr_median <- median(
  hotel_bookings_cleaned$adr[
    hotel_bookings_cleaned$adr > 0 &
      hotel_bookings_cleaned$reservation_status == "Check-Out"
  ],
  na.rm = TRUE
)
cat("Median ADR for valid positive Check-Outs:", valid_adr_median, "\n")

# Impute ADR = 0 for Check-Outs with median ADR
hotel_bookings_cleaned$adr <- ifelse(
  hotel_bookings_cleaned$adr == 0 &
    hotel_bookings_cleaned$reservation_status == "Check-Out",
  valid_adr_median,
  hotel_bookings_cleaned$adr
)
cat("ADR values of 0 for Check-Outs replaced with median ADR.\n")

# Confirm how many ADR values <= 0 remain after cleaning
adr_problem_count_after <- sum(hotel_bookings_cleaned$adr <= 0, na.rm = TRUE)
cat("Number of ADR values <= 0 AFTER cleaning:", adr_problem_count_after, "\n")

#===============================================================================
# Create alias for compatibility with Parts B, C, D
hotel_cleaned <- hotel_bookings_cleaned   #becuz I always switch laptops, I create this twice

# Prepare data for analysis (needed for Parts B, C, D)
hotel_cleaned$revenue_per_booking <- hotel_cleaned$adr * (hotel_cleaned$stays_in_weekend_nights + hotel_cleaned$stays_in_week_nights)
hotel_cleaned$total_guests <- hotel_cleaned$adults + hotel_cleaned$children + hotel_cleaned$babies
hotel_cleaned$total_nights <- hotel_cleaned$stays_in_weekend_nights + hotel_cleaned$stays_in_week_nights

#===============================================================================
###   1.2. Dataset Summary Statistics
cat("=== Dataset Summary Statistics ===\n")
print(summary(hotel_bookings))

#===============================================================================
###   1.3. Visualization with Insights
#===============================================================================
###   1.3.1. Frequency of No-Shows by Customer Type Grouped by Hotel
# Create no-show indicator column: "No-Show" if canceled, else "Completed"
hotel_cleaned$no_show <- ifelse(hotel_cleaned$is_canceled == 1, "No-Show", "Completed")

# Prepare data: filter valid customer types, group by customer_type, hotel, and no_show, then count
viz1_data <- hotel_cleaned %>%
  filter(customer_type %in% c("Transient", "Transient-Party", "Contract", "Group")) %>%
  group_by(customer_type, hotel, no_show) %>%
  summarise(count = n(), .groups = "drop")

# Create grouped bar chart showing cancellation status per customer type and hotel
viz1 <- ggplot(viz1_data, aes(x = customer_type, y = count, fill = interaction(hotel, no_show))) +
  geom_bar(stat = "identity", position = "dodge") +  # grouped bars
  scale_fill_manual(values = c("City Hotel.Completed" = "blue", 
                               "Resort Hotel.Completed" = "green",
                               "City Hotel.No-Show" = "orange",
                               "Resort Hotel.No-Show" = "gold"),
                    name = "Hotel & Status") +  # manual colors using names
  labs(title = "Frequency of No-Shows by Customer Type Grouped by Hotel",
       x = "Customer Type",
       y = "Frequency") +
  theme_minimal() +  # clean background
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.3, size = 3)  # add count labels

print(viz1)

#===============================================================================
###   1.3.2. No-Show Reservations by Month (Time Series)
# Convert date and create time series
hotel_cleaned$reservation_date <- as.Date(hotel_cleaned$reservation_status_date)
hotel_cleaned$year_month <- format(hotel_cleaned$reservation_date, "%Y-%m") #extract year and month from date

# Count No-Shows per month
viz2_data <- hotel_cleaned %>%
  filter(is_canceled == 1) %>%
  group_by(year_month) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  arrange(year_month)

# Create time series line plot
viz2 <- ggplot(viz2_data, aes(x = year_month, y = frequency, group = 1)) +
  geom_line(color = "blue", linewidth = 1.2) +      # line plot with blue color
  geom_point(color = "blue", size = 2) +            # points at each data value
  labs(title = "Frequency of No-Show Reservations by Month",
       x = "Reservation Status Date (Month)",
       y = "Frequency") +
  theme_minimal() +  # clean visual theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = viz2_data$year_month[seq(1, nrow(viz2_data), by = 3)])  # show every 3rd month

print(viz2)

#===============================================================================
###   1.3.3. Monthly Booking Trends by Hotel Type
# Make sure months are ordered correctly
hotel_cleaned$arrival_date_month <- factor(hotel_cleaned$arrival_date_month, levels = month.name)

# Prepare data: group by month and hotel type, then count bookings
viz3_data <- hotel_cleaned %>%
  filter(arrival_date_month %in% month.name) %>%  # filter valid months
  group_by(arrival_date_month, hotel) %>%
  summarise(bookings = n(), .groups = "drop")

# Create bar chart
viz3 <- ggplot(viz3_data, aes(x = arrival_date_month, y = bookings, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +  # side-by-side bars
  labs(title = "Monthly Booking Trends by Hotel Type",
       x = "Month of Arrival",
       y = "Number of Bookings",
       fill = "Hotel Type") +
  theme_minimal() +  # clean style
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "green")) +  # use color names
  geom_text(aes(label = bookings), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3)  # add labels on bars

print(viz3)

#===============================================================================
###   1.3.4. Deposit Type Distribution for Cancelled Reservations
#filter only canceled reservations and group by hotel and deposit type
viz4_data <- hotel_cleaned %>%
  filter(is_canceled == 1) %>%
  group_by(hotel, deposit_type) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  mutate(deposit_type = factor(deposit_type, 
                               levels = c("No Deposit", "Non Refund", "Refundable")))  # order the deposit types

# Create stacked bar chart
viz4 <- ggplot(viz4_data, aes(x = deposit_type, y = frequency, fill = hotel)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "green"),  # use color names
                    name = "Hotel") +
  labs(title = "Deposit Type Distribution for Cancelled Reservations by Hotel",
       x = "Deposit Type",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  
  # Add labels inside bars: show as "X.XK" if > 500, else full number
  geom_text(aes(label = ifelse(frequency > 500, 
                               paste0(round(frequency / 1000, 1), "K"), 
                               frequency)), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "white", fontface = "bold")

print(viz4)

#===============================================================================
###   1.3.5. Distribution of Bookings by Market Segment
#calculation to show how bookings are distributed across market segments for each hotel type
viz5 <- hotel_cleaned %>%
  ggplot(aes(x = market_segment, fill = hotel)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Distribution of Bookings by Market Segment",
       x = "Market Segment",
       y = "Number of Bookings",
       fill = "Hotel Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "green"))  # color names instead of hex

print(viz5)

#===============================================================================
###   1.3.6. Market Segment Revenue Analysis
#filter out revenues, group by market segment, and calculate totals
viz6_data <- hotel_cleaned %>%
  filter(!is.na(revenue_per_booking) & revenue_per_booking > 0) %>%
  group_by(market_segment) %>%
  summarise(
    avg_revenue = mean(revenue_per_booking, na.rm = TRUE),     # average revenue per booking
    total_bookings = n(),                                      # number of bookings
    total_revenue = sum(revenue_per_booking, na.rm = TRUE),    # total revenue
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))  # sort from highest to lowest revenue

# Create bar chart
viz6 <- ggplot(viz6_data, aes(x = reorder(market_segment, total_revenue), 
                              y = total_revenue / 1e6)) +  # scale to millions
  geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +  # use color name
  coord_flip() +  # flip axes for better readability
  labs(title = "Total Revenue by Market Segment",
       x = "Market Segment",
       y = "Total Revenue (Millions $)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  geom_text(aes(label = paste0("$", round(total_revenue / 1e6, 1), "M")), # Add labels to bars showing revenue in millions with "M" suffix
            hjust = -0.1, size = 4)

print(viz6)

#===============================================================================
###   1.3.7. ADR Distribution by Hotel and Cancellation Status
#remove outliers and create cancellation status labels
viz7_data <- hotel_cleaned %>%
  filter(adr > 0 & adr < 500) %>%  # Remove extreme ADR values
  select(hotel, is_canceled, adr) %>%
  mutate(cancel_status = ifelse(is_canceled == 1, "Cancelled", "Not Cancelled"))

#Create density plot
viz7 <- ggplot(viz7_data, aes(x = adr, fill = factor(cancel_status))) +
  geom_density(alpha = 0.6) +  # overlapping density curves
  facet_wrap(~hotel, ncol = 1) +  # one plot per hotel type
  scale_fill_manual(values = c("Cancelled" = "orange", "Not Cancelled" = "blue"),
                    name = "Status") +  # use color names
  labs(title = "ADR Distribution by Hotel and Cancellation Status",
       x = "Average Daily Rate ($)",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 12))  # for labels

print(viz7)




#===============================================================================
# Part B: Business Driver Identification
#===============================================================================
###   2.1. Method 1: Correlation Analysis

#Linear relationships with adr
# Select only numeric variables (excluding date-related ones)
numeric_vars <- hotel_cleaned %>%
  select_if(is.numeric) %>%
  select(-arrival_date_year, -arrival_date_week_number, -arrival_date_day_of_month)  # Remove calendar-related columns
numeric_vars

# Calculate correlation with adr
cor_matrix <- cor(numeric_vars, use = "complete.obs")  #ignores rows with missing values
cor_matrix

# Extract correlation with adr
adr_correlations <- cor_matrix[, "adr"]
adr_correlations

# Create a data frame for correlation results
cor_df <- data.frame(
  variable = names(adr_correlations),
  correlation = adr_correlations
) %>%
  filter(variable != "adr") %>%
  arrange(desc(abs(correlation)))

# Print top 10 variables 
cat("Top 10 Factors Correlated with ADR:\n")
print(head(cor_df, 10))

###Visual 1: Top 10 Correlations with adr
# Create bar plot of top 10 correlations
p1 <- ggplot(head(cor_df, 10), aes(x = reorder(variable, abs(correlation)), 
                                   y = correlation, fill = correlation > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral"),
                    labels = c("Positive", "Negative"),
                    name = "Correlation") +
  labs(title = "Top 10 Variables Correlated with ADR",
       x = "Variable",
       y = "Correlation Coefficient") +
  theme_minimal() +
  geom_text(aes(label = round(correlation, 2)), 
            hjust = ifelse(head(cor_df$correlation, 10) > 0, -0.1, 1.1),
            size = 3.5)

print(p1)

###Visual 2: Correlation matrix heatmap of top factors + adr
top_vars <- c(head(cor_df$variable, 10), "adr")  # include ADR
top_vars

cor_top_matrix <- cor(numeric_vars[, top_vars], use = "complete.obs")
cor_top_matrix

# Create the correlation heatmap
corrplot(cor_top_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("darkred", "white", "darkgreen"))(100),
         title = "Correlation Matrix: Top ADR Factors",
         mar = c(0, 0, 2, 0))

###Visual 3: Scatter plots of top 4 factors vs adr
# Select top 4 
top_4 <- c("total_guests", "children", "adults", "total_of_special_requests")
scatter_plots <- list()

# Loop through the top 4 variables and create scatter plots
for (i in 1:4) {
  var <- top_4[i]
  corr <- cor_df$correlation[cor_df$variable == var]  # correlation value for annotation
  
  p <- ggplot(hotel_cleaned, aes_string(x = var, y = "adr")) +
    geom_point(alpha = 0.6, color = "steelblue") +        # scatter plot points
    geom_smooth(method = "lm", se = TRUE, color = "red") + # add linear regression line
    labs(title = paste(var, "vs ADR"),
         subtitle = paste("Correlation:", round(corr, 3)),
         x = gsub("_", " ", str_to_title(var)),  # make variable names readable
         y = "Average Daily Rate (ADR)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  scatter_plots[[i]] <- p
}

# Arrange scatter plots in 2x2 grid layout
grid.arrange(grobs = scatter_plots, ncol = 2,
             top = "Top 4 Factors vs ADR")

#===============================================================================
###   2.2. Method 2: Regression Modeling
#===============================================================================
###   2.2.1. Linear Regression (adr)

#choose predictors 
adr_vars <- c("total_guests","children","adults","total_of_special_requests",
              "lead_time","previous_cancellations","previous_bookings_not_canceled")

#build initial full model with all predictors
adr_df <- hotel_cleaned[, c("adr", adr_vars)]
adr_df <- na.omit(adr_df)

lm_adr <- lm(adr ~ ., data = adr_df)

cat("\n=== LINEAR REGRESSION: ADR (Full Model) ===\n")
print(summary(lm_adr))    #for display

# Check for multicollinearity using VIF
library(car)
cat("\n=== VIF (Multicollinearity) ===\n")
print(sort(vif(lm_adr), decreasing = TRUE))

#Stepwise regression (backward selection)
library(MASS)
lm_adr_step <- step(lm_adr, direction = "backward", trace = 0)

cat("\n=== LINEAR REGRESSION: ADR (Stepwise Model) ===\n")
print(summary(lm_adr_step))

#calculate standardized coefficients
adr_scaled <- as.data.frame(scale(adr_df))
lm_adr_std <- lm(adr ~ ., data = adr_scaled)

std_coef <- data.frame(term = names(coef(lm_adr_std))[-1],
                       std_est = coef(lm_adr_std)[-1])
std_coef <- std_coef[order(abs(std_coef$std_est), decreasing = TRUE), ]

cat("\n=== STANDARDIZED COEFFICIENTS (ADR) ===\n")
print(std_coef)

#Visual: Coefficient plot with confidence intervals
library(broom); library(ggplot2)
coef_ci <- tidy(lm_adr_step, conf.int = TRUE) |> subset(term != "(Intercept)")
coef_ci$term <- reorder(coef_ci$term, coef_ci$estimate)

ggplot(coef_ci, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha=0.5) +
  labs(title = "Regression Coeffecients with 95% CI",
       x = "Coefficient Estimate", 
       y = "Predictor") +
  theme_minimal()

#===============================================================================
###   2.2.2. Logistic Regression (is_canceled)
#choose predictors
cancel_vars <- c("lead_time","previous_cancellations","previous_bookings_not_canceled",
                 "booking_changes","total_of_special_requests","total_guests","adr")

logit_df <- hotel_cleaned[, c("is_canceled", cancel_vars)]
logit_df <- na.omit(logit_df)
logit_df$is_canceled <- as.factor(logit_df$is_canceled)  # 0/1 as factor

#build initial full model with all predictors
glm_can <- glm(is_canceled ~ ., data = logit_df, family = binomial())

cat("\n=== LOGISTIC REGRESSION: is_canceled (Full Model) ===\n")
print(summary(glm_can))  # coefficients on log-odds scale

#compute Odds Ratios with 95% Wald CI (avoids profiling error)
coef_table <- coef(summary(glm_can))  # get coefficients and SE
or <- exp(coef(glm_can))              # odds ratios
se <- coef_table[, "Std. Error"]      # standard errors for Wald CI

# Wald confidence intervals
ci_low <- exp(coef(glm_can) - 1.96 * se)
ci_high <- exp(coef(glm_can) + 1.96 * se)

# Make a tidy table of odds ratios
or_df <- data.frame(
  term = names(or),
  OR = or,
  CI_low = ci_low,
  CI_high = ci_high
)

# Remove intercept
or_df <- subset(or_df, term != "(Intercept)")
or_df <- or_df[order(abs(log(or_df$OR))), ]

cat("\n=== ODDS RATIOS (is_canceled) ===\n")
print(or_df)

# Quick in-sample performance check
pred_prob <- predict(glm_can, type = "response")  # predicted probabilities
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)      # classify at 0.5 threshold
cm <- table(Predicted = pred_class,
            Observed = as.integer(as.character(logit_df$is_canceled)))

cat("\n=== CONFUSION MATRIX (threshold = 0.5, in-sample) ===\n")
print(cm)

acc <- sum(diag(cm)) / sum(cm)
cat("\nAccuracy:", round(acc, 3), "\n")

#Visual: Odds Ratios with 95% Confidence Intervals
library(ggplot2)
or_plot <- or_df
or_plot$term <- reorder(or_plot$term, or_plot$OR)

ggplot(or_plot, aes(x = OR, y = term)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Cancellation Model: Odds Ratios (95% CI)",
       x = "Odds Ratio (exp(coef))", y = "Predictor") +
  theme_minimal()

#===============================================================================
###   2.2.3. Models Diagnostic
par(mfrow = c(2,2))
plot(lm_adr_step)
par(mfrow = c(1,1))


#===============================================================================
###   2.3. Features importance (Random Forest)

###---Revenue: adr---
# Choose variables
selected_vars <- c(
  "revenue_per_booking", "adr", "lead_time", "adults", "children", "babies",
  "stays_in_weekend_nights", "stays_in_week_nights", "total_of_special_requests",
  "previous_cancellations", "previous_bookings_not_canceled", "booking_changes"
)

rf_data <- hotel_cleaned[, selected_vars] %>%   #filter data
  na.omit()

# Training a model
set.seed(123)  # for reproducibility

rf_model <- randomForest(
  revenue_per_booking ~ ., 
  data = rf_data, 
  importance = TRUE, 
  ntree = 100  # number of trees
)

# Extract variable importance
importance_scores <- importance(rf_model)

# Create a tidy data frame
importance_df <- data.frame(
  Variable = rownames(importance_scores),
  Importance = round(importance_scores[, "%IncMSE"], 2)
) %>%
  mutate(
    Relative_Importance = round((Importance / max(Importance)) * 100, 1)
  ) %>%
  arrange(desc(Importance))

#Plot Top 10 features
library(ggplot2)

p_rf <- ggplot(head(importance_df, 10), aes(x = reorder(Variable, Importance), y = Relative_Importance)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Most Important Features for Revenue (Random Forest)",
       subtitle = "Relative Importance (Most important = 100%)",
       x = "Feature",
       y = "Relative Importance (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_rf)

###---Cancellation: is_canceled---
# Choose variables
cancel_vars_rf <- c("is_canceled", "lead_time", "adr", "previous_cancellations", 
                   "previous_bookings_not_canceled", "booking_changes", 
                   "total_of_special_requests", "total_guests", 
                   "adults", "children", "babies")

rf_cancel_data <- hotel_cleaned[, cancel_vars_rf]
rf_cancel_data <- na.omit(rf_cancel_data)
rf_cancel_data$is_canceled <- as.factor(rf_cancel_data$is_canceled)

# Training a model
set.seed(123)
rf_cancel_model <- randomForest(
  is_canceled ~ ., 
  data = rf_cancel_data, 
  importance = TRUE,
  ntree = 100  # Number of trees
)

# Extract variable importance
cancel_importance <- importance(rf_cancel_model, type = 2)

# Create a tidy data frame
cancel_importance_df <- data.frame(
  Feature = rownames(cancel_importance),
  Importance = round(cancel_importance[, "MeanDecreaseGini"], 2)
) %>%
  arrange(desc(Importance)) %>%
  mutate(Relative_Importance = round((Importance / max(Importance)) * 100, 1))

#Plot Top 10 features
ggplot(head(cancel_importance_df, 10), 
       aes(x = reorder(Feature, Importance), y = Relative_Importance)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Most Important Features for Cancellation (Random Forest)",
       subtitle = "Relative Importance (Most Important = 100)",
       x = "Feature", y = "Relative Importance (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


#===============================================================================
###   2.4. Comparison of Methods
#Combine results
comp <- dplyr::full_join(
  dplyr::select(cor_df, variable, correlation) %>% 
    dplyr::rename(Variable = variable, Corr = correlation),
  dplyr::select(std_coef, Variable = term, Reg = std_est),
  by = "Variable"
) %>%
  dplyr::full_join(
    dplyr::select(importance_df, Variable, RF = Importance),
    by = "Variable"
  )
comp

#Limit to top 10 based
top10_vars <- head(cor_df$variable, 10)
top10_vars

comp <- comp %>% filter(Variable %in% top10_vars)
comp

#Rescale each metric
comp_long <- comp %>%
  mutate(
    Corr_abs = abs(Corr),
    Reg_abs  = abs(Reg),
    RF_abs   = RF
  ) %>%
  pivot_longer(
    cols = c(Corr_abs, Reg_abs, RF_abs),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  mutate(
    Method = case_when(
      Method == "Corr_abs" ~ "Correlation",
      Method == "Reg_abs" ~ "Regression",
      Method == "RF_abs" ~ "RandomForest",
      TRUE ~ Method
    )
  ) %>%
  group_by(Method) %>%
  mutate(Relative = 100 * Value / max(Value, na.rm = TRUE)) %>%
  ungroup()
comp_long

#Plot the comparison
ggplot(comp_long, aes(x = reorder(Variable, Relative), y = Relative, fill = Method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Comparison of Top 10 Drivers Across Methods",
       subtitle = "Each method scaled to 0–100 for fair comparison",
       x = "Driver (Variable)", y = "Relative Importance (0–100)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# ==============================================================================
# 2.5 CLASSIFY TOP 10 DRIVERS INTO ACTIONABLE vs NON-ACTIONABLE
#Combine top drives from each method
top_corr <- cor_df |> arrange(desc(abs(correlation))) |> head(10) |> pull(variable)
top_reg <- std_coef$term[1:10]
top_rf <- importance_df$Variable[1:10]

top_all <- unique(c(top_corr, top_reg, top_rf))
top_all

#Decide on actionable and non-actionable #because it's a business judgment
class_map <- c(
  "revenue_per_booking" = "Actionable",
  "total_guests" = "Non-Actionable",
  "children" = "Non-Actionable",
  "adults" = "Non-Actionable",
  "total_of_special_requests" = "Actionable",
  "is_repeated_guest" = "Non-Actionable",
  "lead_time" = "Non-Actionable",
  "previous_cancellations" = "Non-Actionable",
  "previous_bookings_not_canceled" = "Non-Actionable",
  "required_car_parking_spaces" = "Actionable",
  "adr" = "Actionable",
  "booking_changes" = "Actionable",
  "stays_in_week_nights" = "Actionable",
  "stays_in_weekend_nights" = "Actionable",
  "babies" = "Non-Actionable"
)

driver_classification <- data.frame(
  Variable = top_all,
  Classification = ifelse(top_all %in% names(class_map),
                      class_map[top_all],
                      "Unknown")  # fallback if not classified
)

#I dont know why there still have NA
driver_classification_clean <- subset(driver_classification, Classification != "Unknown")
driver_classification_clean

#Plot driver classification with importance , using result from Random Forest
#combine classification
plot_df <- importance_df %>%
  inner_join(driver_classification_clean, by = c("Variable"))

#reorder 
plot_df$Variable <- reorder(plot_df$Variable, plot_df$Relative_Importance)

#plot
ggplot(plot_df, aes(x = Relative_Importance, y = Variable, fill = Classification)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Actionable" = "darkgreen", "Non-Actionable" = "darkred")) +
  labs(
    title = "Driver Classification by Importance (Random Forest)",
    subtitle = "Top variables with actionable vs. non-actionable categories",
    x = "Relative Importance (%)",
    y = "Driver (Variable)",
    fill = "Classification"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#===============================================================================
# Part C: Business Impact Analysis
#===============================================================================
### 3.1. Financial Impact of Key Drivers

#Define hotel basic information
rooms <- 200  #total room in the hotel
occupancy_rate <- 0.75  #75% of the rooms occupied on average
days <- 365

#calculate how many room we sell per year
room_nights <- rooms * occupancy_rate * days
room_nights

#Find our current average
current_adr <- mean(hotel_cleaned$adr, na.rm = TRUE)
current_special_requests <- mean(hotel_cleaned$total_of_special_requests, na.rm = TRUE)
current_booking_changes <- mean(hotel_cleaned$booking_changes, na.rm = TRUE)
current_parking <- mean(hotel_cleaned$required_car_parking_spaces, na.rm = TRUE)
current_weekend_stays <- mean(hotel_cleaned$stays_in_weekend_nights, na.rm = TRUE)

#assume avarage guests affected
avg_guests_per_booking <- mean(hotel_cleaned$adults + hotel_cleaned$children, na.rm = TRUE)

# Scenarios: Assume a % improvement
impact_df <- data.frame(
  Driver = c("ADR", "Special Requests", "Booking Changes", "Parking Spaces", "Weekend Nights Stay"),
  Current = c(current_adr, current_special_requests, current_booking_changes, current_parking, current_weekend_stays),
  Change_Percent = c(0.05, 0.1, 0.1, 0.1, 0.1),  # assumed % increase
  Impact_Formula = c(
    "New ADR * Room Nights",
    "ADR * Room Nights * 5% Uplift from More Requests",
    "ADR * Room Nights * 3% Uplift from Smoother Changes",
    "ADR * Room Nights * 2% Uplift from More Parking",
    "ADR * Room Nights * 4% Uplift from More Weekend Stays"
  ),
  Estimated_Impact = c(
    (current_adr * 1.05 - current_adr) * room_nights,
    current_adr * room_nights * 0.05,
    current_adr * room_nights * 0.03,
    current_adr * room_nights * 0.02,
    current_adr * room_nights * 0.04
  )
)

#display results
impact_df <- impact_df %>%
  mutate(
    Current = round(Current, 2),
    Estimated_Impact = dollar(round(Estimated_Impact, 0))
  )

print("=== FINANCIAL IMPACT OF 5 KEY DRIVERS (Annual Estimate) ===")
print(impact_df)

# Clean the Estimated_Impact column to numeric
impact_df$Estimated_Impact <- as.numeric(gsub("[$,]", "", impact_df$Estimated_Impact))

# Bar plot of financial impact estimates
ggplot(impact_df, aes(x = reorder(Driver, Estimated_Impact), y = Estimated_Impact)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Estimated Financial Impact of Key Drivers (Annual)",
    x = "Driver",
    y = "Estimated Impact (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

#===============================================================================
### 3.2. What-if scenarios
# === Define 5 Business Scenarios ===
scenarios <- data.frame(
  Scenario = c(
    "1. Improve Special Requests Handling",
    "2. Increase Weekend Stays",
    "3. Reduce Booking Changes",
    "4. Add Parking Spaces",
    "5. Raise ADR"
  ),
  Driver = c("total_of_special_requests", "stays_in_weekend_nights",
             "booking_changes", "required_car_parking_spaces", "adr"),
  Current = c(0.57, 0.93, 0.22, 0.06, 103.2),
  Change_Percent = c(0.10, 0.10, 0.10, 0.10, 0.05),  # 10% change or 5% for ADR
  Uplift_Percent = c(0.05, 0.04, 0.03, 0.02, 0.05),  # Expected revenue uplift
  stringsAsFactors = FALSE
)

# Set baseline values
adr <- 103.2
room_nights <- 54750  # example from your prior section

# === Calculate Estimated Revenue Impact for Each Scenario ===
scenarios$Estimated_Impact <- round(
  adr * room_nights * scenarios$Uplift_Percent,
  0
)

scenarios

#===============================================================================
### 3.3. ROI Calculation and Break-Even
# Assumptions
total_room_nights <- 200 * 0.75 * 365  # Total annual room nights sold
average_adr <- mean(hotel_cleaned$adr, na.rm = TRUE)  # Average daily rate

# Base revenue
annual_base_revenue <- total_room_nights * average_adr

# Scenario definitions
intervention_roi <- data.frame(
  Intervention = c(
    "Raise ADR by 5%",
    "Improve Special Requests by 10%",
    "Reduce Booking Changes by 10%",
    "Add Parking Spaces by 10%",
    "Increase Weekend Stays by 10%"
  ),
  Revenue_Uplift_Percent = c(0.05, 0.05, 0.03, 0.02, 0.04),  # Estimated uplift
  Implementation_Cost = c(50000, 30000, 20000, 25000, 40000)  # Estimated cost in USD
)

# Financial calculations
intervention_roi$Projected_Revenue_Gain <- round(intervention_roi$Revenue_Uplift_Percent * annual_base_revenue, 0)
intervention_roi$Net_Profit <- intervention_roi$Projected_Revenue_Gain - intervention_roi$Implementation_Cost
intervention_roi$ROI_Percentage <- round((intervention_roi$Net_Profit / intervention_roi$Implementation_Cost) * 100, 1)
intervention_roi$BreakEven_Uplift_Threshold <- round(intervention_roi$Implementation_Cost / annual_base_revenue * 100, 2)

# Display results
intervention_roi

#Plot bar chart
ggplot(intervention_roi, aes(x = reorder(Intervention, ROI_Percentage), y = ROI_Percentage)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = paste0(ROI_Percentage, "%")), 
            hjust = -0.2, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "ROI Comparison Across Interventions",
    x = "Intervention",
    y = "ROI (%)"
  ) +
  theme_minimal()

#===============================================================================


