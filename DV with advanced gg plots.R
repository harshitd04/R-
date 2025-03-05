# Step 1: Load the libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(corrplot)
library(ggthemes)
library(scales)
library(forcats)
library(RColorBrewer)

# Step 2: Load your dataset
file_path <- "C:/Users/RAKESH KUMAR DABAS/Desktop/Hotel Bookings.csv"  
hotel_data <- read_csv(file_path)

# Step 3: Explore the dataset
head(hotel_data)
summary(hotel_data)
str(hotel_data)

# Step 4: Data Cleaning
colSums(is.na(hotel_data))
hotel_data_clean <- hotel_data %>% drop_na()

# Step 5: Data Exploration and Visualizations

# Plot 1: Histogram of Lead Time
ggplot(hotel_data_clean, aes(x = lead_time)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Lead Time Distribution", x = "Lead Time", y = "Count")

# Plot 2: Bar plot of Hotel Type
ggplot(hotel_data_clean, aes(x = fct_infreq(hotel))) +
  geom_bar(fill = "darkgreen") +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Bookings by Hotel Type", x = "Hotel Type", y = "Number of Bookings")

# Plot 3: Correlation Heatmap for Numeric Variables
numeric_columns <- hotel_data_clean %>%
  select_if(is.numeric)
corr_matrix <- cor(numeric_columns, use = "complete.obs")
corrplot(corr_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# Plot 4: Scatter Plot with Outliers: Lead Time vs ADR
# No filtering applied to display all data points including outliers
ggplot(hotel_data_clean, aes(x = lead_time, y = adr)) +
  geom_point(alpha = 0.4, color = "darkred", size = 0.8) +  # Transparency and smaller points
  theme_minimal() +
  labs(title = "Lead Time vs ADR (Including Outliers)", x = "Lead Time (days)", y = "Average Daily Rate (ADR)") +
  scale_x_continuous(breaks = seq(0, max(hotel_data_clean$lead_time), 100)) +  # Dynamic x-axis limits
  scale_y_continuous(breaks = seq(0, max(hotel_data_clean$adr), 100)) +  # Dynamic y-axis limits
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Advanced Plot 1: Facet Wrap - Lead Time vs ADR by Hotel Type
ggplot(hotel_data_clean, aes(x = lead_time, y = adr, color = hotel)) +
  geom_point(alpha = 0.4, size = 0.8) +  # Transparency and smaller points
  theme_minimal() +
  facet_wrap(~ hotel) +  # Facet by hotel type
  labs(title = "Lead Time vs ADR by Hotel Type", x = "Lead Time (days)", y = "Average Daily Rate (ADR)") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")  # Style for facet labels
  )

# Advanced Plot 2: Boxplot of ADR by Hotel Type with Jittered Points
ggplot(hotel_data_clean, aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkblue", size = 0.7) +  # Jittered points
  theme_minimal() +
  scale_fill_manual(values = c("darkorange", "darkgreen")) +  # Custom colors for hotel types
  labs(title = "ADR Distribution by Hotel Type", x = "Hotel Type", y = "Average Daily Rate (ADR)") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

# Advanced Plot 3: Pie Chart of Market Segment
market_segment_count <- hotel_data_clean %>%
  count(market_segment) %>%
  mutate(perc = round(n / sum(n) * 100, 1))  # Calculate percentage

ggplot(market_segment_count, aes(x = "", y = perc, fill = market_segment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Polar coordinates for pie chart
  theme_void() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Market Segment Distribution", fill = "Market Segment") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Advanced Plot 4: Stacked Bar Plot for Deposit Type by Hotel
ggplot(hotel_data_clean, aes(x = hotel, fill = deposit_type)) +
  geom_bar(position = "fill", color = "white") +  # Position "fill" creates a stacked bar chart
  theme_minimal() +
  scale_fill_manual(values = c("No Deposit" = "darkgreen", "Non Refund" = "red", "Refundable" = "orange")) +
  labs(title = "Deposit Type by Hotel", x = "Hotel", y = "Proportion", fill = "Deposit Type") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Advanced Plot 5: Heatmap for Lead Time, ADR, and Stays
# Using geom_tile for a simple heatmap of correlations
agg_data <- hotel_data_clean %>%
  group_by(lead_time, stays_in_weekend_nights) %>%
  summarize(mean_adr = mean(adr, na.rm = TRUE))

ggplot(agg_data, aes(x = lead_time, y = stays_in_weekend_nights, fill = mean_adr)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap of Lead Time, Weekend Stays and ADR", x = "Lead Time", y = "Stays in Weekend Nights", fill = "Mean ADR") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Step 6: Save a plot if needed
ggsave("lead_time_vs_adr_with_outliers.png")
