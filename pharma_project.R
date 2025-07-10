# Load required libraries
library(tidyverse)
library(readr)
library(lubridate)

# Load the dataset
pharma_data <- read_csv("E:/indian_pharmaceutical_products_clean.csv")

# View structure of the dataset
glimpse(pharma_data)

# Preview first few rows
head(pharma_data)


# Check for duplicate rows
pharma_data <- pharma_data %>% distinct()

# Check for missing values summary
colSums(is.na(pharma_data))

# Convert therapeutic_class and dosage_form to lowercase for consistency
pharma_data <- pharma_data %>%
  mutate(
    therapeutic_class = str_to_lower(therapeutic_class),
    dosage_form = str_to_lower(dosage_form)
  )

# Preview cleaned data
head(pharma_data) 


# Load ggplot2 (comes with tidyverse)
library(ggplot2)

# Top 10 most expensive products
top_expensive <- pharma_data %>%
  arrange(desc(price_inr)) %>%
  slice_head(n = 10)

# Plot
ggplot(top_expensive, aes(x = reorder(brand_name, price_inr), y = price_inr)) +
  geom_col(fill = "#0073C2FF") +
  coord_flip() +
  labs(
    title = "Top 10 Most Expensive Pharmaceutical Products",
    x = "Brand Name",
    y = "Price (INR)"
  ) +
  theme_minimal()



# Load scales package for number formatting
library(scales)

# Count of products per therapeutic class (top 10)
top_classes <- pharma_data %>%
  count(therapeutic_class, sort = TRUE) %>%
  slice_head(n = 10)

# Plot
ggplot(top_classes, aes(x = reorder(therapeutic_class, n), y = n)) +
  geom_col(fill = "#E64B35FF") +
  coord_flip() +
  labs(
    title = "Top 10 Therapeutic Classes by Product Count",
    x = "Therapeutic Class",
    y = "Number of Products"
  ) +
  scale_y_continuous(labels = comma) +  # This prevents scientific notation
  theme_minimal()



# Top 10 therapeutic classes by average price
avg_price_class <- pharma_data %>%
  group_by(therapeutic_class) %>%
  summarise(avg_price = mean(price_inr, na.rm = TRUE)) %>%
  arrange(desc(avg_price)) %>%
  slice_head(n = 10)

# Plot
ggplot(avg_price_class, aes(x = reorder(therapeutic_class, avg_price), y = avg_price)) +
  geom_col(fill = "#00A087FF") +
  coord_flip() +
  labs(
    title = "Top 10 Therapeutic Classes by Average Price",
    x = "Therapeutic Class",
    y = "Average Price (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()



# Top 5 manufacturers by number of products
top_mfr <- pharma_data %>%
  count(manufacturer, sort = TRUE) %>%
  slice_head(n = 5) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         label = paste0(manufacturer, " (", percent, "%)"))

# Plot as pie chart
ggplot(top_mfr, aes(x = "", y = n, fill = label)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Top 5 Manufacturers by Product Count",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Pastel1")


# Filter data for Allegra brand only
allegra_data <- pharma_data %>%
  filter(str_detect(tolower(brand_name), "allegra")) %>%
  arrange(desc(price_inr)) %>%
  mutate(batch = row_number())  # Simulate batch/entry order

# Plot line chart
ggplot(allegra_data, aes(x = batch, y = price_inr)) +
  geom_line(color = "#3C5488FF", size = 1.2) +
  geom_point(color = "#3C5488FF", size = 2) +
  labs(
    title = "Price Trend for Allegra Variants (Simulated Batches)",
    x = "Batch Number (simulated)",
    y = "Price (INR)"
  ) +
  theme_minimal()



# 1. Top 10 most expensive products
top_10_expensive <- pharma_data %>%
  arrange(desc(price_inr)) %>%
  slice_head(n = 10)
write.csv(top_10_expensive, "E:/top_10_expensive_products.csv", row.names = FALSE)

# 2. Product count by therapeutic class
therapeutic_summary <- pharma_data %>%
  count(therapeutic_class, sort = TRUE)
write.csv(therapeutic_summary, "E:/product_count_by_therapeutic_class.csv", row.names = FALSE)

# 3. Average price by therapeutic class
avg_price_summary <- pharma_data %>%
  group_by(therapeutic_class) %>%
  summarise(avg_price = mean(price_inr, na.rm = TRUE)) %>%
  arrange(desc(avg_price))
write.csv(avg_price_summary, "E:/avg_price_by_therapeutic_class.csv", row.names = FALSE)


