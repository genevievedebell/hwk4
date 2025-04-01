# 1. Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). Provide a box and whisker plot showing the distribution of plan counts by county over time. Do you think that the number of plans is sufficient, too few, or too many?

## number of plans ber county per year 
plans_per_county_year <- final.data %>%
  filter(year %in% 2010:2015) %>%
  group_by(fips, year) %>%
  summarise(plan_count = n_distinct(planid)) %>%
  ungroup()

## boxplot of plan counts over time 
ggplot(plans_per_county_year, aes(x = factor(year), y = plan_count)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Distribution of Medicare Advantage Plan Counts by County (2010–2015)",
    x = "Year",
    y = "Number of Plans per County"
  ) +
  theme_minimal()

## Interpretation: From 2010 to 2015, the number of Medicare Advantage plans available per county has steadily increased. The median county had access to about 7–9 plans in 2010, rising to around 12–14 by 2015. At the same time, the number of counties with extremely high plan counts (over 75) also increased, suggesting growth in highly competitive markets. Despite these gains, a subset of counties remained underserved, with access to fewer than 5 plans in some years — highlighting potential disparities in choice.

# 2. Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. How has this distribution changed over time?

## Filter data for the years of interest
star_ratings_years <- final.data %>%
  filter(year %in% c(2010, 2012, 2015)) %>%
  group_by(year) %>%
  summarise(star_rating = mean(Star_Rating, na.rm = TRUE))

## Bar plot of star ratings over time
ggplot(star_ratings_years, aes(x = factor(year), y = star_rating)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  labs(
    title = "Average Star Ratings of Medicare Advantage Plans (2010, 2012, 2015)",
    x = "Year",
    y = "Average Star Rating"
  ) +
  theme_minimal()

## The average Medicare Advantage star rating has increased from around 3.3 in 2010 to nearly 4.0 in 2015. This suggests that either plan quality has improved over time or that changes in rating methodology and incentives (like bonus payments for 4+ star plans) have led to upward shifts in ratings.

# 3. Plot the average benchmark payment over time from 2010 through 2015. How much has the average benchmark payment risen over the years?

## Filter data for the years of interest
avg_benchmark <- final.data %>%
  filter(year %in% 2010:2015) %>%
  group_by(year) %>%
  summarise(avg_benchmark = mean(ma_rate, na.rm = TRUE))

## Line plot of average benchmark payments over time
ggplot(avg_benchmark, aes(x = year, y = avg_benchmark)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "Average Benchmark Payment for MA Plans (2010–2015)",
    x = "Year",
    y = "Average Benchmark Payment ($)"
  ) +
  theme_minimal()

#. 4. Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?
## Filter data for the years of interest
avg_penetration <- final.data %>%
  filter(year %in% 2010:2015, !is.na(avg_enrolled), !is.na(avg_eligibles)) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarise(avg_ma_share = mean(ma_share, na.rm = TRUE))

## Line plot of average MA share over time
ggplot(avg_penetration, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Average Medicare Advantage Share of Medicare Eligibles (2010–2015)",
    x = "Year",
    y = "MA Share"
  ) +
  theme_minimal()

# 5. Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

library(dplyr)
library(knitr)

final_2010_table <- final.data %>%
  filter(year == 2010, !is.na(partc_score)) %>%
  count(partc_score) %>%
  filter(partc_score %in% c(3, 3.5, 4, 4.5, 5)) %>%
  rename(`Rounded Rating` = partc_score,
         `Number of Plans` = n)

kable(final_2010_table, caption = "Number of Plans Receiving Each Rounded Star Rating in 2010")


# 6. Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.
final_2010 <- final.data %>%
  filter(year == 2010, !is.na(partc_score), !is.na(avg_enrollment))

cutoff_3.0 <- 2.75
bw <- 0.125

rd_3.0 <- final_2010 %>%
  filter(partc_score >= (cutoff_3.0 - bw) & partc_score <= (cutoff_3.0 + bw)) %>%
  mutate(treatment = ifelse(partc_score >= cutoff_3.0, 1, 0))

model_3.0 <- lm(avg_enrollment ~ treatment, data = rd_3.0)
summary(model_3.0)
