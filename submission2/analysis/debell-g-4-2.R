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

library(ggplot2)
library(dplyr)

# Prepare the data
star_plot_data <- final.data %>%
  filter(year %in% c(2010, 2012, 2015)) %>%
  filter(!is.na(Star_Rating)) %>%
  mutate(Star_Rating = factor(Star_Rating, levels = sort(unique(Star_Rating))))

# Create faceted bar plot
ggplot(star_plot_data, aes(x = Star_Rating)) +
  geom_bar(fill = "#3C78D8") +
  facet_wrap(~ year, nrow = 1) +
  labs(
    title = "Distribution of Star Ratings by Year",
    x = "Star Rating",
    y = "Number of Plans"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
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
  filter(year %in% 2010:2015, (avg_enrolled), (avg_eligibles)) %>%
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

data.2010 <- final.data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score))

### calculate raw average
data.2010 <- data.2010 %>%
  mutate(raw.rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw.rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type) %>%
    mutate(mkt_share = avg_enrollment/avg_eligibles,
          HMO=str_detect(plan_type, "HMO"))

### table of rounded ratings
rating.2010 <- data.2010 %>%
  mutate(rounded_30=ifelse(raw.rating>=2.75 & raw.rating<3.00 & Star_Rating ==3.0,1,0),
        rounded_35=ifelse(raw.rating>=3.25 & raw.rating<3.50 & Star_Rating ==3.5,1,0),
        rounded_40=ifelse(raw.rating>=3.75 & raw.rating<4.00 & Star_Rating ==4.0,1,0),
        rounded_45=ifelse(raw.rating>=4.25 & raw.rating<4.50 & Star_Rating ==4.5,1,0),
        rounded_50=ifelse(raw.rating>=4.75 & raw.rating<5.00 & Star_Rating ==5.0,1,0)) %>%
group_by(Star_Rating) %>% filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>%
summarize(count_30=sum(rounded_30),
          count_35=sum(rounded_35),
          count_40=sum(rounded_40),
          count_45=sum(rounded_45),
          count_50=sum(rounded_50)) %>%
mutate(rounded = count_30 + count_35 + count_40 + count_45 + count_50) %>%
select(Star_Rating, rounded)

kable(rating.2010, caption = "Number of Plans Rounded Up to Each Star Rating (2010)")


# 6. Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.

rd_3.0 <- data.2010 %>%
  filter(raw.rating >= 2.875 & raw.rating < 3.125) %>%
  mutate(treatment = ifelse(raw.rating >= 3, 1, 0))

model_3.0 <- lm(mkt_share ~ treatment, data = rd_3.0)
summary(model_3.0)

rd_3.5 <- data.2010 %>%
  filter(raw.rating >= 3.375 & raw.rating < 3.625) %>%
  mutate(treatment = ifelse(raw.rating >= 3.5, 1, 0))

model_3.5 <- lm(mkt_share ~ treatment, data = rd_3.5)
summary(model_3.5)

tibble(
  Cutoff = c("3 vs 2.5 Stars", "3.5 vs 3 Stars"),
  Estimate = c(coef(model_3.0)[["treatment"]], coef(model_3.5)[["treatment"]])
) %>%
  knitr::kable(digits = 4, caption = "RD Estimates of Star Rating Impact on Market Share")

# 7. Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars). Show all of the results in a graph. How sensitive are your findings to the choice of bandwidth?

library(broom)

bandwidths <- c(0.10, 0.12, 0.13, 0.14, 0.15)
results <- list()

for (bw in bandwidths) {
  # 3.0 cutoff
  rd_3 <- data.2010 %>%
    filter(raw.rating >= (3.0 - bw) & raw.rating < (3.0 + bw)) %>%
    mutate(treatment = ifelse(raw.rating >= 3.0, 1, 0))
  
  model_3 <- lm(mkt_share ~ treatment, data = rd_3)
  
  tidy_3 <- tidy(model_3) %>%
    filter(term == "treatment") %>%
    mutate(bandwidth = bw, cutoff = "3.0 vs 2.5")
  
  # 3.5 cutoff
  rd_35 <- data.2010 %>%
    filter(raw.rating >= (3.5 - bw) & raw.rating < (3.5 + bw)) %>%
    mutate(treatment = ifelse(raw.rating >= 3.5, 1, 0))
  
  model_35 <- lm(mkt_share ~ treatment, data = rd_35)
  
  tidy_35 <- tidy(model_35) %>%
    filter(term == "treatment") %>%
    mutate(bandwidth = bw, cutoff = "3.5 vs 3.0")
  
  # Combine results
  results[[length(results)+1]] <- tidy_3
  results[[length(results)+1]] <- tidy_35
}

rd_results <- bind_rows(results)

# Plot
ggplot(rd_results, aes(x = bandwidth, y = estimate, shape = cutoff)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.005) +
  labs(
    title = "RD Estimates by Star Ratings and Bandwidth",
    x = "Bandwidth",
    y = "Estimate and Standard Error",
    shape = "Star Rating"
  ) +
  theme_minimal(base_size = 14)

# 8. Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the distribution of the running variable before and after the relevent threshold values. What do you find?
ggplot(data.2010 %>% filter(raw.rating >= 2.5 & raw.rating <= 3.5), 
       aes(x = raw.rating)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 3.0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Raw Ratings Around 3-Star Cutoff (2010)",
       x = "Raw Rating", y = "Count of Plans") +
  theme_minimal()

  ggplot(data.2010 %>% filter(raw.rating >= 3.0 & raw.rating <= 4.0), 
       aes(x = raw.rating)) +
  geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = 3.5, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Raw Ratings Around 3.5-Star Cutoff (2010)",
       x = "Raw Rating", y = "Count of Plans") +
  theme_minimal()

save.image("submission2/Hwk4_workspace.Rdata")
