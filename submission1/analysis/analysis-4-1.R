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

plans_per_county_year %>%
  group_by(year) %>%
  summarise(
    mean_plans = mean(plan_count),
    median_plans = median(plan_count),
    max_plans = max(plan_count),
    n_counties = n()
  )
