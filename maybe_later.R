### 4e. Histogram of Coverage

> Below is a histogram of municipalities where UNICEF is present showing the coverage of enrolled children (aged 3-17). Of note, we have reached 10% or less of the population in **`r sum(all_mun$coverage_percent <= 10, na.rm = TRUE)`** out of the **`r n_distinct(u_ben$pcode2)`** in which we operate. This is in addition to the **`r sum(is.na(all_mun$coverage_percent))`** where no UNICEF Education activities have occurred.

```{r PLOT-histogram-of-coverage}
all_mun %>% 
  ggplot(aes(x = coverage_percent)) +
  geom_histogram(binwidth = 10, colour = "black", fill = "cornflowerblue") +
  stat_bin(binwidth = 10, geom = "text", aes(label = ..count..), vjust = -0.5) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
  labs(x = "% of enrolled children (3-17) covered", 
       y = "Number of municipalties")
ylab("Number of municipalities") + xlab("Percent of enrolled children (3-17) covered")

```