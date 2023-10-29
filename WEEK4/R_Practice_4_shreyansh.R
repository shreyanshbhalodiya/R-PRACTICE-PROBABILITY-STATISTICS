
##install.packages("MASS")

library(MASS)
data_cat <- cats
head(data_cat)
df_male <- data_cat[data_cat["Sex"] == "M",]
df_female <- data_cat[data_cat["Sex"] == "F",]
### For body weight
t.test(df_male$Bwt, df_female$Bwt, alternative =
         "two.sided", var.equal = FALSE)

### For height
t.test(df_male$Hwt, df_female$Hwt, alternative = "two.sided", var.equal = FALSE)


# Data in two numeric vectors
before <-c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after <-c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  sleep = c(before,  after)
)


### with 95% confidence interval
t_test_1 <- t.test(sleep ~ group, data = my_data, 
      paired = TRUE, alternative = "greater",conf.level = .95)

t_test_1

### with 90% confidence interval
t_test_2 <- t.test(sleep ~ group, data = my_data, paired = 
                     TRUE, alternative = "greater",conf.level = .90)
t_test_2

attributes(t_test_1)





























