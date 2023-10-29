library("readxl")
df <- read_excel("Practice_Assignment_Data.xls")

df <- na.omit(df)  ## REMOVING NULL VALUES
sum(is.na(df$age))
### https://sparkbyexamples.com/r-programming/remove-rows-with-na-in-r/
install.packages("jtools")
unique(df$treatment)

## creating dummy variable for each of the unique values in column treatment.
df$treatment_gabapentin <- ifelse(df$treatment == "Gabapentin", 1, 0)
df$treatment_Placebo <- ifelse(df$treatment == "Placebo", 1, 0)

head(df)

library(jtools) # Load jtools
# Telling R we want to use this data
fit <- lm(wbc ~ time + age + smoker + hrt_months+ outcome + treatment_gabapentin + treatment_Placebo, data = df)
summ(fit)

library(jtools) # Load jtools
# Telling R we want to use this data
fit <- lm(wbc ~ time + age + smoker + hrt_months+ outcome, data = df)
summ(fit)

fit <- lm(df$wbc ~ df$time + df$age + df$smoker + df$hrt_months+ df$outcome)
summary(fit)## https://www.geeksforgeeks.org/dummy-variables-in-r-programming/

install.packages("ggplot2")
library(ggplot2)

plt <- ggplot(df, aes(x=df$wbc,y=df$outcome)) + geom_point(color="black")+theme_bw()
plt 

plt2 <- plt + geom_smooth(method = lm, color="red",se=FALSE)
plt2

plt3 <- plt + geom_smooth(method = lm , color = "red", fill="green",se= TRUE)
plt3



