library("readxl")
df <- read_excel("Practice_Assignment_Data.xls")

df <- na.omit(df)  ## REMOVING NULL VALUES
sum(is.na(df$age))
### https://sparkbyexamples.com/r-programming/remove-rows-with-na-in-r/

colnames(df)
columns <- c('smoker','hrt_months','wbc','age','outcome')  
df_1 <- df[columns] 

str(df_1) 
cor(df_1)


head(df_1)
install.packages("jtools")
## https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

library(jtools) # Load jtools
# Telling R we want to use this data
fit <- lm(wbc ~ smoker, data = df_1)
summ(fit)

### correlation between wbc and smoker we got is 0.177 and R-value we got for the same is 0.03 which again not strong. 
library(jtools) # Load jtools
# Telling R we want to use this data
fit <- lm(outcome ~ age, data = df_1)
summ(fit)


library(jtools) # Load jtools
# Telling R we want to use this data
fit <- lm(smoker ~ wbc + age + outcome, data = df_1)
summ(fit)





