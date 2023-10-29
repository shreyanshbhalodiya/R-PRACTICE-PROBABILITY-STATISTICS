

url <- "https://vincentarelbundock.github.io/Rdatasets/csv/AER/CreditCard.csv"
df <- read.csv(url)


head(df)
print(colnames(df))
print(dim(df)) 
paste(sum(is.na(df)),", dataset has zero null value")  ## dataset has zero null value
## dataset has zero null values. 

summary(df)  ## summary of datarame
str(df)   ## data structure

df$age <- as.integer(df$age)  ## converted age into interger from float
str(df)  ## checking structure again. 


df_2 <- data.frame(aggregate(df$expenditure, list(df$selfemp), FUN=sum)) 
df_2 <- setNames(df_2, c("selfemp","exp"))
df_2
barplot(df_2$exp, names.arg=df_2$selfemp ,xlab="selfemp",ylab="expenditure",col="blue", main="self employed yes no expendtiture",border="red")
### self employed no has maximum expenditure as compare to self employed yes

input <- df[,c('selfemp','dependents')]
print(head(input))

png(file = "boxplot.png")

# Plot the chart.
boxplot(dependents ~ selfemp, data = input, xlab = "self employed",
        ylab = "dependents", main = "boxplot for outlier analysis")

# Save the file.
dev.off()


hist(df$dependents)  ### histogram.
hist(df$months)   ### months.
