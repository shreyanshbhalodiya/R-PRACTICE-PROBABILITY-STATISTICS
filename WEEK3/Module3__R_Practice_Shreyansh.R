df <- read.csv("Student Mental health-2.csv")


colnames(df)[colnames(df) == "Choose.your.gender"] <- "gender"
colnames(df)[colnames(df) == "What.is.your.course."] <- "course"
colnames(df)[colnames(df) == "Your.current.year.of.Study"] <- "year_of_study"
colnames(df)[colnames(df) == "What.is.your.CGPA."] <- "CGPA"
colnames(df)[colnames(df) == "Marital.status"] <- "Marital_status"
colnames(df)[colnames(df) == "Do.you.have.Depression."] <- "Depression"
colnames(df)[colnames(df) == "Do.you.have.Anxiety."] <- "Anxiety"
colnames(df)[colnames(df) == "Do.you.have.Panic.attack."] <- "attack"
colnames(df)[colnames(df) == "Did.you.seek.any.specialist.for.a.treatment."] <- "specialist_treatment"


head(df)

## One test sample test

# One-sample t-test
res <- t.test(df$Age, mu = 23)
# Printing the results
res 


# mean is equal to 23
# t is the t-test statistic value (t = -57.966),
# df is the degrees of freedom (df= 99),
# p-value is the significance level of the t-test (p-value =  2.2e-16).
# conf.int is the confidence interval of the mean at 95% (conf.int = [20.03468, 21.02532]);
# sample estimates is the mean value of the sample (mean = 20.53).


# we can see that our values our not aligned in year of study columns first let's give common name for each unique year

df["year_of_study"][df["year_of_study"] == "year 1"] <- "Year 1"
df["year_of_study"][df["year_of_study"] == "year 2"] <- "Year 2"
df["year_of_study"][df["year_of_study"] == "year 3"] <- "Year 3"


table(df$year_of_study) ### rechecking again


### let's assume that age of males and females in same year of study is same. We will conduct t test for year 1 and year 2 assuming that mean age for male and female students is same for each year. 

df_1 <- df[which(df$year_of_study=='Year 1'),]
df_1_m <- df_1[which(df_1$gender=='Male'),]
df_1_f <- df_1[which(df_1$gender=='Female'),]

t.test(df_1_m$Age, df_1_f$Age)

df_2 <- df[which(df$year_of_study=='Year 2'),]
df_2_m <- df_1[which(df_2$gender=='Male'),]
df_2_f <- df_1[which(df_2$gender=='Female'),]

t.test(df_2_m$Age, df_2_f$Age)


### finally let's check data distribution in box plot. 
boxplot(cbind(df_1_m$Age,df_1_f$Age))
boxplot(cbind(df_2_m$Age,df_2_f$Age)) 





