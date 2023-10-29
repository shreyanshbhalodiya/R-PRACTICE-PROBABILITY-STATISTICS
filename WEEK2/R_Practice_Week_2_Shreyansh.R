#Name : shreyansh Bhalodiya
#NUID : 002664707


df <- read.csv("Student Mental health-2.csv")

head(df)

dim(df)

## let's first rename the columns because column names are long and not long names are not fit for analysis. 

colnames(df)

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

### Now our dataframe names are short and easy to read and understand. 

print(sum(is.na(df)))   ### checking null values
df  <- na.omit(df)  ### dropping null values
print(sum(is.na(df)))   ##  checking again null values if it is dropped or not
## we have only one value as null which we dropped hence our rest of data is clean 
## let's check age box plot to see if it has any outliers
bx <- boxplot(df$Age, 
              main = "Age boxplot", 
              ylab = "Age")


summary(df)

table(df$gender)
table(df$course)
table(df$year_of_study)
table(df$CGPA)
table(df$Marital_status)
table(df$Depression)
table(df$Anxiety)
table(df$attack)
table(df$specialist_treatment)


counts <- table(df$Depression,df$CGPA)
barplot(counts, main="Depression and CGPA",
        xlab="CGPA", col=c("darkblue","red"),
        legend = rownames(counts),cex.names=0.9, beside=TRUE)

counts <- table(df$Depression,df$gender)
barplot(counts, main="Depression and Gender",
        xlab="Gender", col=c("green","red"),
        legend = rownames(counts),cex.names=0.9, beside=TRUE)


counts <- table(df$Depression,df$Anxiety)
barplot(counts, main="Depression and Anxiety",
        xlab="Anxiety", col=c("black","red"),
        legend = rownames(counts),cex.names=0.9, beside=TRUE)

df_temp <- df
df_temp["Depression"][df_temp["Depression"] == "Yes"] <- 1
df_temp["Depression"][df_temp["Depression"] == "No"] <- 0
#define data
x <- c(df_temp $ Age)
y <- c(df_temp $ Depression)
#create scatter plot of x vs. y
plot(x, y)
#add line of best fit to scatter plot
abline(lm(y ~ x) ,col='red' , lty='dashed')


table(df$year_of_study)
## we can see that our values our not aligned in year of study columns first let's give common name for each unique year

df_tem <- df  ## setting temp dataframe 
df_tem["year_of_study"][df_tem["year_of_study"] == "year 1"] <- "Year 1"
df_tem["year_of_study"][df_tem["year_of_study"] == "year 2"] <- "Year 2"
df_tem["year_of_study"][df_tem["year_of_study"] == "year 3"] <- "Year 3"


table(df_tem$year_of_study) ### rechecking again


newdata <- df_tem[ which(df_tem$Depression=='Yes'), ]
head(newdata)

x <- data.frame(table(df_tem$year_of_study))
x
pie(x$Freq, labels = x$Var1)