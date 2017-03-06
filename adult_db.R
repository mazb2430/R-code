adults_db <- read.table(file = "adult.data.txt", sep = ",", 
                        na.strings = c("?", "NA", "-"), strip.white = TRUE, stringsAsFactors = FALSE)

names(adults_db) = c("age","workclass","fnlwgt","education","education_num","marital_status","occupation",
                     "relationship", "race","sex","capital_gain","capital_loss","hours_per_week","native_country",
                     "class")

adults_db$class[adults_db$class==">50K"] <- 1
adults_db$class[adults_db$class=="<=50K"] <- 0

fix(adults_db)

#Count NAs in rows
sapply(adults_db, function(x)sum(is.na(x)))
#clean
clean_adults_data <- na.omit(adults_db)
#Count again for safety reasons
sapply(clean_adults_data, function(x)sum(is.na(x)))

#Make the dataset smaller
set.seed(1013)
idx = sample(1:nrow(clean_adults_data),1000)
limited_rows = clean_adults_data[idx,]
row.names(limited_rows) <- NULL

fix(limited_rows)

#Histogram for age
hist(limited_rows$age[limited_rows$class == 0], breaks = 50,  main = "Age of adults", xlab = "Age", ylab = "Frequency",
     col = "red")

hist(limited_rows$age[limited_rows$class == 1], breaks = 50,  main = "Age of adults", xlab = "Age", ylab = "Frequency"
     , col = "blue", add = T)

legend(x = 70, y = 25, legend = c(">50K", "<=50K"), col = c("blue", "red"), pch = 18)


#Barplot for race
race_plot <- table(limited_rows$race)

barplot(race_plot, main = "Race of adults", xlab = "Race", ylim = c(0, 800), 
        col = c(1:5))

legend(y = 600, x = 1, 
       legend = c("Amer-Indian-Eskimo", "Asian-Pac-Islander", "Black", "Other", "White"),
       fill = c(1:5))


#Boxplot for age
boxplot(limited_rows$age, main = "Age of adults", ylab = "Age", col = "red", outpch = 21, outbg = "black")
boxplot.stats(limited_rows$age)$out

#Standardize the numeric attributes (age, fnlwgt, education_num, capital_gain, capital_loss, hours_peer_week, )
adult_numeric_db <- limited_rows[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_val <- as.numeric(limited_rows[,c("class")])
standardization <- scale(adult_numeric_db)

#PCA
prin_comp <- prcomp(standardization, scale = TRUE, center = TRUE)
prin_comp2<-prin_comp$x
plot(prin_comp2[,1:2], main = "First Two Principal Components", col = (as.numeric(as.factor(class_val))+1), pch=20)

legend(y = 4, x = -7, legend = c("<=50K", ">50K"), fill = c("red", "green"))

pr.var <- prin_comp$sdev^2
pve <- pr.var/sum(pr.var)

#Splitscreen
par(mfrow = c(1,2), oma = c(0,0,2,0))

plot(pve, xlab = "Principal Components", ylab = "Variation", ylim = c(0,1), xlim = c(1,6), 
     type = 'b', col = "red")

plot(cumsum(pve), xlab = "Principal Components", ylab = "Cumulative Variance", ylim = c(0,1),
      xlim = c(1,6), type = 'b', col = "red")

mtext("Proportion of variance explained by Principal Components", outer=TRUE, cex=1.2)

#End splitscreen
par(mfrow = c(1,1))
