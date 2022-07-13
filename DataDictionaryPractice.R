#Sachleen Tuteja - January 22, 2022 - Working with and understanding clinical data

#adding package
library(dplyr)
library(ggplot2)

#reading/opening data set
clinical <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/210508_PCORdeIDdata.csv")
dataDict <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/reviewedCombined.csv")

#frequencies
table(dataDict["attribute"])
table(dataDict["vocab"])
dict <- table(dataDict["Dictnry_qry.Column2"])

print("Top 5 Most Frequent Entries")
print("1. Pleural Effusion Not Elsewhere Classified (7035)")
print("2. Herpes simplex without mention of compilcation (6695)")
print("3. Hemoglobin (6653)")
print("4. Mononeuritis multiplex (3325)")
print("5. Drug or chemical induced diabetes mellitus with neurological compilcations with diabetic polyneuropathy (3204)")

#creating plot 
frequentData <- data.frame(
  Diagnosis=c("PE","HSV","Hb","MNM","DM") ,  
  Count=c(7035, 6695, 6653, 3325, 3204)
    
)
ggplot(frequentData, aes(x=Diagnosis, y=Count)) + 
  geom_bar(stat = "identity", color = rgb(0.90, 0.17, 0.31), 
           fill = rgb(0.67, 0.15, 0.31))