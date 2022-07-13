#Sachleen Kaur, Jan 25, 2022 & Feb 6, 2022, merging clinical data and data dictionary

#files
dd <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/finalDict.csv", head = T)
pcor <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/210508_PCORdeIDdata.csv", head = T)

#______________________MERGING BY ALL CODES_____________________________________

#merging by all codes - Jan 25, 2022
mergeByAllCode <- merge(dd, pcor, by = "code")
table(mergeByAllCode["definition"])

#frequency of top 5 all code merge entries - Jan 25, 2022
print("Top 5 Most Frequent Entries - Merge By All Code")
print("1. Pleural Effusion Not Elsewhere Classified (7035)")
print("2. Herpes simplex without mention of compilcation (6695)")
print("3. Hemoglobin (6653)")
print("4. Mononeuritis multiplex (3325)")
print("5. Drug or chemical induced diabetes mellitus with neurological compilcations with diabetic polyneuropathy (3204)")

#creating plot based on merge of all codes - Jan 25, 2022
frequentData <- data.frame(
  Diagnosis=c("PE","HSV","Hb","MNM","DM") ,  
  Count=c(7035, 6695, 6653, 3325, 3204)
)
ggplot(frequentData, aes(x=Diagnosis, y=Count)) + 
  geom_bar(stat = "identity", color = rgb(0.90, 0.17, 0.31), 
           fill = rgb(0.67, 0.15, 0.31))

#______________________MERGING BY LAB CODES_____________________________________
#UPDATE: FEB 6, 2022

#file: loinc data dictionary
loincLab <- read.csv("/Users/sachleentuteja/Desktop/northwestern/CodeDictionary/loinc.csv", head = T)

#merging by lab codes
mergeByLab <- merge(loincLab, pcor, by = "code")
#mergeByLa2b <- merge(pcor, loinLab, by = "code") 
table(mergeByLab["COMPONENT"])

#frequency of top 5 lab code merge entries
print("Top 5 Most Frequent Entries - Merge By Lab Codes")
print("1. Platelets (7035)")
print("2. Hemoglobin (6695)")
print("3. Hematocrit (6653)")
print("4. Lymphocytes (3325)")
print("5. DNA double strand Ab (3204)")

#creating a plot based on top 5 lab code merge entries
frequentData <- data.frame(
  Lab=c("PLT","Hb","HCT","Lymphs","Anti-dsDNA") ,  
  Count=c(7035, 6695, 6653, 3325, 3204)
)
ggplot(frequentData, aes(x=Lab, y=Count)) + 
  geom_bar(stat = "identity", color = rgb(0.90, 0.17, 0.31), 
           fill = rgb(0.67, 0.15, 0.31))

#avg number of lab tests run per patient
subjectIdLab <- table(mergeByLab['subject_id'])
#sum of every other row
row_odd <- seq_len(nrow(subjectIdLab)) %% 2  
data_row_even <- subjectIdLab[row_odd == 0 ]
sumOfLabFreq <- sum(data_row_even)
#average number of tests ordered for a single patient
avgOfLabFreq <- sumOfLabFreq / 267 

#_________________________MERGING BY DIAG CODES_________________________________

#file: icd 9 and 10 data dictionary
icdDiag <- read.csv("/Users/sachleentuteja/Desktop/northwestern/CodeDictionary/icd.csv", head = T)

#merging by diagnosis codes
mergeByDiag <- merge(icdDiag, pcor, by = "code")
table(mergeByDiag["definition"])

#frequency of top 5 diagnosis code merge entries
print("Top 5 Most Frequent Entries - Merge By Diag Codes")
print("1. Lupus erythematosus (678)")
print("2. Discoid lupus erythematosus (674)")
print("3. Regional enteritis of unspecified site (200)")
print("4. Iron deficiency anemia unspecified (186)")
print("5. Proteinuria unspecified (114)")

#creating a plot based on top 5 diag code merge entries
frequentData <- data.frame(
  Diag = c("LE","DLE","Reg Ent NOC","IDA","PU") ,  
  Count = c(678, 674, 200, 186, 114)
)
ggplot(frequentData, aes(x=Diag, y=Count)) + 
  geom_bar(stat = "identity", color = rgb(0.90, 0.17, 0.31), 
           fill = rgb(0.67, 0.15, 0.31))

#avg number of diag per patient
subjectIdDiag <- table(mergeByDiag['subject_id'])
#sum of every other row
row_odd <- seq_len(nrow(subjectIdDiag)) %% 2  
data_row_even <- subjectIdDiag[row_odd == 0 ]
sumOfDiagFreq<- sum(data_row_even)
#average number of diagnoses for a single patient
avgOfDiagFreq <- sumOfDiagFreq / 267 

#_________________________MERGING BY MEDS CODES_________________________________

#file: rxnorm data dictionary
rxNorm <- read.csv("/Users/sachleentuteja/Desktop/northwestern/CodeDictionary/RxNorm.csv", head = T)

#merging by meds codes
mergeByMeds <- merge(rxNorm, pcor, by = "code")
table(mergeByMeds["definition"])

#frequency of top 2 meds code merge entries
print("Top 2 Most Frequent Entries - Merge By Meds Codes")
print("1. Hydroxychloroquine Sulfate 200 MG Oral Tablet (2553)")
print("2. Hydroxychloroquine Sulfate 200 MG Oral Tablet [Plaquenil] (36)")

#creating a plot based on top 2 meds code merge entries
frequentData <- data.frame(
  Meds = c("HCQ Sulfate 200mg", "Plaquenil") ,  
  Count = c(2553, 36)
)
ggplot(frequentData, aes(x=Meds, y=Count)) + 
  geom_bar(stat = "identity", color = rgb(0.90, 0.17, 0.31), 
           fill = rgb(0.67, 0.15, 0.31))

#avg number of meds per patient
subjectIdMeds <- table(mergeByMeds['subject_id'])
#sum of every other row
row_odd <- seq_len(nrow(subjectIdMeds)) %% 2  
data_row_even <- subjectIdMeds[row_odd == 0 ]
sumOfMedsFreq<- sum(data_row_even)
#average number of meds ordered for a single patient
avgOfMedsFreq <- sumOfMedsFreq / 267

#_________________________MERGING BY PROC CODES_________________________________

#file: rxnorm data dictionary
proc <- read.csv("/Users/sachleentuteja/Desktop/northwestern/CodeDictionary/proc.csv", head = T)

#merging by proc codes
mergeByProc <- merge(proc, pcor, by = "code")
table(mergeByProc["definition"])

#frequency of top 2 meds code merge entries
print("Top 2 Most Frequent Entries - Merge By Proc Codes")
print("1. Detachment at Left Lower Leg, High, Open Approach (5)")
print("2. Detachment at Left 4th Toe, Complete, Open Approach (4)")

#creating a plot based on top 2 proc code merge entries
frequentData <- data.frame(
  Proc = c("Detachment Left Lower Leg", "Detachment Left 4th Toe") ,  
  Count = c(5, 4)
)
ggplot(frequentData, aes(x=Proc, y=Count)) + 
  geom_bar(stat = "identity", color = rgb(0.90, 0.17, 0.31), 
           fill = rgb(0.67, 0.15, 0.31))

#avg number of proc per patient
subjectIdProc <- table(mergeByProc['subject_id'])
#sum of every other row
row_odd <- seq_len(nrow(subjectIdProc)) %% 2  
data_row_even <- subjectIdProc[row_odd == 0 ]
sumOfProcFreq<- sum(data_row_even)
#average number of procedures for a single patient
avgOfProcFreq <- sumOfProcFreq / 6

