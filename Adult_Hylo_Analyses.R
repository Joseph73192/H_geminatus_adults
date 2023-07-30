setwd("C:/Users/hebre/Desktop/Grad School/Thesis")
library(readxl)
library(ggplot2)


###########################################
#######East/West of river graph############
###########################################

adults <- read.csv("adults.csv")

#Get rid of MS and GA records and no length data records:
adults <- subset(adults, (Site.name != "TN: Cumberland Co.") & Total.Length.mm != "na")

#Assign East/West values in a new column for GGPLOT to reference
adults$EW_River <- ifelse(adults$Long<(-85), "West", "East")

t <- ggplot(adults, aes(x=factor(EW_River, level=c('West', 'East')), y=Total.Length.mm)) + geom_boxplot(aes(fill = EW_River)) +
  theme_gray() + scale_fill_brewer(palette="Dark2") 
#Change geomboxplot fill to "Sex" for E vs. W by sex

t + xlab("Apalachicola River") + ylab("Total Length (mm)") + geom_vline(xintercept=1.5, col = "blue", lwd=1) +
  theme(axis.text=element_text(size=17), axis.title=element_text(size=19), legend.position = "none") 

#For Wilcox Testing East vs. West
East <- subset(adults, EW_River == "East")
West <- subset(adults, EW_River =="West")
wilcox.test(East$Total.Length.mm, West$Total.Length.mm, alternative="greater") 

#For Wilcox Testing East Female vs. Male
East_F <- subset(East, Sex=="female")
East_M <- subset(East, Sex=="male")
wilcox.test(East_F$Total.Length.mm, East_M$Total.Length.mm, alternative="greater") 

#For Wilcox Testing West Female vs. Male
West_F <- subset(West, Sex=="female")
West_M <- subset(West, Sex=="male")
wilcox.test(West_F$Total.Length.mm, West_M$Total.Length.mm, alternative="greater") 

#For Wilcox Testing East Male vs. West Male
wilcox.test(East_M$Total.Length.mm, West_M$Total.Length.mm, alternative="greater") 

#For Wilcox Testing East Female vs. West Female
wilcox.test(East_F$Total.Length.mm, West_F$Total.Length.mm, alternative="greater") 

######################################
######Using GIS Attribute table#######
######################################

adults <- read.csv("Adults_watershed.csv", na = "", header = T) #This is spatial join from GIS of just adults
adults <- subset(adults, Total_Length_mm != "NA") #Get rid of individuals w/out total length
adults$Total_Length_mm <- as.numeric(adults$Total_Length_mm) 
adults$HUC_8 <- as.factor(adults$HUC_8)
#All used HUC-8 codes go into a string for later use
hucs <- c('3170007','3160205','3140305','3140105','3140304','3140103','3140104','3140102','3140203','3140101',
          '3130012','3120003','3130011','3130005')
east_huc <- c('3120003','3130011','3130005')
west_huc <- c('3170007','3160205','3140305','3140105','3140304','3140103','3140104','3140102','3140203','3140101',
              '3130012')

t <- ggplot(adults, aes(x=factor(HUC_8, level=hucs), y=Total_Length_mm)) + geom_boxplot(aes(fill = HUC_8)) + theme_bw()
# Change geom_boxplot fill to "Sex" for lengths by sex

t + xlab("Watershed") + ylab("Total Length (mm)") + geom_vline(xintercept=11.5, col = "blue", lwd=1) + 
  geom_hline(yintercept=mean(adults$Total_Length_mm), col = "black", lwd=0.5, linetype="dotdash")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position="none")
#Comment out legend position for boxplot showing sex

west = subset(adults,HUC_8 %in% west_huc) #Adults west of river 
east = subset(adults,HUC_8 %in% east_huc) #Adults east of river

#Kruskal-Wallis test
kruskal.test(adults$Total_Length_mm ~ adults$HUC_8)

#Which ones are significantly different? 
pairwise <- pairwise.wilcox.test(adults$Total_Length_mm, adults$HUC_8, p.adjust.method = "BH")
pairwise <- as.data.frame(pairwise$p.value)
ifelse(pairwise > 0.05, NA, "check") #just for ease of use