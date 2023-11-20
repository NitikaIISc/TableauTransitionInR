#### Soft code for other companies

setwd("C:/UCLAAnderson/10KfilingsTables/Data")

data1<-read.csv("August25_TCFDCompCorrected_DataOutput1RecenctYearOnlyTruncatedAggregation.csv", header=T)
colnames(data1)
data=data1


data2<-read.csv("August25_nonNACompCorrected_PivotedDataOutput2.csv", header=T)
colnames(data2)

##The tableau file has diffrent metric names as compared to raw data so 
######### Step 1: Replace ONLY the colnames of metrics that are used in the Tableau dashboard ################
unique(data2$Cleaned_names)
subset(data2, Cleaned_names=="Employee Ethnic Diversity")


variable_mapping<-unique(data2[,c("Variable_name", "Cleaned_names")]) ##creating a map between raw data colnames that correspond to metrics and cleaned names for Tableau 


colnames(data) <- variable_mapping[match(colnames(data1), variable_mapping[,1]), 2]

df=data1
df2=variable_mapping
# library(dplyr)
# library(tibble)
# 
# 
# setdiff(colnames(df), df2$Variable_name)
# df %>% 
#   rename_with(~deframe(df2)[.x], .cols = df2$Variable_name) %>% 
#   select(colnames(df), any_of(df2$Cleaned_names))

####replacing colnames of metrics with Tableau cleaned metric names without hindering any other colnames or chning them to NA as match was doing

v <- colnames(df) %in% df2$Variable_name
w <- df2$Variable_name %in% colnames(df)
colnames(df)[v] <- df2$Cleaned_names[w]

head(df)
colnames(df)

KBH<-subset(df, ExternalReference=="KBH2022")
yellowBar<-KBH$`Employee Ethnic Diversity`*100##Changing proportion to percentage
selectedsectors<-round(((mean(subset(df, GICS.Sector == (KBH$GICS.Sector))$`Employee Ethnic Diversity`,na.rm=TRUE))*100),0)

SnPBar<-round(((mean(df$`Employee Ethnic Diversity`,na.rm=TRUE))*100),0)

plot1Data<-as.data.frame(rbind(KBH = yellowBar, `Selected Sectors`=selectedsectors, `S&P 500` = SnPBar))
colnames(plot1Data)<-"Employee Ethnic Diversity"


plot1Data<-as.data.frame(rbind(cbind(axisLabel = rep("KBH",nrow(KBH)),"Employee Ethnic Diversity" = yellowBar), 
                               cbind(axisLabel = rep("Selected Sectors",length(selectedsectors)),"Employee Ethnic Diversity" = selectedsectors), 
                               cbind(axisLabel = rep("S&P 500",length(SnPBar)),"Employee Ethnic Diversity"=SnPBar)))

plot1Data$axisLabel <- factor(plot1Data$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

plot1Data$cat<-rownames(plot1Data)
##manually assign colors to each group
group.colors <- c(KBH = "#FFD100", `Selected Sectors` = "#005587", `S&P 500` ="#8BB8E8")

# Barplot

class(plot1Data$`Employee Ethnic Diversity`)

plot1Data$`Employee Ethnic Diversity`<-as.numeric(as.character(plot1Data$`Employee Ethnic Diversity`))

# lock in factor level order
plot1Data$cat <- factor(plot1Data$cat, levels = plot1Data$cat)


library(ggplot2)
setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plot1EthnicityDisclosure.tiff", units="in", width=6.75, height=5, res=300)

ggplot(plot1Data, aes(x=axisLabel, y=`Employee Ethnic Diversity`, fill=as.factor(`axisLabel`), label = `Employee Ethnic Diversity`)) +
  geom_bar(aes(y=`Employee Ethnic Diversity`),stat="identity") + 
  geom_text(aes(label = paste(plot1Data$`Employee Ethnic Diversity`,"%",sep="")), vjust = 4.5, 
            colour = "black", size=8, face="bold") +
 # scale_fill_manual(values=c("#FFD100", "#005587", "#8BB8E8"))+
  scale_fill_manual(values=group.colors)+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 25))+
  theme(axis.title.x=element_blank())+ # All font sizes
  theme(axis.text.x = element_text(size=20, face="bold", colour = "black"))+
  theme(axis.title.y=element_blank())+ # All font sizes
  theme(legend.position="none")+##remove all legends
  ggtitle("Ethnicity Disclosure Rate") +
  theme(plot.title = element_text(size=25,hjust = 0.5, face="bold"))+
  theme(axis.text=element_text(size=20, face="bold"),axis.title=element_text(size=20,face="bold"))

dev.off()
############## Plot 2 #############################




###Employee ethnicity #######

ethnicity<-df[,c("ExternalReference","Company.Name","GICS.Sector","CM11c...White","CM11c...Black","CM11c...Hispanic","CM11c...Asian")]
KBH<-subset(ethnicity, ExternalReference=="KBH2022")

library(tidyr)
ethnicity_pivot<-ethnicity %>%
  pivot_longer(
    cols = starts_with("CM11c."),
    names_to = "ethnicity_cat",
    # names_pattern = "^(.*?)(\\d+)$",
    names_prefix = "wk",
    values_to = "value",
    values_drop_na = FALSE
  )

ethnicity_pivot$ethnicity_cat <- factor(ethnicity_pivot$ethnicity_cat, levels=c('CM11c...White', 'CM11c...Black', 'CM11c...Hispanic', 'CM11c...Asian'))

# ethnicity_pivot$axisLabel <- factor(ethnicity_pivot$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

##Removing the "CM11c..." so that ggplot labels only have ethnicities
ethnicity_pivot$ethnicity_cat<-gsub( "CM11c...", "", as.character(ethnicity_pivot$ethnicity_cat))

KBH2<-subset(ethnicity_pivot, ExternalReference=="KBH2022")

yellowBar<-KBH2$value##Changing proportion to percentage

##find separate mean for white, black, hispanic, asian--if you have plyr loaded specify dplyr for summarise
library(dplyr)
selectedsectors<-(subset(ethnicity_pivot, GICS.Sector == (KBH2$GICS.Sector)))%>%
                    group_by(ethnicity_cat) %>%
                    dplyr::summarise(value = round(mean(value, na.rm=TRUE),0))
                  
                  
SnPBar<-ethnicity_pivot %>%
  group_by(ethnicity_cat) %>%
  dplyr::summarise(value = round(mean(value, na.rm=TRUE),0),.groups = 'drop')

##selectedsectors<-round(((mean(subset(ethnicity_pivot, GICS.Sector == (KBH2$GICS.Sector))$value,na.rm=TRUE))),1)

##SnPBar<-round(((mean(df$`Employee Ethnic Diversity`,na.rm=TRUE))*100),0)

plot2Data<-as.data.frame(rbind(cbind(axisLabel = rep("KBH",nrow(KBH2)),KBH2[c("ethnicity_cat","value")]), 
                               cbind(axisLabel = rep("Selected Sectors",nrow(selectedsectors)),selectedsectors), 
                               cbind(axisLabel = rep("S&P 500",nrow(SnPBar)),SnPBar)))

plot2Data$axisLabel <- factor(plot2Data$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

# colnames(plot2Data)<-"EmployeeSByEthnicGroup"
# plot2Data$ethnicity_cat<-rownames(plot2Data)
# Barplot

class(plot2Data$value)

# plot2Data$`Employee Ethnic Diversity`<-as.numeric(as.character(plot1Data$`Employee Ethnic Diversity`))

# lock in factor level order
# plot1Data$cat <- factor(plot1Data$cat, levels = plot1Data$cat)


library(ggplot2)
##manually assign colors to each group
group.colors <- c(KBH = "#FFD100", `Selected Sectors` = "#005587", `S&P 500` ="#8BB8E8")

plot2Data$axisLabel <- factor(plot2Data$axisLabel, levels = c("S&P 500","Selected Sectors","KBH"))

plot2Data$ethnicity_cat <- factor(plot2Data$ethnicity_cat, levels = c("Asian","Hispanic","Black","White"))

# plot2Data$value<-paste(plot2Data$value,"%",sep="")  

###Saving high def image ###


setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd/KellyCorrection")
tiff("Aug30_plot2NoLegendEthnicityPercent.tiff", units="in", width=6.75, height=5, res=300)

ggplot(plot2Data, aes(x=ethnicity_cat, y=value, fill=axisLabel, label = value)) +
  geom_bar(aes(y=value),stat="identity", width = .7, 
           position = position_dodge(preserve = "single")) + 
  # geom_text(aes(label = paste(plot2Data$value,"%")), vjust = 0.5, colour = "black", size=1) +
  geom_text(position = position_dodge(width= .7),  size=5.5, aes(y=value+0.25, fill=axisLabel, label=paste0(value,"%", sep=""), hjust=-0.15, face="bold"))+
  scale_fill_manual(values=group.colors)+
  #scale_fill_viridis_c(option = 'E', direction = -1)+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 15))+
 theme(axis.title.x=element_blank())+ # All font sizes
 theme(axis.title.y=element_blank())+ # All font sizes
  theme(plot.title = element_text(size=25, face= "bold", colour= "black" ),
#axis.text.x = element_text(size=14, face="bold", colour = "black"),    
axis.text.y = element_text(size=20, face="bold", colour = "black")
)+
  theme(legend.position="none")+##remove all legends
  ggtitle("% of Employees by Ethnic Group") +
  theme(plot.title = element_text(hjust = 0.5, size=25))+
  ylim (NA, (max(plot2Data$value,na.rm=T)+6.5))+
  # scale_x_discrete(labels=c('Asian', 'Hispanic', 'Black','White'))+<<<<Shouldn't force write the axes labels
  theme(legend.title=element_blank())+##remove legend title
  coord_flip()


dev.off()


############Plot 3 : gender diversity disclosure ###########


df$`Employee Gender Diversity`


KBH<-subset(df, ExternalReference=="KBH2022")
yellowBar<-KBH$`Employee Gender Diversity`*100##Changing proportion to percentage
selectedsectors<-round(((mean(subset(df, GICS.Sector == (KBH$GICS.Sector))$`Employee Gender Diversity`,na.rm=TRUE))*100),0)

SnPBar<-round(((mean(df$`Employee Gender Diversity`,na.rm=TRUE))*100),0)

plot3Data<-as.data.frame(rbind(KBH = yellowBar, `Selected Sectors`=selectedsectors, `S&P 500` = SnPBar))
colnames(plot3Data)<-"Employee Gender Diversity"
plot3Data$cat<-rownames(plot3Data)
# Barplot

class(plot3Data$`Employee Gender Diversity`)

plot3Data$`Employee Gender Diversity`<-as.numeric(as.character(plot3Data$`Employee Gender Diversity`))

# lock in factor level order
plot3Data$cat <- factor(plot3Data$cat, levels = plot3Data$cat)


library(ggplot2)
###Saving high def image ###


setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plot3GenderDisclosure.tiff", units="in", width=6.75, height=5/2, res=300)

ggplot(plot3Data, aes(x=cat, y=`Employee Gender Diversity`, fill=as.factor(`Employee Gender Diversity`), label = `Employee Gender Diversity`)) +
  geom_bar(aes(y=`Employee Gender Diversity`),stat="identity") + 
  geom_text(aes(label = paste(plot3Data$`Employee Gender Diversity`,"%",sep="")), vjust = 3.5, 
            colour = "black", size=7, face="bold") +
  scale_fill_manual(values=c("#005587", "#8BB8E8","#FFD100"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 5))+
  theme(axis.title.x=element_blank())+ # All font sizes
  theme(axis.text.x = element_text(size=20, face="bold", colour = "black"))+
  theme(axis.title.y=element_blank())+ # All font sizes
  theme(legend.position="none")+##remove all legends
  ggtitle("Gender Disclosure Rate") +
  theme(plot.title = element_text(size=25,hjust = 0.5, face="bold"))+
  theme(axis.text=element_text(size=10, face="bold"),axis.title=element_text(size=24,face="bold"))
dev.off()



######### Plot 4: % of women employees #################

colnames(df)


df$`% Employee gender diversity`


KBH<-subset(df, ExternalReference=="KBH2022")
yellowBar<-KBH$`% Employee gender diversity`##Changing proportion to percentage
selectedsectors<-round(((mean(subset(df, GICS.Sector == (KBH$GICS.Sector))$`% Employee gender diversity`,na.rm=TRUE))),0)

SnPBar<-round(((mean(df$`% Employee gender diversity`,na.rm=TRUE))),0)

plot4Data<-as.data.frame(rbind(KBH = yellowBar, `Selected Sectors`=selectedsectors, `S&P 500` = SnPBar))
colnames(plot4Data)<-"% Employee gender diversity"
plot4Data$cat<-rownames(plot4Data)
# Barplot

class(plot4Data$`% Employee gender diversity`)

plot4Data$`% Employee gender diversity`<-as.numeric(as.character(plot4Data$`% Employee gender diversity`))

# lock in factor level order
plot4Data$cat <- factor(plot4Data$cat, levels = plot4Data$cat)
##manually assign colors
group.colors <- c(KBH = "#FFD100", `Selected Sectors` = "#005587", `S&P 500` ="#8BB8E8")


library(ggplot2)
###Saving high def image ###


setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plot4WomenPercentage.tiff", units="in", width=6.75, height=5/2, res=300)

ggplot(plot4Data, aes(x=cat, y=`% Employee gender diversity`, fill=as.factor(cat), label = `% Employee gender diversity`)) +
  geom_bar(aes(y=`% Employee gender diversity`),stat="identity") + 
  geom_text(aes(label = paste(plot4Data$`% Employee gender diversity`,"%",sep="")), 
            vjust = 3.5, colour = "black", size=7) +
  scale_fill_manual(values=c(`KBH` = "#FFD100", `Selected Sectors` = "#005587", `S&P 500` ="#8BB8E8"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 5))+
  theme(axis.title.x=element_blank())+ # All font sizes
  theme(axis.text.x = element_text(size=20, face="bold", colour = "black"))+
  theme(axis.title.y=element_blank())+ # All font sizes
  theme(legend.position="none")+##remove all legends
  ggtitle("% of Women employees") +
  theme(plot.title = element_text(size=25,hjust = 0.5, face="bold"))+
  theme(axis.text=element_text(size=10, face="bold"),axis.title=element_text(size=24,face="bold"))
dev.off()



########## Plot 5: CEO to Median ratio #########

colnames(df)
KBH<-subset(df, ExternalReference=="KBH2022")

KBH$`CEO to median employee compensation ratio`

hist(df$`CEO to median employee compensation ratio`)


###Adding yellow to highlight bin containing company: ######
plot5data=subset(df,GICS.Sector == (KBH$GICS.Sector))


###Remove everything after a colon
plot5data$`CEO to median employee compensation ratio`<-gsub(":.*","",plot5data$`CEO to median employee compensation ratio`)
plot5data$`CEO to median employee compensation ratio`<-round(as.numeric(as.character(plot5data$`CEO to median employee compensation ratio`)),0)
max(plot5data$`CEO to median employee compensation ratio`,na.rm=T)


# KBH$`CEO to median employee compensation ratio`
# plot5data$col <- ifelse(plot5data$ExternalReference == "KBH2022" & plot5data$`CEO to median employee compensation ratio` == KBH$`CEO to median employee compensation ratio`, T, F)
# plot5data$col <- ifelse(plot5data$ExternalReference != "KBH2022" & plot5data$`CEO to median employee compensation ratio` != KBH$`CEO to median employee compensation ratio`, T, plot5data$color)


# 
# plot5data$cuts <- cut(plot5data$`CEO to median employee compensation ratio`, 30, labels = F)
# 
# A_colored_cuts <- unique(plot5data$cuts[plot5data$`CEO to median employee compensation ratio` == KBH$`CEO to median employee compensation ratio`])
# plot5data$color <- ifelse(plot5data$ExternalReference == "KBH2022" & plot5data$cuts == A_colored_cuts, T, F)
# # 
# B_colored_cuts <- unique(plot5data$cuts[plot5data$Value == obs.B])
# plot5data$color <- ifelse(plot5data$Var == "B" & plot5data$cuts == B_colored_cuts, T, plot5data$color)

plot5data$color <- ifelse(plot5data$ExternalReference == "KBH2022" & plot5data$`CEO to median employee compensation ratio` == KBH$`CEO to median employee compensation ratio`, T, F)

plot5data$color <- ifelse(plot5data$ExternalReference != "KBH2022" & plot5data$Value != KBH$`CEO to median employee compensation ratio`, T, data$color)

#plot5data$color3<- ifelse(plot5data$`CEO to median employee compensation ratio` >100 &plot5data$`CEO to median employee compensation ratio` <150, T, F)
#plot5data$color2<- ifelse(plot5data$`CEO to median employee compensation ratio` <100 | plot5data$`CEO to median employee compensation ratio` >150, T, data$color)

### If the value lies within 50 bin of 131 of KBH
KBH$`CEO to median employee compensation ratio`

plot5data$color <- ifelse(plot5data$`CEO to median employee compensation ratio`>125 & plot5data$`CEO to median employee compensation ratio` < 175, T, F)

subset(plot5data, ExternalReference=="KBH2022")


# lock in factor level order
plot5data$color <- factor(plot5data$color, levels = plot5data$color)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plot5CEOmedianRatio.tiff", units="in", width=12, height=5, res=300)


ggplot(plot5data, 
       aes(x=`CEO to median employee compensation ratio`)) + 
  # geom_histogram(data=subset(plot5data, `CEO to median employee compensation ratio`!=gsub(":.*","",KBH$`CEO to median employee compensation ratio`)), color="white", aes(x=`CEO to median employee compensation ratio`), fill = "#005587",binwidth=50) +
  # geom_histogram(data=subset(plot5data, `CEO to median employee compensation ratio`==gsub(":.*","",KBH$`CEO to median employee compensation ratio`)), aes(x=`CEO to median employee compensation ratio`), fill="#FFD100", binwidth=50)+
  geom_histogram(binwidth=50, color="white",
                 aes(x=`CEO to median employee compensation ratio`,
                     #fill=factor(ifelse(`CEO to median employee compensation ratio`!=(subset(plot5data, ExternalReference=="KBH2022"))$`CEO to median employee compensation ratio`,"Normal","Highlighted")))
                     fill=color)) +
 # scale_fill_manual(name = "CEO to median employee compensation ratio", values=c("Normal"="#005587","Highlighted"="#FFD100")) +
  scale_fill_manual(name = "CEO to median employee compensation ratio", values=c("TRUE"="#FFD100","FALSE"="#005587")) +
  xlim(NA, 2000)+
  ylab("# of Companies")+
  xlab("Ratio of CEO compensation to Median Employee Wage")+
  theme(axis.title.y=element_blank())+ # All font sizes
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=20, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=20, face="bold", colour = "black"))+
  theme(axis.text.x = element_text(size=20, face="bold", colour = "black"))+
  theme(axis.text.y = element_text(size=20, face="bold", colour = "black"))+
  ggtitle("CEO to Median Employee Compensation Ratio")+
  theme(plot.title = element_text(size=25,hjust = 0.5, face="bold"))+
  theme(legend.position="none")

dev.off()
# subset(df,`CEO to median employee compensation ratio`=="40668")
# 
# subset(data1,SEC.Filing.Name == "Tesla, Inc.")

df$`CEO to median employee compensation ratio`<-gsub(":.*","",df$`CEO to median employee compensation ratio`)

summary_df=df
##Create new column to categorize KBH, Selected sectors and S&P500
summary_df$cat<-"NA"

summary_df$cat <- ifelse(summary_df$ExternalReference=="KBH2022","KBH", ifelse(summary_df$GICS.Sector == KBH$GICS.Sector & summary_df$ExternalReference!="KBH2022","Selected Sectors","S&P500"))

###KBH will also be aprt of consumer discretionary so you should not remove KBH from this calculation
SelectedSectorMean = mean(subset(summary_df, GICS.Sector == KBH$GICS.Sector)$`CEO to median employee compensation ratio`,na.rm=T)
tail(subset(summary_df, GICS.Sector == KBH$GICS.Sector))
subset(summary_df, SEC.Filing.Name=="Tesla, Inc.")$`CEO to median employee compensation ratio`

library(dplyr)
value=summary_df$`CEO to median employee compensation ratio`

summary_df$`CEO to median employee compensation ratio`<-as.numeric(as.character(summary_df$`CEO to median employee compensation ratio`))
setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd/KellyCorrection")
write.csv(summary_df[,c("SEC.Filing.Name","GICS.Sector", "cat","CEO to median employee compensation ratio")],"CEOMedianSectorWiseRatio.csv")

Summary_CEO<-t(summary_df%>%
  group_by(cat) %>%
  dplyr::summarise(Mean = round(mean(`CEO to median employee compensation ratio`, na.rm=TRUE),1),Median = round(median(`CEO to median employee compensation ratio`, na.rm=TRUE),1)))
setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
write.csv(Summary_CEO,"Aug25_summaryCEO_dashboard1.csv")




############## Dashboard page 2 ###########################

setwd("C:/UCLAAnderson/10KfilingsTables/Data")

data1<-read.csv("August25_TCFDCompCorrected_DataOutput1RecenctYearOnlyTruncatedAggregation.csv", header=T)
colnames(data1)
data=data1

data2<-read.csv("August25_nonNACompCorrected_PivotedDataOutput2.csv", header=T)
colnames(data2)

##The tableau file has diffrent metric names as compared to raw data so 
######### Step 1: Replace ONLY the colnames of metrics that are used in the Tableau dashboard ################
unique(data2$Cleaned_names)
subset(data2, Cleaned_names=="Employee Ethnic Diversity")


variable_mapping<-unique(data2[,c("Variable_name", "Cleaned_names")]) ##creating a map between raw data colnames that correspond to metrics and cleaned names for Tableau 

write.csv(variable_mapping, "variable_mapping.csv")

colnames(data) <- variable_mapping[match(colnames(data1), variable_mapping[,1]), 2]

df=data1
df2=variable_mapping
# library(dplyr)
# library(tibble)
# 
# 
# setdiff(colnames(df), df2$Variable_name)
# df %>% 
#   rename_with(~deframe(df2)[.x], .cols = df2$Variable_name) %>% 
#   select(colnames(df), any_of(df2$Cleaned_names))

####replacing colnames of metrics with Tableau cleaned metric names without hindering any other colnames or chning them to NA as match was doing

v <- colnames(df) %in% df2$Variable_name
w <- df2$Variable_name %in% colnames(df)
colnames(df)[v] <- df2$Cleaned_names[w]

head(df)
colnames(df)

KBH<-subset(df, ExternalReference=="KBH2022")

## Plot 1 : GHG Scopes #############



############## Plot B1 #############################


colnames(df)
df$`CM7b.Uncategorized.`
df$`CM7b.Scope.2..market`
df$CM7a.Response..in.me
subset(df, ExternalReference == "KBH2022")$CM7a.Response..in.me
###GHGScopes123 #######

GHGScopes123<-df[,c("ExternalReference","Company.Name","GICS.Sector","CM7a.Response..in.me","CM7b.Scope.2..locati","CM7c.Response..metri")]##KBHomes does not have "CM7b.Scope.2..market"
head(GHGScopes123)

GHGScopes123$CM7b.Scope.2..locati<-as.numeric(gsub(",","",GHGScopes123$CM7b.Scope.2..locati))
colnames(GHGScopes123)[4:6]<-c("Scope 1 GHG Emmissions","Scope 2 GHG Emmissions","Scope 3 GHG Emmissions")

KBH<-subset(GHGScopes123, ExternalReference=="KBH2022")

library(tidyr)
GHGScopes123_pivot<-GHGScopes123 %>%
  pivot_longer(
    cols = starts_with("Scope "),
    names_to = "GHGScopes123_cat",
    # names_pattern = "^(.*?)(\\d+)$",
    names_prefix = "wk",
    values_to = "value",
    values_drop_na = FALSE
  )

GHGScopes123_pivot$GHGScopes123_cat <- factor(GHGScopes123_pivot$GHGScopes123_cat, levels=c("Scope 3 GHG Emmissions","Scope 2 GHG Emmissions","Scope 1 GHG Emmissions"))

# GHGScopes123_pivot$axisLabel <- factor(GHGScopes123_pivot$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

##Removing the "CM11c..." so that ggplot labels only have ethnicities
#GHGScopes123_pivot$GHGScopes123_cat<-gsub( "CM11c...", "", as.character(GHGScopes123_pivot$GHGScopes123_cat))

KBH2<-subset(GHGScopes123_pivot, ExternalReference=="KBH2022")%>%
  group_by(GHGScopes123_cat) #%>%
  # dplyr::summarise(value = round(mean(value, na.rm=TRUE),0))%>%
  # mutate(value = prettyNum(value,big.mark=",",scientific=FALSE))

yellowBar<-KBH2$value##Changing proportion to percentage

##find separate mean for white, black, hispanic, asian
library(dplyr)
selectedsectors<-(subset(GHGScopes123_pivot, GICS.Sector == unique(KBH2$GICS.Sector)))%>%
  group_by(GHGScopes123_cat) %>%
  dplyr::summarise(value = round(mean(value, na.rm=TRUE),0))#%>%
  # mutate(value = prettyNum(value,big.mark=",",scientific=FALSE))



SnPBar<-GHGScopes123_pivot %>%
  group_by(GHGScopes123_cat) %>%
  dplyr::summarise(value = round(mean(value, na.rm=TRUE),0))#%>%
  # mutate(value = prettyNum(value,big.mark=",",scientific=FALSE))##added commas and removed scientific notations



##selectedsectors<-round(((mean(subset(GHGScopes123_pivot, GICS.Sector == (KBH2$GICS.Sector))$value,na.rm=TRUE))),1)

##SnPBar<-round(((mean(df$`Employee Ethnic Diversity`,na.rm=TRUE))*100),0)

plotB1Data<-as.data.frame(rbind(cbind(axisLabel = rep("KBH",nrow(KBH2)),KBH2[c("GHGScopes123_cat","value")]), 
                               cbind(axisLabel = rep("Selected Sectors",nrow(selectedsectors)),selectedsectors), 
                               cbind(axisLabel = rep("S&P 500",nrow(SnPBar)),SnPBar)))

plotB1Data$axisLabel <- factor(plotB1Data$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

# colnames(plotB1Data)<-"EmployeeSByEthnicGroup"
# plotB1Data$GHGScopes123_cat<-rownames(plotB1Data)
# Barplot

class(plotB1Data$value)

# plotB1Data$Millvalue<-ifelse(plotB1Data$value< 1000000, plotB1Data$value, round((plotB1Data$value/1000000),1))####to convert to million
# plotB1Data$`Employee Ethnic Diversity`<-as.numeric(as.character(plot1Data$`Employee Ethnic Diversity`))

# lock in factor level order
# plot1Data$cat <- factor(plot1Data$cat, levels = plot1Data$cat)


library(ggplot2)
##manually assign colors to each group
group.colors <- c(KBH = "#FFD100", `Selected Sectors` = "#005587", `S&P 500` ="#8BB8E8")

plotB1Data$axisLabel <- factor(plotB1Data$axisLabel, levels = c("S&P 500","Selected Sectors","KBH"))
# plotB1Data$value<-paste(plotB1Data$value,"%",sep="")  

###Saving high def image ###


setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plotB1NoLegendGHGScopes123Percent.tiff", units="in", width=8.75, height=5, res=300)

ggplot(plotB1Data, aes(x=GHGScopes123_cat, y=value, fill=axisLabel, label = value)) +
  geom_bar(aes(y=value),stat="identity", width = .7, 
           position = position_dodge(preserve = "single")) + 
  # geom_text(aes(label = paste(plotB1Data$value,"%")), vjust = 0.5, colour = "black", size=1) +
  geom_text(position = position_dodge(width= .7),  size=4.5, aes(y=value+0.25, fill=axisLabel, label=paste0(prettyNum(value,big.mark=",",scientific=FALSE),"", sep=""), hjust=-0.05, face="bold"))+
  scale_fill_manual(values=group.colors)+
  ylim (NA, (max(plotB1Data$value,na.rm=T)+4500000))+
  #scale_fill_viridis_c(option = 'E', direction = -1)+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 10))+
  theme(axis.title.x=element_blank())+ # All font sizes
  theme(axis.title.y=element_blank())+ # All font sizes
  theme(plot.title = element_text(size=15, face= "bold", colour= "black" ),
        axis.text.x = element_blank(),    
        axis.text.y = element_text(size=15, face="bold", colour = "black")
  )+
  theme(legend.position="none")+##remove all legends
  ggtitle("Average GHG Emissions per Company \n (in metric tons)") +
  theme(plot.title = element_text(hjust = 0.5, size=25))+
  scale_x_discrete(labels=c("Scope 3 \nGHG Emmissions","Scope 2 \nGHG Emmissions","Scope 1 \nGHG Emmissions"))+
  theme(legend.title=element_blank())+##remove legend title
  #geom_errorbarh(aes(xmax=as.numeric(value)+0.45,xmin=as.numeric(value)-0.45,height=0,color="yellow"),position=position_dodge(width=0.9))+
  coord_flip()


dev.off()

##add yellow bars for small values manually

############# Plot B2##########


colnames(df)
df$`CM7a.GHG.Emissions.`
df$`CM7b.GHG.Emissions.`
df$CM7c.GHG.Emissions.
subset(df, ExternalReference == "KBH2022")$CM7a.Response..in.me
###GHGDisclosures #######

GHGDisclosures<-df[,c("ExternalReference","Company.Name","GICS.Sector","CM7a.GHG.Emissions.","CM7b.GHG.Emissions.","CM7c.GHG.Emissions.")]
head(GHGDisclosures)

colnames(GHGDisclosures)[4:6]<-c("Scope 1 GHG Disclosure","Scope 2 GHG Disclosure","Scope 3 GHG Disclosure")

KBH<-subset(GHGDisclosures, ExternalReference=="KBH2022")

library(tidyr)

GHGDisclosures_pivot<-GHGDisclosures %>%
  pivot_longer(
    cols = starts_with("Scope "),
    names_to = "GHGDisclosures_cat",
    # names_pattern = "^(.*?)(\\d+)$",
    names_prefix = "wk",
    values_to = "value",
    values_drop_na = FALSE
  )

GHGDisclosures_pivot$GHGDisclosures_cat <- factor(GHGDisclosures_pivot$GHGDisclosures_cat, levels=c("Scope 1 GHG Disclosure","Scope 2 GHG Disclosure","Scope 3 GHG Disclosure"))

# GHGDisclosures_pivot$axisLabel <- factor(GHGDisclosures_pivot$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

##Removing the "CM11c..." so that ggplot labels only have ethnicities
#GHGDisclosures_pivot$GHGDisclosures_cat<-gsub( "CM11c...", "", as.character(GHGDisclosures_pivot$GHGDisclosures_cat))

KBH2<-subset(GHGDisclosures_pivot, ExternalReference=="KBH2022")
KBH2$value<-KBH2$value*100
yellowBar<-round(KBH2$value,2)*100##Changing proportion to percentage

##find separate mean for white, black, hispanic, asian
library(dplyr)
selectedsectors<-(subset(GHGDisclosures_pivot, GICS.Sector == (KBH2$GICS.Sector)))%>%
  group_by(GHGDisclosures_cat) %>%
  dplyr::summarise(value = round(mean(value, na.rm=TRUE),2)*100)


SnPBar<-GHGDisclosures_pivot %>%
  group_by(GHGDisclosures_cat) %>%
  dplyr::summarise(value = round(mean(value, na.rm=TRUE),2)*100)

##selectedsectors<-round(((mean(subset(GHGDisclosures_pivot, GICS.Sector == (KBH2$GICS.Sector))$value,na.rm=TRUE))),1)

##SnPBar<-round(((mean(df$`Employee Ethnic Diversity`,na.rm=TRUE))*100),0)

plotB2Data<-as.data.frame(rbind(cbind(axisLabel = rep("KBH",nrow(KBH2)),KBH2[c("GHGDisclosures_cat","value")]), 
                                cbind(axisLabel = rep("Selected Sectors",nrow(selectedsectors)),selectedsectors), 
                                cbind(axisLabel = rep("S&P 500",nrow(SnPBar)),SnPBar)))

plotB2Data$axisLabel <- factor(plotB2Data$axisLabel, levels=c('KBH', 'Selected Sectors', 'S&P 500'))

# colnames(plotB2Data)<-"EmployeeSByEthnicGroup"
# plotB2Data$GHGDisclosures_cat<-rownames(plotB2Data)
# Barplot

class(plotB2Data$value)

# plotB2Data$`Employee Ethnic Diversity`<-as.numeric(as.character(plot1Data$`Employee Ethnic Diversity`))

# lock in factor level order
# plot1Data$cat <- factor(plot1Data$cat, levels = plot1Data$cat)


library(ggplot2)
##manually assign colors to each group
group.colors <- c(KBH = "#FFD100", `Selected Sectors` = "#005587", `S&P 500` ="#8BB8E8")

plotB2Data$axisLabel <- factor(plotB2Data$axisLabel, levels = c("KBH","Selected Sectors","S&P 500"))
# plotB2Data$value<-paste(plotB2Data$value,"%",sep="")  



###Saving high def image ###


setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plotB2NoLegendGHGDisclosuresPercent.tiff", units="in", width=6.75, height=5, res=300)

ggplot(plotB2Data, aes(x=GHGDisclosures_cat, y=value, fill=axisLabel, label = value)) +
  geom_bar(aes(y=value),stat="identity", width = .7, 
           position = position_dodge(preserve = "single")) + 
  # geom_text(aes(label = paste(plotB2Data$value,"%")), vjust = 0.5, colour = "black", size=1) +
  geom_text(position = position_dodge(width= .7),  size=5.5, aes(y=value+3.5, fill=axisLabel, label=paste0(value,"%", sep=""), hjust=0.5, face="bold"))+
  scale_fill_manual(values=group.colors)+
  ylim (NA, (max(plotB2Data$value,na.rm=T)+5))+
  #scale_fill_viridis_c(option = 'E', direction = -1)+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 5))+
  theme(axis.title.x=element_blank())+ # All font sizes
  theme(axis.title.y=element_blank())+ # All font sizes
  theme(plot.title = element_text(size=15, face= "bold", colour= "black" ),
        axis.text.x = element_text(size=15, face="bold", colour = "black"),    
        axis.text.y = element_text(size=15, face="bold", colour = "black")
  )+
  theme(legend.position="none")+##remove all legends
  ggtitle("GHG Disclosure Rate") +
  theme(plot.title = element_text(hjust = 0.5, size=22))+
  scale_x_discrete(labels=c("Scope 1 \nGHG Emissions","Scope 2 \nGHG Emissions","Scope 3 \nGHG Emissions"))+
  theme(legend.title=element_blank())
#+ facet_grid(cols = vars(GHGDisclosures_cat))#+##remove legend title
  #coord_flip()


dev.off()


########### Plot B3: GHG per revenue ############

setwd("C:/UCLAAnderson/10KfilingsTables/Data/compustat")
compustat<-read.csv("RevenueSales_2022.csv")

library(dplyr)
colnames(compustat)
colnames(df)

## Compustat has duplicate rows for the same ticker

df_withrev<-left_join(df,subset(compustat, indfmt=="INDL"),by=c("Ticker.Symbol"="tic"))

setwd("C:/UCLAAnderson/10KfilingsTables/Data/")
write.csv(df_withrev, "August25_OFGdata_compustatat2022Rev.csv")

KBH<-subset(df_withrev, ExternalReference=="KBH2022")


plot(((df_withrev$CM7a.Response..in.me)/df_withrev$sale)~df_withrev$revt)


selectedSectors<-subset(df_withrev, GICS.Sector==KBH$GICS.Sector)

head(selectedSectors)
reg<-lm(formula = (CM7a.Response..in.me/revt) ~ revt,
        data=selectedSectors)                      

#get intercept and slope value
coeff<-coefficients(reg)          
intercept<-coeff[1]
slope<- coeff[2]

setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plotB3NoLegendScope1PerRevenue.tiff", units="in", width=12, height=5, res=300)
options(scipen = 999)##this before the plot prevents scientific notation in x axis label
library(scales)
ggplot(selectedSectors,aes(x=revt,y=(CM7a.Response..in.me/revt))) + 
  geom_point(colour="#005587", size=5, shape =16, alpha = 0.5) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  geom_point(data=KBH, aes(x=revt, y=(CM7a.Response..in.me/revt)), colour="#FFD100", fill="#FFD100", size=15, shape="\u2605",alpha=0.95)+
  theme(text = element_text(size = 5))+
  # theme(axis.title.x=element_blank())+ # All font sizes
  # theme(axis.title.y=element_blank())+ # All font sizes
  theme(plot.title = element_text(size=15, face= "bold", colour= "black" ),
        axis.text.x = element_text(size=10, face="bold", colour = "black"),    
        axis.text.y = element_text(size=10, face="bold", colour = "black"),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="bold", colour = "black"))+
  geom_abline(intercept = intercept, slope = slope, color="black",linetype="dashed", size=1.5)+
  ggtitle("GHG Emissions per Revenue vs Sales Turnover") +
  theme(plot.title = element_text(hjust = 0.5, size=25))+ 
  scale_size_area() + 
  ylab("Scope 1 GHG emissions \n per unit revenue")+
  xlab("Revenue (in Millions USD)") 

dev.off()

########### Plot B4: External Assurance #############

colnames(df)
variable_mapping
df$Audited.Report


KBH$Audited.Report

selectedsectors = subset(df, GICS.Sector==KBH$GICS.Sector)

selectedsectors$Audited.Report

selectedsectors$ExternalAssurance<-ifelse(selectedsectors$Audited.Report==0,"Not Audited","Partially or fully Audited")

KBH <- subset(selectedsectors, ExternalReference=="KBH2022")

yellowBar<-KBH$ExternalAssurance##Changing proportion to percentage

##find separate mean for white, black, hispanic, asian
library(dplyr)

KBH2<-as.data.frame(KBH %>%
                            group_by(ExternalAssurance) %>%
                            dplyr::summarise(count = n()))##nO. OF COMPANIES

selectsectors<-as.data.frame(selectedsectors %>%
  group_by(ExternalAssurance) %>%
  dplyr::summarise(count = n()))##nO. OF COMPANIES
colnames(selectsectors)


plotB4Data<-as.data.frame(rbind(cbind(axisLabel = rep("KBH",nrow(KBH)),KBH2[c("ExternalAssurance","count")]), 
                                cbind(axisLabel = rep("Selected Sectors",nrow(selectsectors)),selectsectors) 
                                #cbind(axisLabel = rep("S&P 500",nrow(SnPBar)),SnPBar)
                                ))

plotB4Data$axisLabel <- factor(plotB4Data$axisLabel, levels=c('KBH', 'Selected Sectors'))


plotB4Data$ExternalAssurance <- factor(plotB4Data$ExternalAssurance, levels = c("Partially or fully Audited","Not Audited"))
plotB4Data$axisLabel
group.colors <- c(KBH = "#FFD100", `Selected Sectors` = "#005587")


setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25_plotB4ExternalAssurance.tiff", units="in", width=5, height=5, res=300)

ggplot(plotB4Data, aes(x=ExternalAssurance, y=count, fill=as.factor(`ExternalAssurance`), label = `ExternalAssurance`)) +
  geom_bar(aes(y=`count`),stat="identity") + 
  geom_text(aes(label = paste(plotB4Data$`count`,"",sep="")), vjust = 4.5, 
            colour = "black", size=8, face="bold") +
  theme(axis.title = element_text())+
  ylab("No. of Companies")+
  scale_fill_manual(values=c( "#005587","#FFD100"))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(text = element_text(size = 25))+
  theme(axis.title.x=element_blank())+ # All font sizes
  theme(axis.text.x = element_text(size=15, face="bold", colour = "black"))+
  #theme(axis.title.y=element_blank())+ # All font sizes
  theme(legend.position="none")+##remove all legends
  ggtitle("External Assurance") +
  theme(plot.title = element_text(size=25,hjust = 0.5, face="bold"))+
  theme(axis.text=element_text(size=20, face="bold"),axis.title=element_text(size=20,face="bold"))+ 
  scale_size_area() + 
  scale_x_discrete(labels=c("  Partially or \n Fully Audited","Not Audited"))

dev.off()



################ Plot C1: Zoomed in KB Homes GHG scatter plot only for Real estate industry ############

setwd("C:/UCLAAnderson/10KfilingsTables/Data/")
df_withrev<-read.csv("August25_OFGdata_compustatat2022Rev.csv")

colnames(df_withrev)

length(unique(subset(df_withrev, GICS.Sector == "Information Technology")$Ticker.Symbol))

plot(df_withrev$Overall.Index~df_withrev$revt)

hist(df_withrev$Overall.Index)
hist(df_withrev$revt)

#?cor.test
cor.test(df_withrev$revt, df_withrev$Overall.Index,method="spearman")


hist(log(df_withrev$revt))


model1 = lm(log(revt)~Overall.Index, data = df_withrev) #Create the linear regression

summary(model1)

model2 = lm(log(Overall.Index)~revt, data = df_withrev) #Create the linear regression

summary(model2)

######################## Plot C1: Zoomed in KB Homes GHG scatter plot only for Real estate industry ############
unique(df$GICS.Industry)##"Residential Construction",Real Estate Management & Development
colnames(df)

subset(df, GICS.Industry=="Residential Construction"|GICS.Industry=="Real Estate Management & Development")$Company.Name


RealEstate_df<-subset(df, GICS.Sector=="Real Estate"|ExternalReference=="KBH2022")
RealEstate_df$GICS.Industry
selectedSectors<-RealEstate_df

head(selectedSectors)
nrow(selectedSectors)
length(unique(selectedSectors$Ticker.Symbol))

selectedSectors$Ticker.Symbol<-factor(selectedSectors$Ticker.Symbol)
compustat$tic

setwd("C:/UCLAAnderson/10KfilingsTables/Data/compustat")
compustat<-read.csv("RevenueSales_2022.csv")
subset(compustat, tic=="CBRE")
library(dplyr)
colnames(compustat)
colnames(df)
nrow(subset(compustat, tic=any(selectedSectors$Ticker.Symbol)))
#Compustat seems to have too many duplicates that vary in indfmt = FS or INDL
SelectSector_withrev<-left_join(selectedSectors,subset(compustat, indfmt=="INDL"),by=c("Ticker.Symbol"="tic"),keep = FALSE)
nrow(SelectSector_withrev)

SelectSector_withrev$SEC.Filing.Name
reg<-lm(formula = (CM7a.Response..in.me/revt) ~ revt,
        data=SelectSector_withrev)                      

#get intercept and slope value
coeff<-coefficients(reg)          
intercept<-coeff[1]
slope<- coeff[2]


library(ggplot2)
library(ggrepel)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/TableauInR/DataQCd")
tiff("Aug25c_plotC1AllargsMxOvrlpRepelLabelingZoomedRealEstate_NoLegendScope1PerRevenue.tiff", units="in", width=14, height=8, res=300)

ggplot(SelectSector_withrev,aes(x=revt,y=(CM7a.Response..in.me/revt), label=Company.Name)) + 
  geom_point(data=subset(SelectSector_withrev, ExternalReference!="KBH2022"), aes(x=revt, y=(CM7a.Response..in.me/revt)),colour="#005587", size=5, shape =16, alpha = 0.5) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),## remove grids and background
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  geom_point(data=subset(SelectSector_withrev, ExternalReference=="KBH2022"), aes(x=revt, y=(CM7a.Response..in.me/revt)), colour="#FFD100", fill="#FFD100", size=15, shape="\u2605",alpha=0.75)+
  theme(text = element_text(size = 5))+
  #geom_text(hjust=-0.1, vjust=0.5, size=3)+
  # theme(axis.title.x=element_blank())+ # All font sizes
  # theme(axis.title.y=element_blank())+ # All font sizes
  theme(plot.title = element_text(size=15, face= "bold", colour= "black" ),
        axis.text.x = element_text(size=10, face="bold", colour = "black"),    
        axis.text.y = element_text(size=10, face="bold", colour = "black"),
        axis.title.x = element_text(size=20, face="bold", colour = "black"),    
        axis.title.y = element_text(size=20, face="bold", colour = "black"))+
  geom_abline(intercept = intercept, slope = slope, color="black",linetype="dashed", size=1.5)+
  ggtitle("GHG Emissions per Revenue vs Sales Turnover \n  in the Real Estate Sector") +
  theme(plot.title = element_text(hjust = 0.5, size=21))+ 
  scale_size_area() + 
  ylab("Scope 1 GHG emissions \n per unit revenue")+
  xlab("Revenue (in Millions USD)") +
  geom_text_repel(aes(label = Company.Name), size=3,
                  box.padding = unit(0.20, "lines"),max.time = 0.75,
                  #max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                  max.overlaps = 10,
                  max.iter = 20000)

dev.off()

subset(SelectSector_withrev,Ticker.Symbol=="KBH")

### A way for legible point labeling ###########

##The PCA function produces a plot with the ggplot2 package and uses the ggrepel package to add nice labels for data points. The special thing about ggrepel is that it uses an iterative algorithm to place the labels in a way such that they do not overlap with each other or with the data points (more about ggrepel).

##However, sometimes the data points are too crowded together and the algorithm finds no solution 
##to place all labels. This is what your message means by "1 unlabeled data points".
##If it is no problem for you to have one unlabeled point in the plot, you can just ignore the message.

##There are several options that affect the iterative algorithm's behavior. 
#In your case, I would play around with these and see if they make a difference. 
#Specifically, try: lower box.padding, higher max.time and/or higher max.iter, and 
#different ratios between force and force_pull. You may also want to allow more overlapping labels. 
#The entire page in these links should be very useful, by the way.