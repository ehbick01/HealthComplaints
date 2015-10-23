setwd("C:/Users/michael/Desktop/r analysis/Tattlers")

data  <- read.csv(url('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Health_Complaints.csv'),
                  stringsAsFactors = TRUE)

Health_Complaints <- data

library(ggplot2)
library(ggthemes)
library(lubridate)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)
library(AUC)
library(plyr)
library(randomForest)
library(stargazer)
library(rgdal)
library(zipcode)




#remove the time stamps from the request and resolved dates

Health_Complaints$RequestDate <- sub(" .*",
                                     "",
                                     as.character(Health_Complaints$RequestDate))

Health_Complaints$ResolvedDate <- sub(" .*",
                                      "",
                                      as.character(Health_Complaints$ResolvedDate))


Health_Complaints$NextInspDate <- sub(" .*",
                                      "",
                                      as.character(Health_Complaints$NextInspDate))

#now turn those into dates
Health_Complaints$RequestDate <-  as.Date(ymd(Health_Complaints$RequestDate))
Health_Complaints$ResolvedDate <- as.Date(ymd(Health_Complaints$ResolvedDate))
Health_Complaints$NextInspDate <- as.Date(ymd(Health_Complaints$NextInspDate))

#create a variable to show resolved times
Health_Complaints$ResolvedTimes <- as.numeric((Health_Complaints$ResolvedDate - Health_Complaints$RequestDate)/365)

Health_Complaints$YearSubmitted <- as.factor(year(Health_Complaints$RequestDate))

Health_ComplaintsRecent <- subset(Health_Complaints,
                                  YearSubmitted == "2011" |
                                    YearSubmitted == "2012" |
                                    YearSubmitted == "2013" |
                                    YearSubmitted == "2014" |
                                    YearSubmitted == "2015")


FrequencyComplaint <- as.data.frame(table((Health_ComplaintsRecent$RCodeDescription)))
Health_Complaints <- merge(Health_Complaints,
                           FrequencyComplaint,
                           by.x = "RCodeDescription",
                           by.y = "Var1")




Health_ComplaintsRecent <- subset(Health_ComplaintsRecent,
                            Duplicate == "NO")


#table to show frequency of complaint types
FrequencyRecentHealthComplaints <- as.data.frame(table(Health_ComplaintsRecent$RCodeDescription))
colnames(FrequencyRecentHealthComplaints)[2] <- "Frequency"
colnames(FrequencyRecentHealthComplaints)[1] <- "ComplaintType"
FrequencyRecentHealthComplaints <- FrequencyRecentHealthComplaints[with(FrequencyRecentHealthComplaints,
                                                                                order(-Frequency,
                                                                                      ComplaintType)),]



#table to show frequency of OPEN complaint types

#what do things that are still open look like?
Open <- subset(Health_ComplaintsRecent,
               Status != "RESOLVED")

FrequencyOpenRecentHealthComplaints <- as.data.frame(table(Open$RCodeDescription))
colnames(FrequencyOpenRecentHealthComplaints)[2] <- "Frequency"
colnames(FrequencyOpenRecentHealthComplaints)[1] <- "ComplaintType"
FrequencyOpenRecentHealthComplaints <- FrequencyOpenRecentHealthComplaints[with(FrequencyOpenRecentHealthComplaints,
                                                                                order(-Frequency,
                                                                                      ComplaintType)),]
FrequencyRecentOpenAndClosedHealthComplaints <- merge(FrequencyOpenRecentHealthComplaints,
                                                      FrequencyRecentHealthComplaints,
                                                      by = "ComplaintType")
colnames(FrequencyRecentOpenAndClosedHealthComplaints)[2] <- "ComplaintsOpen"
colnames(FrequencyRecentOpenAndClosedHealthComplaints)[3] <- "Complaints"


FrequencyRecentOpenAndClosedHealthComplaints$PercentUnresolved <- FrequencyRecentOpenAndClosedHealthComplaints$ComplaintsOpen / FrequencyRecentOpenAndClosedHealthComplaints$Complaints
FrequencyRecentOpenAndClosedHealthComplaints <- FrequencyRecentOpenAndClosedHealthComplaints[with(FrequencyRecentOpenAndClosedHealthComplaints,
                                                                                                  order(-PercentUnresolved,
                                                                                                        ComplaintType)),]
stargazer(subset(FrequencyRecentOpenAndClosedHealthComplaints,
                 ComplaintsOpen >= 25),
          type = "html",
          summary = FALSE,
          out = "FrequencyRecentOpenAndClosedHealthComplaints.html",
          rownames = FALSE)









#make some graphs


                                  
                                                                                 
                                                                                



#pull out the year that the complain was submitted and make it a factor
Open$YearSubmitted <- as.factor(year(Open$RequestDate))

Open$RCodeDescription <- factor(Open$RCodeDescription)

ggplot(subset(Open,
              YearSubmitted == "2011" |
                YearSubmitted == "2012" |
                YearSubmitted == "2013" |
                YearSubmitted == "2014" |
                YearSubmitted == "2015"),
       aes(x = RCodeDescription))+
  geom_bar()+
  coord_flip() +
  theme(axis.title.y = element_text(face="bold",
                                    colour="#990000",
                                    size=9),
        axis.text.y  = element_text(angle=0,
                                    vjust=1,
                                    size=9))+
  theme_solarized()+
  facet_grid(~YearSubmitted)



#############################################################
#Of those that are open, where are they open? ###############
############################################################
OpenFoodServiceEstablishments <- subset(Open,
                                        RCodeDescription == "FOOD MANAGERS")

ggplot(subset(OpenFoodServiceEstablishments,
              YearSubmitted == "2014"),
       aes(x = RCodeDescription))+
  geom_bar()+
  #coord_flip() +
  theme(axis.title.y = element_text(face="bold",
                                    colour="#990000",
                                    size=9),
        axis.text.y  = element_text(angle=0,
                                    vjust=1,
                                    size=9))+
  facet_wrap(~PersonOrPremiseZip)


#### lets make a map
#####################################################
# Import shape file
zip <- readOGR(dsn = "tl_2010_21_zcta510", layer = "tl_2010_21_zcta510")
zip@data$id <- rownames(zip@data)

# Convert polygons in zip to a data frame for plotting
zip.df <- fortify(zip)

# Join columns
zip.df <- join(zip.df, zip@data, by="id")
################################33
################################
##############################
##############################33
# Join columns from df to zip

plotzips <- as.data.frame(table(OpenFoodServiceEstablishments$PersonOrPremiseZip))
colnames(plotzips)[1] <- "ZCTA5CE10"
plotzips$ZCTA5CE10 <- as.factor(plotzips$ZCTA5CE10)

zip.df <- join(zip.df,plotzips , by = "ZCTA5CE10")

# Calculate quartiles for plotting chloropleth against
quantShareA <- quantile(zip.df$Freq, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantShareA

zip.df$quant <- with(zip.df, factor(ifelse(Freq < quantShareA[1], 0, 
                                           ifelse(Freq < quantShareA[2], 1,
                                                  ifelse(Freq < quantShareA[3], 2, 3)))))

# Plot where the share of A's is highest
ggplot(data=subset(zip.df, Freq != 'NA'), aes(x=long, y=lat, group=group)) +
  #geom_polygon(aes(fill=quant)) +
  geom_path(data = subset(zip.df, Freq != 'NA'), color="gray75", size = 0.2) +
  coord_equal()  +
  scale_fill_gradient(aes(fill = quant)) +
  labs(title="Grade-A Share of Establishments\nLouisville Metro") +
  ggtitle(expression(atop(bold("Grade-A Establishments"), atop("Louisville Metro", "")))) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 18),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank()
  )


ggplot(data=subset(zip.df, Freq != 'NA'), aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Freq), colour = alpha("white", 1/2), size =0.2) +
  scale_fill_gradient(low = "white",
                      mid = "grey",
                      high = "blue")


ggplot(data=subset(zip.df, 
                   Freq != 'NA'), 
       aes(x = long,
           y = lat,
           fill = Freq,
           group = group)) +
  geom_polygon(colour = "black") +
  coord_equal() +
  theme_few()




ggsave('Grade-A-Establishments-Share.png', type = 'cairo-png')

