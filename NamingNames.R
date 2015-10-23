# -------------------------------------------
# Louisville Health Code Violations Study
#
# Date : September 10, 2015
# -------------------------------------------

## Load Packages
library(ggplot2)
library(ggthemes)
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)


## Load data & Clean
scoresDF <- read.table(url('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=FoodServiceData.txt'), 
                       sep = '\t', quote = NULL, comment = '', header = TRUE) # Recorded scores for every restaurant/food processor
inspectionsDF <- read.csv('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Health_Inspections.csv',
                          header = FALSE, stringsAsFactors = FALSE, colClasses = 'character') # Inspection attributes
establishmentsDF <- read.csv('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Health_Establishments.csv',
                             header = TRUE)

inspectionsDF <- inspectionsDF[order(-xtfrm(inspectionsDF[,7])), ] # Re-order from newest to oldest

# Rename columns, because they are (for some reason) not included...thanks a lot
colnames(inspectionsDF) <- c('ID', 
                             'Inspection_ID', 
                             'Establishment_ID', 
                             'Request_ID', 
                             'EHS_Number',
                             'County_ID',
                             'Inspection_Date',
                             'Inspection_Type',
                             'Is_FollowUp',
                             'RF_Insp_ID',
                             'Blank',
                             'Grade',
                             'Score',
                             'Insp_Time_Hours',
                             'Insp_Time_Mins',
                             'Blank',
                             'Blank',
                             'Next_Insp_Date',
                             'Action_Code',
                             'Complaint_Resolved')

# Get rid of blanks
inspectionsDF <- inspectionsDF[, -which(grepl('Blank', names(inspectionsDF)))]

# Merge the two sets together
totes <- merge(scoresDF, inspectionsDF, 
               by.x = 'InspectionID', by.y = 'Inspection_ID')

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}

ggplot(subset(totes, Grade.x != ''), aes(x = Grade.x, y = Score.x)) +
  geom_point(alpha = 0.025) +
  stat_sum_single(median) +
  facet_wrap(~Zip, nrow = 6) +
  theme_fivethirtyeight()

ggplot(subset(totes, Grade.x != ''), aes(x = Grade.x, y = Score.x)) +
  ggtitle('Restaurant Inspection Scores in Louisville, by Zip') +
  geom_point(alpha = 0.025, size = 1.5) +
  stat_summary(fun.y = median, fun.ymin = min, fun.ymax = max,
             colour = "orange", size = 0.5, alpha = 0.75) +
  coord_flip() +
  facet_wrap(~Zip, nrow = 9) +
  theme_fivethirtyeight(base_size = 8) +
  theme(
    plot.background = element_rect(fill = 'white')
  )
# ggsave('Score-by-Zip.png', height = 6, width = 8, units = 'in', type = 'cairo-png')

# Find the names and merge against inspections
complaints <- subset(inspectionsDF, Inspection_Type == 'COMPLAINT')
named <- merge(establishmentsDF, complaints, by.x = 'EstablishmentID', by.y = 'Establishment_ID')

# ---------------- 
# Mapping Zipcodes
# ----------------

## Grab plotting data
zipA <- table(subset(totes, Grade.x == 'A')$Zip)
zipB <- table(subset(totes, Grade.x == 'B')$Zip) 
zipC <- table(subset(totes, Grade.x == 'C')$Zip)

plotZips <- merge(zipA, zipB, by = 'Var1', all.x = TRUE)
plotZips <- merge(plotZips, zipC, by = 'Var1', all.x = TRUE)
plotZips[is.na(plotZips)] <- 0

colnames(plotZips) <- c('ZCTA5CE10', 'CountA', 'CountB', 'CountC')

plotZips$CountA <- as.numeric(plotZips$CountA)
plotZips$CountB <- as.numeric(plotZips$CountB)
plotZips$CountC <- as.numeric(plotZips$CountC)

# plotZips$ShareA <- plotZips$CountA/rowSums(plotZips[,c(2:4)])
# plotZips$ShareB <- plotZips$CountB/rowSums(plotZips[,c(2:4)])
# plotZips$ShareC <- plotZips$CountC/rowSums(plotZips[,c(2:4)])

plotZips$ShareA <- plotZips$CountA/sum(plotZips[,2])
plotZips$ShareB <- plotZips$CountB/sum(plotZips[,3])
plotZips$ShareC <- plotZips$CountC/sum(plotZips[,4])

# Import shape file
zip <- readOGR(dsn = "tl_2010_21_zcta510", layer = "tl_2010_21_zcta510")
zip@data$id <- rownames(zip@data)

# Convert polygons in zip to a data frame for plotting
zip.df <- fortify(zip)

# Join columns
zip.df <- join(zip.df, zip@data, by="id")

# Join columns from df to zip
zip.df <- join(zip.df, plotZips, by = "ZCTA5CE10")

# Calculate quartiles for plotting chloropleth against
quantShareA <- quantile(zip.df$ShareA, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantShareA

quantShareB <- quantile(zip.df$ShareB, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantShareB

quantShareC <- quantile(zip.df$ShareC, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantShareC

zip.df$quantA <- with(zip.df, factor(ifelse(ShareA < quantShareA[1], 0, 
                                           ifelse(ShareA < quantShareA[2], 1,
                                                   ifelse(ShareA < quantShareA[3], 2, 3)))))

zip.df$quantB <- with(zip.df, factor(ifelse(ShareB < quantShareB[1], 0, 
                                            ifelse(ShareB < quantShareB[2], 1,
                                                   ifelse(ShareB < quantShareB[3], 2, 3)))))

zip.df$quantC <- with(zip.df, factor(ifelse(ShareC < quantShareC[1], 0, 
                                            ifelse(ShareC < quantShareC[2], 1,
                                                   ifelse(ShareC < quantShareC[3], 2, 3)))))


# Plot where the share of A's is highest
ggplot(data=subset(zip.df, ShareA != 'NA'), aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=quantA)) +
  geom_path(data = subset(zip.df, ShareA != 'NA'), color="gray75", size = 0.2) +
  coord_equal()  +
  scale_fill_manual("Share of Establishments", values = c("0" = "#fee5d9" , "1" = "#fcae91", "2" = "#fb6a4a", "3" = "#cb181d"),
                               labels = c("0 - 94%", "94% - 96%", "96% - 97%", "> 97%"),
                               na.value = "gray65") +
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

# ggsave('Grade-A-Establishments-Share.png', type = 'cairo-png')

# Plot where the share of B's is highest
ggplot(data=subset(zip.df, ShareB != 'NA'), aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=quantB)) +
  geom_path(data = subset(zip.df, ShareB != 'NA'), color="gray75", size = 0.2) +
  coord_equal()  +
  scale_fill_manual("Share of Establishments", values = c("0" = "#fee5d9" , "1" = "#fcae91", "2" = "#fb6a4a", "3" = "#cb181d"),
                    labels = c("0 - 0.5%", "0.5% - 1%", "> 1%"),
                    na.value = "gray65") +
  labs(title="Grade-B Share of Establishments\nLouisville Metro") +
  ggtitle(expression(atop(bold("Grade-B Establishments"), atop("Louisville Metro", "")))) +
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

# ggsave('Grade-B-Establishments-Share.png', type = 'cairo-png')

# Plot where the share of C's is highest
ggplot(data=subset(zip.df, ShareC != 'NA'), aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=quantC)) +
  geom_path(data = subset(zip.df, ShareC != 'NA'), color="gray75", size = 0.2) +
  coord_equal()  +
  scale_fill_manual("Share of Establishments", values = c("0" = "#fee5d9" , "1" = "#fcae91", "2" = "#fb6a4a", "3" = "#cb181d"),
                    labels = c("0% - 3%", "3% - 3.3%", "3.3% - 5%", "> 5%"),
                    na.value = "gray65") +
  labs(title="Grade-C Share of Establishments\nLouisville Metro") +
  ggtitle(expression(atop(bold("Grade-C Establishments"), atop("Louisville Metro", "")))) +
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

# ggsave('Grade-C-Establishments-Share.png', type = 'cairo-png')
