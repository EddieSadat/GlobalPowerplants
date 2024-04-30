install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('maps')
install.packages('mapproj')
install.packages('treemap')
install.packages('googleVis')
install.packages('datasets')
install.packages("tidyverse")

library('googleVis')
library('datasets')
library("ggplot2")
library("RColorBrewer")
library('maps')
library('mapproj')
library('treemap')
library('tidyverse')

#Saving dataset into dataframe
mydata <- GlobalPowerPlant

#Initial analysis
dim(mydata) #Dimension of dataframe
head(mydata) #First few rows of dataframe
str(mydata) #Structure of dataframe

#Sum of Missing Values per Column
colSums(is.na(mydata))
colSums(mydata == '')

#=====Country Barplot=========================================================================
  #Counting number of occurences per unique value (Country) - sort descending
countrymax <- mydata %>% count(Country, sort = TRUE)
str(countrymax)
  #Barplot of top 10 countries by number of power plants

ggplot(data = countrymax[0:10,], aes(x = Country, y = n)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), 
        panel.background = element_rect(fill = "white", color = 'black', linetype = 'solid')) + 
  labs(title = "Top 10 Countries with the most Power Plants", y = "Number of Power Plants")


  #Changing country names in dataset to match Maps data
countrydata <- countrymax  

countrydata$Country[countrydata$Country == "United States of America"] <- "USA"
countrydata$Country[countrydata$Country == "United Kingdon"] <- "UK"
countrydata$Country[countrydata$Country == "Syrian Arab Republic"] <- "Syria"
countrydata$Country[countrydata$Country == "Czech Republic"] <- "Czechia"
countrydata$Country[countrydata$Country == "Bosnia and Herzegovina"] <- "Bosnia"
countrydata$Country[countrydata$Country == "Democratic Republic of the Congo"] <- "Congo - Brazzaville"


  #Saving map data
world_map <- map_data("world")

  #Plotting
ggplot(countrydata) +
    #Generate plot with all countries yellow
  geom_map(dat = world_map, map = world_map, aes(map_id = region), fill = "lightyellow", color = "#7f7f7f", size = 0.25) +
    #Fill countried according to number of powerplants
  geom_map(map = world_map, aes(map_id = Country, fill = n), size = 0.25) +
    #Specifying gradient colors for legend
  scale_fill_gradient(low = "#DEEBF7", high = "#084594", name = "Number of Power Plants") +
    #Using longitude and latitude from maps data
  expand_limits(x = world_map$long, y = world_map$lat) +
    #White background, remove x and y labels
  theme(panel.background = element_rect(fill = "white", color = 'black', linetype = 'solid')) + 
  labs(title = "Number of Power Plants per Country", x = "", y = "")


#=====Generating bar for total Energy Capacity per country=================================================================
capacitydata <- aggregate(mydata$Capacity..MW., by=list(mydata$Country), FUN=sum)
capacitydata <- capacitydata[order(-capacitydata$x),]
ggplot(data = capacitydata[0:10,], aes(x = Group.1, y = x)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), 
        panel.background = element_rect(fill = "white", color = 'black', linetype = 'solid')) + 
  labs(title = "Top 10 Countries with the most Energy Capacity", x = "Country",y = "Total Energy Production Capacity (MW)")


#=====Generating Geographic Heatmap based on total energy prod. capacity===================================================
  #Saving table into new variable
capacitydata1 <- capacitydata  

  #Changing country names in dataset to match Maps data
capacitydata1$Group.1[capacitydata1$Group.1 == "United States of America"] <- "USA"
capacitydata1$Group.1[capacitydata1$Group.1 == "United Kingdon"] <- "UK"
capacitydata1$Group.1[capacitydata1$Group.1 == "Syrian Arab Republic"] <- "Syria"
capacitydata1$Group.1[capacitydata1$Group.1 == "Czech Republic"] <- "Czechia"
capacitydata1$Group.1[capacitydata1$Group.1 == "Bosnia and Herzegovina"] <- "Bosnia"
capacitydata1$Group.1[capacitydata1$Group.1 == "Democratic Republic of the Congo"] <- "Congo - Brazzaville"

  #Saving map data
world_map <- map_data("world")

  #Plotting
ggplot(capacitydata1) +
  #Generate plot with all countries yellow
  geom_map(dat = world_map, map = world_map, aes(map_id = region), fill = "lightyellow", color = "#7f7f7f", size = 0.25) +
  #Fill countried according to number of powerplants
  geom_map(map = world_map, aes(map_id = Group.1, fill = x), size = 0.25) +
  #Specifying gradient colors for legend
  scale_fill_gradient(low = "#DEEBF7", high = "#084594", name = "Total Energy Production Capacity") +
  #Using longitude and latitude from maps data
  expand_limits(x = world_map$long, y = world_map$lat) +
  #White background, remove x and y labels
  theme(panel.background = element_rect(fill = "white", color = 'black', linetype = 'solid')) + 
  labs(title = "Total Energy Production Capacity per Country (MW)", x = "", y = "")

#=====Generating Bar graph for Owners=====================================================================================
  #colSums(mydata == '')
mydata1 <- mydata[!mydata$Owner == '',]
colSums(mydata1 == '')

ownermax <- mydata1 %>% count(Owner, sort = TRUE)
str(ownermax)

ggplot(data = ownermax[0:10,], aes(x = Owner, y = n)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1), 
        panel.background = element_rect(fill = "white", color = 'black', linetype = 'solid')) + 
  labs(title = "Top 10 Owners with the most Power Plants", y = "Number of Power Plants")


#=====Generating Bar graph for Fuel sources==============================================================================
fuelmax <- mydata %>% count(Primary.Fuel, sort = TRUE)
str(fuelmax)

ggplot(data = fuelmax, aes(x = Primary.Fuel, y = n)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), 
        panel.background = element_rect(fill = "white", color = 'black', linetype = 'solid')) + 
  labs(title = "Number of Power Plants that utilize each Fuel Source", x = 'Primary Fuel Source',y = "Number of Power Plants")


#=====Generating Treemap=================================================================================================
treemap(mydata, index = c("Country","Primary.Fuel"), vSize = "Capacity..MW.", type = 'index', 
        fontsize.labels = c(15,10), align.labels = list(c("left", "top"), c("left", "bottom")), 
        title = "Fuel Source and Energy Production Capacity by Country")


#=====Interactive Visuals=================================================================================================
  #Renaming columns
fuelmax <- fuelmax %>% rename("Number of Power Plants" = "n")

  #Interactive Bar graph of Fuel sources
interbar <- gvisColumnChart(fuelmax, options=list(legend='none', width=700, height=700,
                                                  title = "Number of Power Plants using each Fuel Source",
                                                  titleTextStyle="{color:'blue',fontName:'Arial',fontSize:20}",
                                                  ))

  #Renaming columns
capacitydata <- capacitydata %>% rename("Country" = "Group.1","Total Energy Production Capacity" = "x")

capacitydata$Country[capacitydata$Country == "United States of America"] <- "US"
capacitydata$Country[capacitydata$Country == "United Kingdon"] <- "UK"
capacitydata$Country[capacitydata$Country == "Syrian Arab Republic"] <- "Syria"
  
  #Interactive Geo Graph of Capacity
intergeo <- gvisGeoChart(capacitydata, "Country","Total Energy Production Capacity",
                         options = list(region = 'world', width=700, height=700,
                                        title = "Total Energy Production Capacity per Country",
                                        titleTextStyle="{color:'blue',fontName:'Arial',fontSize:20}")
                                        )


combined <- gvisMerge(interbar, intergeo, horizontal = TRUE)
plot(combined)

