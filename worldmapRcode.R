######################################################################################################
## how to create a world map in R with survey data, World Bank indicators, ggplot2 and rworldmap  ####
######################################################################################################
# Author: Tim Winke (tim.winke@gmail.com)
# Created: 23/10/2014

#install.packages(c("dplyr", "ggplot2", "RCurl", "rworldmap", "WDI", "mapproj"))
library("dplyr")   # data manipulation
library("ggplot2") # visualsation
library("RCurl") # network client interface
library("rworldmap") # get world map with iso codes
library("WDI") # World Bank indicators
library("mapproj") # Converts latitude/longitude into projected coordinates


#establish https connection to download csv file
URL <- "https://s3-eu-west-1.amazonaws.com/reports.dashboard/public/oak_fb/DaliaResearch_ProjectOak_Dataset.csv"
x <- getURL(URL, ssl.verifypeer=FALSE)
car_raw <- read.csv2(textConnection(x))

# First data manipulations
car <- tbl_df(car_raw) %>% 
  mutate(age = as.numeric(format(Sys.time(), "%Y")) - ybirth, #turn year of birth to age 
         carbrand = as.character(carbrand) #refers to the question: What's your favourite German car brand?
  ) %>%  
  select(iso2c, countryname, device_kind, device_platform, gender, age, household_income, education,carbrand) %>%
  mutate(carbrand = ifelse(is.na(carbrand), "No response", carbrand))


# Country statistics
car_countrystat <- car %>% 
  group_by(iso2c, carbrand) %>% 
  summarise(count = n(),
            countryname = unique(countryname)) %>% 
  arrange(carbrand,desc(count)) 

head(car_countrystat) #AR indicates 



################################
## Barplot - Country groups ####
################################

#country groups
eu <- c('BE','DE','FR','IT','LU','NL','UK','IE','DK','GR','ES','PT','AT','FI','SE','CY','CZ','EE','HU','LT','LV','MT','PL','SI','BG','RO','HR', 'SK') # EU-28 countries

bindcgroups <- function(data) {
  cgroup1 <- data %>% filter(iso2c %in% c('JP')) %>% mutate(cgroup = "Japan") 
  cgroup2 <- data %>% filter(iso2c %in% "CN") %>% mutate(cgroup = "China")
  cgroup3 <- data %>% filter(iso2c %in% "US") %>% mutate(cgroup = "U.S.A.")
  cgroup4 <- data %>% filter(iso2c %in% "IN") %>% mutate(cgroup = "India")
  cgroup5 <- data %>% filter(iso2c %in% eu) %>% mutate(cgroup = "EU-28")
  cgroup6 <- data %>% filter(iso2c %in% "DE") %>% mutate(cgroup = "Germany")
  cgroup7 <- data %>% filter(iso2c %in% "RU") %>% mutate(cgroup = "Russia")
  cgroup8 <- data %>% filter(iso2c %in% "UA") %>% mutate(cgroup = "Ukraine")
  cgroup9 <- data %>% mutate(cgroup = "World")
  cgroup10 <- data %>% filter(iso2c %in% "BR") %>% mutate(cgroup = "Brasil") 
  cgroup11 <- data %>% filter(iso2c %in% "IR") %>% mutate(cgroup = "Iran")
  cgroup12 <- data %>% filter(iso2c %in% "KR") %>% mutate(cgroup = "South Korea") # KR Korea, Rep.
  cgroup13 <- data %>% filter(iso2c %in% "ID") %>% mutate(cgroup = "Indonesia")
  cgroup14 <- data %>% filter(iso2c %in% c("EC", "PE", "VE", "CO")) %>% mutate(cgroup = "Peru-Ecuador-Venezuela-Colombia")
  cgroup15 <- data %>% filter(iso2c %in% c('CL','AR','UY')) %>% mutate(cgroup = "Chile-Argentina-Uruguay") 
  cgroup16 <- data %>% filter(iso2c %in% c('TW')) %>% mutate(cgroup = "Taiwan") 
  
  cgroup_bind <- rbind(cgroup1,cgroup2,cgroup3,cgroup4,cgroup5,cgroup6,cgroup7,cgroup8,cgroup9,
                       cgroup10,cgroup11,cgroup12,cgroup13,cgroup14,cgroup15,cgroup16)
  return(cgroup_bind)
}

car_barplot <- bindcgroups(data = car_countrystat)

# prepare data set for bar plot 
car_barplot <- car_barplot %>%  
  group_by(cgroup, carbrand) %>%
  summarise(countn = sum(count)) %>% #total counts by country groups and carbrand
  group_by(cgroup) %>% 
  mutate(groupsum = sum(countn), #total counts per country group
         percent = countn*100/groupsum, # percentage for each carbrand
         position = cumsum(percent) - 0.5*(percent) #find center of each carbrand part along the bar
  ) 



# create bar plot 
p <- ggplot(data=car_barplot, aes(x=cgroup, y=percent, fill=carbrand)) 
p + geom_bar(stat="identity") + 
  xlab("") +  ylab("Share of responses") + theme_bw() + coord_flip() +
  guides(fill = guide_legend(
    title="What's your favourite German car brand?",
    keywidth = 0.7, keyheight = 0.7, 
    reverse=F, title.position="top", 
    nrow = 3, byrow=T)) +
  theme(legend.position="bottom") +
  geom_text(aes(x=cgroup, y=position, label=sprintf("%1.0f%%", percent)),size=3) 



#######################################
## Join with World Bank indicators ####
########################################

library("WDI")     # package to retrieve World Bank data
#WDIsearch() #To have a look at all indicators

wdi_gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD",start = 2013, end = 2013, extra = T, cache = NULL) 
# NY.GDP.PCAP.CD refers to "GDP per capita (current US$)" 

wdi_gdp <- wdi_gdp %>%
  mutate(gdp2013 = NY.GDP.PCAP.CD,
         capital_lon = as.numeric(paste(longitude)), #factor to numeric
         capital_lat = as.numeric(paste(latitude))) %>%
  select(iso2c, country, gdp2013, capital, capital_lon, capital_lat)

wdi_gdp$capital <- as.character(wdi_gdp$capital)
wdi_gdp$capital[wdi_gdp$country %in% "Israel"] <- "Jerusalem" #No capital for Israel provided

taiwan <- data.frame(iso2c=as.factor("TW"), 
                     country=as.factor("Taiwan"), 
                     gdp2013=475.33, # http://www.tradingeconomics.com/taiwan/gdp
                     capital=as.factor("Taiwan"), 
                     capital_lon=120.7120023, 
                     capital_lat=22.6158015)
wdi_gdp <- rbind(wdi_gdp, taiwan)

# Now lets merge the previous survey data with the World Bank indicators
car <- car %>% merge(wdi_gdp, by="iso2c", all.x=T)
car_countrystat <- car_countrystat %>%  merge(wdi_gdp, by="iso2c", all.x=T)

head(car_countrystat)


#################
## World map ####
#################

#only take the top 1 car brand by country
car_topbrand <- car_countrystat %>% group_by(iso2c) %>%  filter(min_rank(desc(count)) <= 1) 

car_topbrand$carbrand <- as.factor(car_topbrand$carbrand)
levels(car_topbrand$carbrand) <- c(levels(car_topbrand$carbrand), "Volkswagen")
# Volkswagen never was number one but should be in the map legend 

# join function from rworldmap
car_map <- joinCountryData2Map(car_topbrand, joinCode = "ISO2", nameJoinColumn = "iso2c")


car_map_poly <- fortify(car_map) #extract polygons 
car_map_poly <- merge(car_map_poly, car_map@data, by.x="id", by.y="ADMIN", all.x=T)
car_map_poly <- car_map_poly %>% arrange(id, order)



# Plot world map
ggplot() + 
  coord_map(xlim = c(-180, 180), ylim = c(-60, 75))  +
  geom_polygon(data = car_map_poly, aes(long, lat, group = group, 
                                        fill=gdp2013),size = 0.3) + 
  scale_fill_gradient(trans = "log",breaks=c(0,1000, 2000,5000,10000,25000,50000,100000), low="#f7fcb9", high="#2c7fb8") + 
  theme_bw() + xlab(NULL) + ylab(NULL) +
  guides(fill = guide_legend(
    title='Text: "What is your favourite German car brand?" \nSmartphone and tablet advert survey \n(August 4-8th 2014, 500 users per country, Dalia Research, open data) \n\nColor: GDP per capita 2013 (current US$) - World Development Indicators (World Bank)',
    keywidth = 0.7, keyheight = 0.7, 
    reverse=F, title.position="top")) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.ticks = element_blank()
    ,axis.text.x = element_blank()
    ,axis.text.y = element_blank()
    ,legend.position = "bottom"
    ,legend.direction = "horizontal"
  ) +
  geom_text(data=subset(car_map@data, !is.na(country)), aes(x=LON, y=LAT,label=carbrand), size=1.5)




