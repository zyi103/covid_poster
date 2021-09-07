library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(lattice)
library(ggmap)
library(maps)
library(mapdata)

ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x)}

FormatSI <- function(x, ...) {
  # Format a vector of numeric values according to the
  # International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Args:
  #   x  : A vector of numeric values
  #   ...: Remaining args passed to format()
  #
  # Returns:
  #   A vector of strings using SI prefix notation
  #
  # Bugs:
  #   Does not (yet) work with small (<1) numbers
  #
  scale.frac <- 1000
  scale.unit <- c("k", "M", "G", "T", "P", "E", "Z", "Y")
  
  # Start with empty prefixes
  p <- rep(" ", length(x))
  
  # Divide by scale.frac and store scale.unit if value is
  # large enough. Repeat for all units.
  for(i in 1:length(scale.unit)) {
    p[x >= scale.frac] <- scale.unit[i]
    x[x >= scale.frac] <- x[x >= scale.frac] / scale.frac
  }
  
  return(paste(format(round(x,1), trim=TRUE, scientific=FALSE, ...), p))
}

# https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset

theme_set(theme_bw())

sum.confirmed.data <- read_csv("time_series_covid_19_confirmed.csv")
sum.confirmed <- sum.confirmed.data[,5:length(sum.confirmed.data)]

sum.confirmed <- sum.confirmed %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum))

sum.confirmed <- melt(sum.confirmed)
sum.confirmed <- sum.confirmed %>% rename(
  date = variable,
  confirmed = value
)
plot(sum.confirmed$confirmed,type="l")
ggplot(sum.confirmed, aes(x=date,y=confirmed,group=1)) + geom_line(color="orange") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Growth on confirmed coronavirus cases worldwide")

#----
sum.deaths.data <- read_csv("time_series_covid_19_deaths.csv")
sum.deaths <- sum.deaths.data[,5:length(sum.deaths.data)]

sum.deaths <- sum.deaths %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum))

sum.deaths <- melt(sum.deaths)
sum.deaths <- sum.deaths %>% rename(
  date = variable,
  deaths = value
)
plot(sum.deaths$deaths,type="l")
ggplot(sum.deaths, aes(x=date,y=deaths,group=1)) + geom_line(color="red") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Growth on deaths coronavirus cases worldwide")

#----
sum.recovered.data <- read_csv("time_series_covid_19_recovered.csv")
sum.recovered <- sum.recovered.data[,5:length(sum.recovered.data)]

sum.recovered <- sum.recovered %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum))

sum.recovered <- melt(sum.recovered)
sum.recovered <- sum.recovered %>% rename(
  date = variable,
  recovered = value
)
plot(sum.recovered$recovered,type="l")
ggplot(sum.recovered, aes(x=date,y=recovered,group=1)) + geom_line(color="green") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Growth on recovered coronavirus cases worldwide")

#---
covid.19 <- inner_join(sum.confirmed,sum.deaths)
covid.19 <- inner_join(covid.19,sum.recovered)

p.world <- ggplot(covid.19,aes(x=date,group=1)) + 
  geom_area(aes(y=confirmed),fill="orange") + 
  geom_area(aes(y=recovered),fill="green") + 
  geom_area(aes(y=deaths),fill="red") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Count on oronavirus cases worldwide") +
  ylab("Count")


#---
data <- read_csv("covid_19_data.csv")
wuhan.covid19.data <- data %>% 
  filter(`Province/State`=="Hubei")
p.wuhan <- ggplot(wuhan.covid19.data,aes(x=ObservationDate,group=1)) + 
  geom_area(aes(y=Confirmed),fill="orange") + 
  geom_area(aes(y=Recovered),fill="green") + 
  geom_area(aes(y=Deaths),fill="red") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Count on oronavirus cases in Wuhan") + 
  ylab("Count") + ylim(0,80000)



grid.arrange(p.world,p.wuhan,nrow=1)


# for loop for the map
count <- 0
for (d in unique(data$ObservationDate)) {
  #---
  # d <- "04/18/2020"
  ww.covid19.data <- data %>% 
    filter(ObservationDate == d)
  # ww.covid19.data <- data
  # not.cn.covid19.data <- ww.covid19.data %>% 
  #   filter(`Country/Region` != "Mainland China")
  # 
  # p.1.23 <- ggplot(ww.covid19.data,aes(x=reorder(`Country/Region`, -Confirmed,FUN=sum),y=Confirmed)) +
  #   geom_bar(stat="identity",fill='steelblue') +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   ylab("Count") + xlab("Counties") + 
  #   ggtitle("List of Countries with Coronavirus cases on 01/23")  
  
  
  #---

  theme_set(
    theme_void()
  )
  
  world_map <- map_data("world")
  # China_map <- map_data("world",region="China")
  
  # country id cleaning
  countris <- recode(ww.covid19.data$`Country/Region`, US="USA", `Mainland China`="China", `Hong Kong`="China", `Macau`="China")
  ww.covid19.data$`Country/Region` <- countris
  
  # countris <- recode(not.cn.covid19.data$`Country/Region`, US="USA", `Hong Kong`="Other", `Macau`="Other")
  # not.cn.covid19.data$`Country/Region` <- countris
  
  country.id.lookup <- world_map %>% count(region)
  country.id <- ww.covid19.data %>% count(`Country/Region`)
  
  country.id$has <- country.id$`Country/Region` %in% country.id.lookup$region
  country.id %>% filter(has == FALSE)
  
  
  library(viridis)
  # gg <- ggplot(ww.covid19.data,aes(fill=Confirmed))
  # gg <- gg + geom_map(dat=world_map, map = world_map, 
  #              aes(map_id=region), fill="white", color="#7f7f7f", size=0.25) 
  # gg <- gg + geom_map(map = world_map, aes(map_id = `Country/Region`), color="#7f7f7f", size=0.25) 
  # gg <- gg + expand_limits(x = world_map$long, y = world_map$lat)
  # gg <- gg + scale_fill_gradient(low="#fff7bc", high="#cc4c02")
  # gg <- gg + ggtitle("Countries with covid19 cases")
  # gg  
  # 
  # grid.arrange(gg,p.1.23,nrow=2)
                                     
  # data$ObservationDate <- parse_datetime(data$ObservationDate, "%m/%d/%Y")
  # 
  # class(data$ObservationDate)
  # library(gganimate)
  www.covid19.data.by.country <- ww.covid19.data %>% group_by(`Country/Region`) %>% summarise(Confirmed = sum(Confirmed)) %>% 
    filter(Confirmed != 0)
  
  gg <- ggplot(www.covid19.data.by.country,aes(fill=Confirmed))
  gg <- gg + geom_map(data=world_map, map = world_map, aes(map_id=region), fill="#cccccc", color="#ffffff", size=0.25) 
  gg <- gg + geom_map(map = world_map, aes(map_id = `Country/Region`), color="#cccccc", size=0.25) 
  gg <- gg + expand_limits(x = world_map$long, y = world_map$lat)
  gg <- gg + scale_fill_gradient(low="#FED79C", high="#CF1112",
                                 name="Count", space = "Lab", trans="log10",
                                 labels= c("1,000,000","100,000", "10,000", "1,000", "100", "10", "1", "0"),
                                 breaks = c(1000000,100000, 10000, 1000, 100, 10, 1 ,0),
                                 limits=c(1,1000000))
  gg <- gg + ggtitle(sprintf("Countries with covid19 cases %s", d))
  gg
  ggsave(sprintf("map-%s.png", gsub('/', '', d)),plot = last_plot(),device='png',path='./temp')
}

#
#
# creating forcus list
major.hit.countries.list <- c("Iran","Mainland China","UK","US","Italy")


#
#
# population graph by countries

pop.data <-data %>% group_by(`Country/Region`, ObservationDate) %>% summarise(Confirmed = sum(Confirmed)) %>% 
  filter(Confirmed != 0) 
# major.hit.countries.list <- pop.data %>% filter(Confirmed > 80000) %>% distinct(`Country/Region`)

major.hit.countries.data <- pop.data %>% filter(`Country/Region` %in% major.hit.countries.list)
major.hit.countries.data$ObservationDate <- as_date(major.hit.countries.data$ObservationDate,format= "%m/%d/%Y")

ggplot(data = major.hit.countries.data, aes(x= ObservationDate, 
                                            y= Confirmed, 
                                            group=`Country/Region`, 
                                            color=`Country/Region`)) + 
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Coronavirus spreads Timeline by Countries") +
  scale_x_date(date_breaks = "2 week", date_minor_breaks = "1 week",
                 date_labels = "%b %d") + 
  scale_y_continuous(labels = ks) +
  labs(x="Date", y="Confirmed Cases") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(),
  )

#
#
# log 
log.data <-data %>% group_by(`Country/Region`, ObservationDate) %>% summarise(Confirmed = sum(Confirmed)) %>% 
  filter(Confirmed != 0) 

major.hit.countries.log.data <- log.data %>% filter(`Country/Region` %in% major.hit.countries.list)
major.hit.countries.log.data$ObservationDate <- as_date(major.hit.countries.data$ObservationDate,format= "%m/%d/%Y")

ggplot(data = major.hit.countries.log.data, aes(x= ObservationDate, 
                                            y= Confirmed, 
                                            group=`Country/Region`, 
                                            color=`Country/Region`)) + 
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Coronavirus spreads Timeline in Logarithmic Scale") +
  scale_x_date(date_breaks = "2 week", date_minor_breaks = "1 week",
               date_labels = "%b %d") + 
  scale_y_continuous(trans = log10_trans(), labels = ks) +
  labs(x="Date", y="Confirmed Cases") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(),
  )

#
#  
# growth rate

growth.rate <- data %>% arrange(desc(ObservationDate), desc(`Country/Region`)) %>% group_by(`Country/Region`, ObservationDate) %>% 
  summarise(SumConfirmed = sum(Confirmed)) %>% filter(SumConfirmed != 0) 


growth.pop <- ungroup(growth.rate) %>% 
  arrange(`Country/Region`) %>%
  group_by(`Country/Region`) %>% 
  mutate(growth = SumConfirmed - lag(SumConfirmed, default = 0))


major.hit.countries.growth.pop <- growth.pop %>% filter(`Country/Region` %in% major.hit.countries.list)
major.hit.countries.growth.pop$ObservationDate <- as_date(major.hit.countries.growth.pop$ObservationDate,format= "%m/%d/%Y")

ggplot(data = major.hit.countries.growth.pop, aes(x= ObservationDate, 
                                            y= growth, 
                                            group=`Country/Region`, 
                                            color=`Country/Region`)) + 
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  ggtitle("Coronavirus Growth Rate Timeline by Countries") +
  scale_x_date(date_breaks = "2 week", date_minor_breaks = "1 week",
               date_labels = "%b %d") + 
  scale_y_continuous(labels = ks) +
  labs(x="Date", y="Newly added Cases") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(),
  )

#
#
# new cases vs total cases

pop.ratio.data <- data %>% arrange(desc(ObservationDate), desc(`Country/Region`)) %>% group_by(`Country/Region`, ObservationDate) %>% 
  summarise(SumConfirmed = sum(Confirmed)) %>% filter(SumConfirmed != 0) 

pop.ratio.data <- ungroup(pop.ratio.data) %>% 
  arrange(`Country/Region`) %>%
  group_by(`Country/Region`) %>% 
  mutate(NewConfirmed = SumConfirmed - lag(SumConfirmed, 7, default = 0)) %>%
  slice(8:n()) %>% filter(SumConfirmed > 100) 

# creating forcus list
major.hit.countries.list.plus <- pop.ratio.data %>% filter(SumConfirmed > 20000) %>% distinct(`Country/Region`)


major.hit.countries.ratio.data <- pop.ratio.data %>% filter(`Country/Region` %in% major.hit.countries.list.plus$`Country/Region`)
major.hit.countries.ratio.data$ObservationDate <- as_date(major.hit.countries.ratio.data$ObservationDate,format= "%m/%d/%Y")

loop.date.arranged <- major.hit.countries.ratio.data %>% group_by(ObservationDate) %>%
  distinct(ObservationDate) %>% arrange(ObservationDate)

for (d in loop.date.arranged$ObservationDate) {
  # d <- major.hit.countries.ratio.data[sample(nrow(major.hit.countries.ratio.data), 1),"ObservationDate"]
  # d <- "2020-04-18"
  # d <- "2020-02-02"
  temp.ratio.data <- major.hit.countries.ratio.data %>% filter(ObservationDate <= d) 
  temp.point.data <- major.hit.countries.ratio.data %>% filter(ObservationDate == d)
  
  ggplot(data = temp.ratio.data, aes(x= SumConfirmed, 
                                              y= NewConfirmed, 
                                              group=`Country/Region`)) + 
    geom_line(alpha=0.4) +
    geom_point(data=temp.point.data, aes(x=SumConfirmed, y=NewConfirmed), size = 2, color= "red") +
    geom_text(data=temp.point.data, aes(label=as.character(`Country/Region`)),vjust=0.5,hjust=0, size=3, nudge_x = 0.05) +
    ggtitle(sprintf("Covid Timeline %s", as_date(as.numeric(d)))) +
    scale_y_log10(limits = c(1e1,1e6), expand = c(0, 0), 
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = FormatSI) +
    scale_x_log10(limits = c(1e1,1e7), expand = c(0, 0), 
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = FormatSI) +
    labs(x="Total Cases", y="New Confirmed Cases (Past Week)") +
    annotation_logticks() +
    theme_linedraw() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(0.4, 0.8, 0.4, 0.4, "cm"),
      plot.background = element_rect(
        fill = "#e5d8d8",
        colour = "black",
        size = 1
      )
    )
  ggsave(sprintf("plot-%s.png", gsub('/', '', d)),plot = last_plot(),device='png',path='~/temp')
}

