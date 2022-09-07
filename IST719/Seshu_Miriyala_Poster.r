################################################################################
#
# Author: Seshu Miriyala
#
# Purpose: US Housing Market Analysis
#
################################################################################

raw_state_df <- read.csv(file="state_market_tracker.tsv000"
                         , sep = "\t", quote = "\"", header = T, stringsAsFactors = F)

raw_county_df <- read.csv(file="county_market_tracker.tsv000"
                         , sep = "\t", quote = "\"", header = T, stringsAsFactors = F)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(usmap)
library(lubridate)
library(scales)
library(RColorBrewer)
library(extrafont)
library(datasets)
library(ggthemes)
library(ggrepel)
library(plotrix)
library(maps)
library(mapproj)
library(plyr)


# state df
state_df_cols <- colnames(raw_state_df)

summary(cleaned_state_df)

state_df_col_stats <- data.frame(col = c(""), Nulls = c(0), Total = c(0))

for (col in state_df_cols) {
  state_df_col_stats[nrow(state_df_col_stats) + 1,] = c(col, sum(is.na(raw_state_df[col]))
                                                        ,length(rownames(raw_state_df[col])))
}
state_df_col_stats <- state_df_col_stats[-1,]
state_df_col_stats$Nulls <- as.numeric(state_df_col_stats$Nulls)
state_df_col_stats$Total <- as.numeric(state_df_col_stats$Total)
state_df_col_stats["percent"] <- (state_df_col_stats$Nulls/state_df_col_stats$Total)*100

state_df_cols_to_remove <- state_df_col_stats[which(state_df_col_stats$percent > 25),1]

# Removing Rows with unique values and unnecessary values
state_df_cols_to_remove <- append(state_df_cols_to_remove, 
                                  c("region_type_id", "table_id"
                                    , "is_seasonally_adjusted"
                                    , "property_type_id"
                                    , "last_updated"
                                    , "region_type"))

cleaned_state_df <- raw_state_df[,!(colnames(raw_state_df) %in% state_df_cols_to_remove)]

#percentage increase in median sale price from 2018
median_sale_df <- cleaned_state_df[,c("period_begin", "median_sale_price")]

median_sale_agg <- aggregate(median_sale_df$median_sale_price, list(median_sale_df$period_begin)
                             , mean, na.rm=T)

colnames(median_sale_agg) <- c("Period", "MedianSalePrice")

median_sale_agg$Period <- as.POSIXct(median_sale_agg$Period)

median_sale_agg %>% ggplot(aes(x = Period, y = MedianSalePrice, group = 1)) +
  geom_line(stat = "identity")


(max(median_sale_agg$MedianSalePrice) - min(median_sale_agg$MedianSalePrice))/min(median_sale_agg$MedianSalePrice)

# Inventory and Home prices over period
inv_homePrices_df <- cleaned_state_df[,c("period_begin", "median_sale_price_yoy", "inventory_yoy")]
inv_homePrices_df$period_begin <- as.POSIXct(inv_homePrices_df$period_begin)
inv_homePrices_df$Year <- year(inv_homePrices_df$period_begin)

inv_homePrices_agg2 <- inv_homePrices_df %>% group_by(Year) %>% 
  mutate(InventoryYoy = mean(inventory_yoy, na.rm=T)
         ,MedianSalePriceYoy = mean(median_sale_price_yoy, na.rm=T))

my.cols <- sample(x = "#000000", size = nrow(inv_homePrices_agg2), replace = T)

my.cols[which(inv_homePrices_agg2$MedianSalePriceYoy <= 0)] <- "#850D09"
my.cols[which(inv_homePrices_agg2$MedianSalePriceYoy > 0)] <- "#17853D"

inv_homePrices_agg2[inv_homePrices_agg2$Year %in% c(2015:2022),] %>% 
  ggplot(aes(x = InventoryYoy, y = MedianSalePriceYoy)) +
  geom_line(aes(col="#850D09"), lwd=2) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent, limits = c(-0.5,2.5), breaks = c(-0.5,0,0.5,1,1.5,2,2.5)) +
  labs(title = "Inventory vs Homes Prices"
       , y = expression(bold("Median Home Prices YoY"))
       , x = expression(bold("Inventory YoY"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 18, colour = "#FFFFFF")
        , axis.text.x = element_text(size = 18, vjust = 0.5, hjust = 1, colour = "#FFFFFF")
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1, colour = "#FFFFFF")
        , legend.position = "None")+
  geom_smooth(method=lm, lwd=2)

# Inventory and Home prices over period - 2
inv_homePrices_df <- cleaned_state_df[,c("period_begin","parent_metro_region", "state", "median_sale_price_yoy", "inventory_yoy")]
inv_homePrices_df$period_begin <- as.POSIXct(inv_homePrices_df$period_begin)
inv_homePrices_df$Year <- year(inv_homePrices_df$period_begin)

inv_homePrices_agg3 <- inv_homePrices_df %>% 
  select(parent_metro_region, state, inventory_yoy, median_sale_price_yoy) %>% 
  group_by(parent_metro_region, state) %>% 
  summarise(InventoryYoy = median(inventory_yoy, na.rm=T)
         ,MedianSalePriceYoy = median(median_sale_price_yoy, na.rm=T))

inv_homePrices_agg3["StateAbb"] <- state.abb[match(inv_homePrices_agg3$state, state.name)]
inv_homePrices_agg3[match("Columbia", inv_homePrices_agg3$state),]$StateAbb <- "DC"

inv_homePrices_agg3[inv_homePrices_agg3$StateAbb != "ME",] %>% 
  ggplot(aes(x = InventoryYoy, y = MedianSalePriceYoy, label = StateAbb)) +
  geom_point(aes(col=parent_metro_region), size = 3) +
  geom_text_repel(colour="#FFFFFF") + 
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = "Inventory vs Median sale price YoY"
       , y = expression(bold("Median Home Prices YoY"))
       , x = expression(bold("Inventory YoY"))) +
  scale_color_discrete(name = "Region") +
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 18, colour = "#FFFFFF")
        , axis.text.x = element_text(size = 18, vjust = 0.5, hjust = 1, colour = "#FFFFFF")
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1, colour = "#FFFFFF")
        , axis.line.x = element_line(color="#FFFFFF")
        , axis.ticks = element_line(color = "#FFFFFF")
        , legend.text = element_text(colour = "#FFFFFF")
        , legend.title = element_text(colour = "#FFFFFF")) +
  geom_smooth(method=loess,se=T,fullrange=TRUE, formula = "y ~ x")
  


  #gather(Homes, Value, -c(period_begin))

inv_homePrices_df <- cleaned_state_df[,c("period_begin", "homes_sold_yoy", "inventory_yoy")] %>% 
  gather(Group, Value, -c(period_begin))
inv_homePrices_df$period_begin <- as.POSIXct(inv_homePrices_df$period_begin)

inv_homePrices_agg <- inv_homePrices_df %>% 
  select(period_begin, Group, Value) %>% 
  group_by(period_begin, Group) %>% 
  summarise(ValueYoY = median(Value, na.rm=T))

#removing underscore from homes_sold
inv_homePrices_agg$Group <- gsub("homes_sold_yoy", "HomesSold", inv_homePrices_agg$Group)
inv_homePrices_agg$Group <- gsub("inventory_yoy", "Inventory", inv_homePrices_agg$Group)

inv_homePrices_agg[year(inv_homePrices_agg$period_begin) %in% c(2018:2022),] %>% 
  ggplot(aes(x = period_begin, y = ValueYoY, col = Group)) +
  geom_line() + 
  scale_y_continuous(labels = percent, limits = c(-0.4,0.3), breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3)) +
  scale_color_manual(values=c("#850D09", "#17853D")) +
  labs(title = "Inventory-Homes Sales"
       , y = expression(atop(bold("Houses count"),atop("in millions")))) +
  xlab("Year") +# ylab("Homes Count")+
  #scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, vjust = 0.5)
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1))



# Inventory and homes sold over period

inv_homesSold_df <- cleaned_state_df[,c("period_begin", "state", "homes_sold", "inventory")] %>% 
  gather(Homes, Value, -c(period_begin, state))

inv_homesSold_agg <- aggregate(inv_homesSold_df$Value
                               , list(inv_homesSold_df$period_begin, inv_homesSold_df$Homes)
                               , sum, na.rm=TRUE, na.action=NULL)

colnames(inv_homesSold_agg) <- c("Period", "Group", "Value")

#removing underscore from homes_sold
inv_homesSold_agg$Group <- gsub("homes_sold", "HomesSold", inv_homesSold_agg$Group)
inv_homesSold_agg$Group <- gsub("inventory", "Inventory", inv_homesSold_agg$Group)

inv_homesSold_agg$Period <- as.POSIXct(inv_homesSold_agg$Period)
inv_homesSold_agg %>% ggplot(aes(x = Period, y = Value, col = Group)) +
  geom_line() + scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_fill_brewer(palette="Set3",name = "", labels=c("Homes Sold","New Inventory")) +
  labs(title = "Inventory-Homes Sales"
       , y = expression(atop(bold("Houses count"),atop("in millions")))) +
  xlab("Year") +# ylab("Homes Count")+
  #scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, vjust = 0.5)
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1)) +
  geom_smooth(method=loess,se=FALSE,fullrange=TRUE,aes(color=Group))

############

price_drop_off_market_df <- cleaned_state_df[,c("period_begin", "price_drops", "off_market_in_two_weeks")]

price_drop_off_market_df$period_begin <- as.POSIXct(price_drop_off_market_df$period_begin)

price_drop_off_market_df["Year"] <- year(price_drop_off_market_df$period_begin)

price_drop_off_market_df$Year <- as.factor(price_drop_off_market_df$Year)

mu <- price_drop_off_market_df[price_drop_off_market_df$Year %in% c(2018:2022),] %>% 
  ddply("Year", summarise
            , grp.mean=mean(off_market_in_two_weeks, na.rm=T))

price_drop_off_market_df[price_drop_off_market_df$Year %in% c(2018:2022),] %>% 
  ggplot(aes(x=off_market_in_two_weeks, fill=Year, col=Year)) +
  geom_density(alpha=0.1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Year),
             linetype="dashed") +
  scale_color_brewer(palette="Set3") + 
  labs(title = "Shift in Sales"
       , y = expression(atop(bold("Density")))
       , x = expression(atop(bold("# Houses Off market in two weeks")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24, colour = "#FFFFFF")
        , axis.text.x = element_text(size = 18, vjust = 0.5, hjust =1,colour = "#FFFFFF")
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1, colour = "#FFFFFF")
        , axis.ticks = element_line(colour = "#FFFFFF")
        , legend.text = element_text(colour = "#FFFFFF")
        , legend.title = element_text(colour = "#FFFFFF"))


###############

inv_homesSold_df <- cleaned_state_df[,c("period_begin", "state", "homes_sold_yoy", "inventory_yoy")] %>% 
  gather(Homes, Value, -c(period_begin, state))

inv_homesSold_agg <- aggregate(inv_homesSold_df$Value
                               , list(inv_homesSold_df$period_begin, inv_homesSold_df$Homes)
                               , median, na.rm=TRUE, na.action=NULL)

colnames(inv_homesSold_agg) <- c("Period", "Group", "Value")

#removing underscore from homes_sold
inv_homesSold_agg$Group <- gsub("homes_sold_yoy", "HomesSold", inv_homesSold_agg$Group)
inv_homesSold_agg$Group <- gsub("inventory_yoy", "Inventory", inv_homesSold_agg$Group)

inv_homesSold_agg$Period <- as.POSIXct(inv_homesSold_agg$Period)
inv_homesSold_agg %>% ggplot(aes(x = Period, y = Value, col = Group)) +
  geom_line() +# scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_color_manual(values=c("#850D09", "#17853D")) +
  labs(title = "Inventory-Homes Sales"
       , y = expression(atop(bold("Houses count"),atop("in millions")))) +
  xlab("Year") +# ylab("Homes Count")+
  #scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, vjust = 0.5)
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1)) +
  geom_smooth(method=loess,se=FALSE,fullrange=TRUE,aes(color=Group))


# Inventory by state
inv_by_state_agg <- aggregate(inv_homesSold_df[inv_homesSold_df$Homes == "inventory",]$Value
                              , list(inv_homesSold_df[inv_homesSold_df$Homes == "inventory",]$period_begin
                                     ,inv_homesSold_df[inv_homesSold_df$Homes == "inventory",]$state)
                              , sum, na.rm = TRUE, na.action=NA)

colnames(inv_by_state_agg) <- c("Period", "State", "Value")

inv_by_state_agg$Period <- as.POSIXct(inv_by_state_agg$Period)

#inv_by_state_agg[inv_by_state_agg$State == "Kansas",] %>% ggplot(aes(x = Period, y = Value)) +
#  geom_line() + scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
#  labs(title = "Home Inventory in Florida") +
#  xlab("Year") + ylab("Homes Count")+
#  theme(plot.title = element_text(hjust = 0.5, size = 36)
#        , panel.background = element_rect(fill = "transparent")
#        , plot.background = element_rect(fill = "transparent")
#        , legend.background = element_rect(fill = "transparent")
#        , axis.title = element_text(size = 24)
#        , axis.text.x = element_text(size = 18, vjust = 0.5)
#        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1))
  
inv_max_by_state <- aggregate(inv_by_state_agg$Value, list(inv_by_state_agg$State), max
                              , na.rm=T, na.action = NA)
inv_min_by_state <- inv_by_state_agg[inv_by_state_agg$Period == "2021-12-01",c("State", "Value")]
inv_drop_percent_by_state <- data.frame(State = inv_max_by_state$Group.1, Max = inv_max_by_state$x)

inv_drop_percent_by_state["Min"] <- inv_min_by_state[inv_min_by_state$State == inv_drop_percent_by_state$State,]$Value

inv_drop_percent_by_state["Percent"] <- ((inv_drop_percent_by_state$Max - inv_drop_percent_by_state$Min)/inv_drop_percent_by_state$Max)

inv_drop_percent_by_state <- arrange(inv_drop_percent_by_state, Percent)
inv_drop_percent_by_state$State <- factor(inv_drop_percent_by_state$State, levels = inv_drop_percent_by_state$State)
ggplot(inv_drop_percent_by_state, aes(State, Percent)) + geom_col(fill = "red") + coord_flip()

inv_drop_percent_by_state %>% ggplot(aes(x = State, y = Percent)) +
  geom_col(fill = "#86D1A0")  + coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Drop in inventory by state") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, vjust = 0.5)
        , axis.text.y = element_text(size = 14, vjust = 0.5, hjust=1))


# Increase in sales per state
sales_by_state_agg <- aggregate(inv_homesSold_df[inv_homesSold_df$Homes == "homes_sold",]$Value
                              , list(inv_homesSold_df[inv_homesSold_df$Homes == "homes_sold",]$period_begin
                                     ,inv_homesSold_df[inv_homesSold_df$Homes == "homes_sold",]$state)
                              , sum, na.rm = TRUE, na.action=NA)

colnames(sales_by_state_agg) <- c("Period", "State", "Value")

sales_by_state_agg$Period <- as.POSIXct(sales_by_state_agg$Period)

sales_max_by_state <- sales_by_state_agg[sales_by_state_agg$Period == "2021-12-01",c("State", "Value")]
sales_min_by_state <- sales_by_state_agg %>% group_by(State) %>% mutate(Min = min(Value, na.rm=T)) %>% select(Min) %>% distinct(State, Min)
sales_raise_percent_by_state <- data.frame(State = sales_max_by_state$State, Max = sales_max_by_state$Value)

sales_raise_percent_by_state["Min"] <- sales_min_by_state[sales_min_by_state$State == sales_raise_percent_by_state$State,]$Min

sales_raise_percent_by_state["Percent"] <- ((sales_raise_percent_by_state$Max - sales_raise_percent_by_state$Min)/sales_raise_percent_by_state$Min)

sales_raise_percent_by_state <- arrange(sales_raise_percent_by_state, Percent)
sales_raise_percent_by_state$State <- factor(sales_raise_percent_by_state$State, levels = sales_raise_percent_by_state$State)
ggplot(sales_raise_percent_by_state, aes(State, Percent)) + geom_col(fill = "red") + coord_flip()

sales_raise_percent_by_state %>% filter(State != "Alabama" & State != "Maine" & State != "Arkansas") %>%
  ggplot(aes(x = State, y = Percent)) +
  geom_col(fill = "#86D1A0")  + coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Drop in inventory by state") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, vjust = 0.5)
        , axis.text.y = element_text(size = 14, vjust = 0.5, hjust=1))


# Sales by Property Type

sales_by_property_type_df <- cleaned_state_df[,c("period_begin", "property_type", "homes_sold", "inventory")] %>% 
  gather(Homes, Value, -c(period_begin, property_type))

homesSold_by_property_type_agg <- aggregate(sales_by_property_type_df$Value
                               , list(sales_by_property_type_df$period_begin, sales_by_property_type_df$property_type)
                               , sum, na.rm=TRUE, na.action=NULL)

colnames(homesSold_by_property_type_agg) <- c("Period", "PropertyType", "Value")

homesSold_by_property_type_agg$PropertyType <- gsub("Single Family Residential", "Single Family", homesSold_by_property_type_agg$PropertyType)
homesSold_by_property_type_agg$PropertyType <- gsub("Multi-Family (2-4 Unit)", "Multi-Family", homesSold_by_property_type_agg$PropertyType, fixed = T)

homesSold_by_property_type_agg$Period <- as.POSIXct(homesSold_by_property_type_agg$Period)

homesSold_by_property_type_agg[homesSold_by_property_type_agg$PropertyType != "All Residential",] %>% 
  ggplot(aes(x=PropertyType,y = Value, fill=PropertyType)) +
  geom_violin(linetype = "dashed", outlier.shape = 1, color = "#FFFFFF") +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1, color="#FFFFFF") +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), color = "#FFFFFF") +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), color="#FFFFFF") +
  scale_fill_manual(values = c("#A0FE00", "#01FA71","#E6CF02","#FAAA00")) +
#scale_color_manual(values=c("#851D1C", "#3A8552"))
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  labs(title = "Home Sales by Property Type") +
  ylab("Homes Count")+
  scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24, colour = "#FFFFFF")
        , axis.text.x = element_text(size = 18, vjust = 0.5, colour = "#FFFFFF")
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1, colour = "#FFFFFF")
        , axis.ticks = element_line(colour = "#FFFFFF")
        , legend.position = "None")

inv_by_property_type_agg <- aggregate(sales_by_property_type_df$Value
                                            , list(sales_by_property_type_df$period_begin, sales_by_property_type_df$Homes)
                                            , sum, na.rm=TRUE, na.action=NULL)

colnames(homesSold_by_property_type_agg) <- c("Period", "Group", "Value")

#removing underscore from homes_sold
homesSold_by_property_type_agg$Group <- gsub("homes_sold", "HomesSold", homesSold_by_property_type_agg$Group)
homesSold_by_property_type_agg$Group <- gsub("inventory", "Inventory", homesSold_by_property_type_agg$Group)

homesSold_by_property_type_agg$Period <- as.POSIXct(homesSold_by_property_type_agg$Period)
homesSold_by_property_type_agg %>% ggplot(aes(x = Period, y = Value, col = Group)) +
  geom_line() + scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  labs(title = "Home Sales by Property Type") +
  xlab("Year") + ylab("Homes Count")+
  scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, vjust = 0.5)
        , axis.text.y = element_text(size = 18, vjust = 0.5, hjust=1))



# Bar chart by property type for 2018,2019,2020, 2021

grouped_bar_chart <- cleaned_state_df[,c("period_begin", "homes_sold", "new_listings")]
grouped_bar_chart$period_begin <- as.POSIXct(grouped_bar_chart$period_begin)
grouped_bar_chart["Year"] <- year(grouped_bar_chart$period_begin)

grouped_bar_chart_agg <- grouped_bar_chart %>% group_by(Year) %>% summarise(medianHomesSold = sum(homes_sold, na.rm = T)
                                                                            , medianNewListings = sum(new_listings, na.rm = T))

grouped_bar_chart_agg$Year <- as.factor(grouped_bar_chart_agg$Year)
grouped_bar_chart_agg <- grouped_bar_chart_agg %>% gather(Group, Value, -c(Year))

grouped_bar_chart_agg[grouped_bar_chart_agg$Year %in% c(2018:2022),] %>% 
  ggplot(aes(fill = Group, x = Year, y = Value))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette="Set3",name = "", labels=c("Homes Sold","New Listings")) +
  #scale_fill_manual(values=c("#A0FE00", "#FAAA00"))+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  labs(title = "Home Sales by Type"
       , y = expression(atop(bold("Houses sold")))
       , y = expression(atop(bold("Year")))) +
  #scale_fill_manual(name = "", labels=c("Houses","New Listings")) +
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24, colour = "#FFFFFF")
        , axis.text.x = element_text(size = 10, vjust = 0.5, colour = "#FFFFFF")
        , axis.text.y = element_text(size = 10, vjust = 0.5, hjust=1, colour = "#FFFFFF")
        , legend.text = element_text(colour = "#FFFFFF")
        , axis.ticks = element_line(colour = "#FFFFFF"))


# county df for New York
county_df_cols <- colnames(raw_county_df)

county_df_col_stats <- data.frame(col = c(""), Nulls = c(0), Total = c(0))

raw_ny_df <- raw_county_df[raw_county_df$state == "New York",]

ny_rows <- length(rownames(raw_ny_df))

for (col in county_df_cols) {
  county_df_col_stats[nrow(county_df_col_stats) + 1,] = c(col, sum(is.na(raw_ny_df[col]))
                                                          ,ny_rows)
}
county_df_col_stats <- county_df_col_stats[-1,]
county_df_col_stats$Nulls <- as.numeric(county_df_col_stats$Nulls)
county_df_col_stats$Total <- as.numeric(county_df_col_stats$Total)
county_df_col_stats["percent"] <- (county_df_col_stats$Nulls/county_df_col_stats$Total)*100

county_df_cols_to_remove <- county_df_col_stats[which(county_df_col_stats$percent > 75),1]

county_df_cols_to_remove <- append(county_df_cols_to_remove, c("region_type_id", "table_id", "is_seasonally_adjusted", "property_type_id", "last_updated"))

cleaned_ny_county_df <- raw_ny_df[,!(colnames(raw_ny_df) %in% county_df_cols_to_remove)]

# DataTypes
#state
copy_df <- cleaned_state_df

copy_df$period_begin <- as.POSIXct(copy_df$period_begin, format = "%Y-%m-%d")
copy_df$period_end <- as.POSIXct(copy_df$period_end, format="%Y-%m-%d")

copy_df$region <- as.factor(copy_df$region)
copy_df$region_type <- as.factor(copy_df$region_type)
copy_df$state <- as.factor(copy_df$state)
copy_df$state_code <- as.factor(copy_df$state_code)
copy_df$property_type <- as.factor(copy_df$property_type)

cleaned_state_df <- copy_df

#county
copy_df <- cleaned_ny_county_df

copy_df$period_begin <- as.POSIXct(copy_df$period_begin, format = "%Y-%m-%d")
copy_df$period_end <- as.POSIXct(copy_df$period_end, format="%Y-%m-%d")

copy_df$region <- as.factor(copy_df$region)
copy_df$region_type <- as.factor(copy_df$region_type)
copy_df$state <- as.factor(copy_df$state)
copy_df$state_code <- as.factor(copy_df$state_code)
copy_df$property_type <- as.factor(copy_df$property_type)

copy_df["year"] <- format(copy_df$period_begin, format = "%Y")

cleaned_ny_county_df <- copy_df

copy_df <- cleaned_state_df

copy_df["year"] <- format(copy_df$period_begin, format = "%Y")

cleaned_state_df <- copy_df

gc()
agg.1 <- aggregate(cleaned_state_df$median_ppsf
                   , list(cleaned_state_df$year, cleaned_state_df$parent_metro_region)
                   , mean, na.rm=T)
colnames(agg.1) <- c("year", "parent_metro_region", "median_ppsf")

agg.1 %>% ggplot() + 
  aes(x = year,y = median_ppsf, group=parent_metro_region, col=parent_metro_region) + 
  geom_line(size=2) + ylim(0, 300) + labs(title = "Median Price Per Sqft"
                                    , subtitle = "Median Price Per Sqft per region") +
  xlab("Year") + ylab("Median Price Per Sqft")+
  scale_color_discrete(name="Regions", breaks=c("Midwest Region", "Northeast Region"
                                                , "South Region", "West Region"),
                       labels=c("Midwest", "Northeast", "South", "West")
                       , guide = guide_legend(reverse=TRUE)) +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , plot.subtitle = element_text(hjust = 0.5, size = 24)
        #, panel.background = element_rect(fill = "transparent")
        #, plot.background = element_rect(fill = "transparent")
        #, legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, angle = 90, vjust = 0.5, hjust=1)
        , axis.text.y = element_text(size = 18)
        )

# PLOT 2
df_west_region <- cleaned_state_df[cleaned_state_df$parent_metro_region == "West Region",]

agg.2 <- aggregate(df_west_region$median_ppsf, list(df_west_region$year, df_west_region$state), mean, na.rm=T)

colnames(agg.2) <- c("year", "state", "median_ppsf")

agg.2 %>% ggplot() + aes(x = year,y = median_ppsf, group=state, col=state) + 
  geom_line(aes(linetype=state), size = 2) +
  ylim(0, 500) + labs(title = "Median Price Per Sqft"
                        , subtitle = "Median Price Per Sqft for Northeast states") +
  xlab("Year") + ylab("Median Price Per Sqft")+
  scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , plot.subtitle = element_text(hjust = 0.5, size = 24)
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent")
        , legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(size = 18, angle = 90, vjust = 0.5, hjust=1))

# PLOT 3
df_ne_region <- cleaned_state_df[cleaned_state_df$parent_metro_region == "Northeast Region",]

agg.3 <- aggregate(df_ne_region$median_ppsf, list(df_ne_region$year, df_ne_region$state), mean, na.rm=T)

agg.4 <- aggregate(df_ne_region$median_ppsf, list(df_ne_region$state), mean, na.rm=T)

colnames(agg.3) <- c("year", "state", "median_ppsf")
colnames(agg.4) <- c("state", "median_ppsf")

agg.3 %>% ggplot() + 
  aes(x = year,y = median_ppsf, group=state, col=state) + 
  geom_line(aes(linetype=state), size = 2) + ylim(0,450) +
  labs(title = "Median Price Per Sqft"
       , subtitle = "Median Price Per Sqft for Northeast states") +
  xlab("Year") + ylab("Median Price Per Sqft")+
  scale_color_discrete(name="States") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , plot.subtitle = element_text(hjust = 0.5, size = 24)
        #, panel.background = element_rect(fill = "transparent")
        #, plot.background = element_rect(fill = "transparent")
        #, legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        , axis.text = element_text(size=18))

plot_usmap(data=agg.4, include = agg.4$state, values = "median_ppsf", color = "tan", labels = T
           , label_color="white") +
  scale_fill_continuous(name = "Median Price per Sqft") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , plot.subtitle = element_text(hjust = 0.5, size = 18)
        #, panel.background = element_rect(fill = "transparent")
        #, plot.background = element_rect(fill = "transparent")
        #, legend.background = element_rect(fill = "transparent")
        , legend.position = "right"
        , plot.caption = element_text(hjust = 0.5, size = 18))+
  labs(title = "US states",
       subtitle = "Northeast states and their median price per Sqft"
       , caption = "Map 2")

# PLOT 4
agg.7 <- cleaned_state_df[,c("state", "median_sale_price_yoy")]

agg.7 <- agg.7 %>% 
  select(state, median_sale_price_yoy) %>% 
  group_by(state) %>% 
  summarise(MedianSalePriceYoy = median(median_sale_price_yoy, na.rm=T))
agg.7$region <- tolower(agg.7$state)

states <- map_data("state")
map.df <- merge(states,agg.7, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]

num.cols <- 20
my.color.vec <- rev(heat.colors(num.cols))


agg.7$index <- round(rescale(x = agg.7$MedianSalePriceYoy, c(1, num.cols)),0)
agg.7$color <- my.color.vec[agg.7$index]

state.order <- match.map(database = "state", regions = agg.7$state
                         , exact = F, warn = T)
m <- map(database = "state")

cbind(m$names, agg.7$state[state.order])

map("state", col=agg.7$color[state.order], fill = T
    , resolution = 0, lty=1, projection = "polyconic"
    , border="tan")

plot_usmap(data = agg.7, values = "MedianSalePriceYoy", color = "tan", labels = T, label_color="#FFFFFF") + 
  scale_fill_continuous(name = "", label = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent", colour = "transparent")
        , plot.background = element_rect(fill = "transparent", colour = "transparent")
        , legend.background = element_rect(fill = "transparent", colour = "transparent")
        , legend.position = "right"
        , legend.text = element_text(colour = "#FFFFFF")
        , plot.caption = element_text(hjust = 0.5, size = 18))+
  labs(title = "Median price YoY.")

#PLOT 5
#region map

copy.1 <- cleaned_state_df

copy.1$state <- tolower(copy.1$state)

region_avg <- copy.1 %>% group_by(parent_metro_region) %>% summarise(med_ppsf = mean(median_ppsf, na.rm=T))

copy.1[which(copy.1$parent_metro_region == "Midwest Region"),"region_mean"] <- region_avg[which(region_avg$parent_metro_region == "Midwest Region"),"med_ppsf"]
copy.1[which(copy.1$parent_metro_region == "Northeast Region"),"region_mean"] <- region_avg[which(region_avg$parent_metro_region == "Northeast Region"),"med_ppsf"]
copy.1[which(copy.1$parent_metro_region == "South Region"),"region_mean"] <- region_avg[which(region_avg$parent_metro_region == "South Region"),"med_ppsf"]
copy.1[which(copy.1$parent_metro_region == "West Region"),"region_mean"] <- region_avg[which(region_avg$parent_metro_region == "West Region"),"med_ppsf"]

copy.1[which(copy.1$parent_metro_region == "Midwest Region"),"region_id"] <- 1
copy.1[which(copy.1$parent_metro_region == "Northeast Region"),"region_id"] <- 2
copy.1[which(copy.1$parent_metro_region == "South Region"),"region_id"] <- 3
copy.1[which(copy.1$parent_metro_region == "West Region"),"region_id"] <- 4

agg.8 <- aggregate(copy.1$region_id, list(copy.1$state, copy.1$parent_metro_region), mean, na.rm=T)

colnames(agg.8) <- c("state", "region", "region_mean")

plot_usmap(data=agg.8, values = "region_mean", color = "tan", labels = T
           , label_color="white") +
  scale_fill_continuous(name = "region_mean") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , plot.subtitle = element_text(hjust = 0.5, size = 24)
        #, panel.background = element_rect(fill = "transparent")
        #, plot.background = element_rect(fill = "transparent")
        #, legend.background = element_rect(fill = "transparent")
        , legend.position = "right"
        , plot.caption = element_text(hjust = 0.5, size = 18))+
  labs(title = "US Regions",
       subtitle = "Median price per Sqft.")


copy.2 <- cleaned_state_df

copy.2$state <- tolower(copy.2$state)

state_avg <- copy.2 %>% group_by(state) %>% summarise(med_ppsf = median(median_sale_price, na.rm=T))

`%!in%` <- Negate(`%in%`)

plot_usmap(data=state_avg, values = "med_ppsf", color = "tan", labels = T
           , label_color="white") +
  scale_fill_continuous(name = "", labels = scales::unit_format(unit = "K", scale = 1e-3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 36, colour = "#FFFFFF")
        , panel.background = element_rect(fill = "transparent", colour = "transparent")
        , plot.background = element_rect(fill = "transparent", colour = "transparent")
        , legend.background = element_rect(fill = "transparent", colour = "transparent")
        , legend.position = "right"
        , legend.text = element_text(colour = "#FFFFFF")
        , plot.caption = element_text(hjust = 0.5, size = 18))+
  labs(title = "Median price")

#PLOT 6
agg.9 <- aggregate(cleaned_state_df$median_ppsf_yoy, list(cleaned_state_df$parent_metro_region), mean, na.rm=T)
colnames(agg.9) <- c("region", "median_ppsf")
agg.9 %>% ggplot() + aes(x = region,y = median_ppsf, fill=region) + 
  geom_bar(stat = "identity", show.legend = F)+
  xlab("Regions") + ylab("Median price per Sqft YoY") +
  theme(plot.title = element_text(hjust = 0.5, size = 24)
        #, panel.background = element_rect(fill = "transparent")
        #, plot.background = element_rect(fill = "transparent")
        #, legend.background = element_rect(fill = "transparent")
        , legend.position = "right"
        , axis.title = element_text(hjust = 0.5, size=18)
        , axis.text = element_text(size=14)
        , plot.caption = element_text(hjust = 0.5, size = 18))+
  labs(title = "Median increase in home prices YoY per region") +
  scale_fill_discrete(name="Regions", breaks=c("Midwest Region", "Northeast Region"
                                                , "South Region", "West Region"),
                       labels=c("Midwest", "Northeast", "South", "West"))


# PLOT 7
agg.5 <- aggregate(cleaned_ny_county_df$median_ppsf, list(cleaned_ny_county_df$year, cleaned_ny_county_df$region), mean, na.rm=T)

agg.6 <- aggregate(cleaned_ny_county_df$median_ppsf, list(cleaned_ny_county_df$region), mean, na.rm=T)

colnames(agg.5) <- c("year", "region", "median_ppsf")
colnames(agg.6) <- c("state", "median_ppsf")

agg.6$state <- gsub(" County, NY", "", agg.6$state)
agg.6["polyname"] <- paste("new york,", tolower(agg.6$state), sep="")

get_fips <- function(polyname) {
  fip <- county.fips[which(county.fips$polyname %like% polyname),]$fips
  print(paste(polyname, fip, sep = ":"))
  fip
}
agg.6["fips"] <- 0
for (polyname in agg.6$polyname) {
  agg.6[agg.6$polyname == polyname,]["fips"] <- get_fips(polyname) 
}

mean_ny_ppsf <- mean(cleaned_ny_county_df$median_ppsf, na.rm = T)
agg.5[agg.5$median_ppsf >= mean_ny_ppsf,] %>% ggplot() + 
  aes(x = year,y = median_ppsf, group=region, col=region) + 
  geom_line(aes(linetype=region), size = .8) + ylim(0,1500) +
  labs(title = "Median Price Per Sqft"
       , subtitle = "Median Price Per Sqft for New York state") +
  xlab("Year") + ylab("Median Price Per Sqft")+
  scale_color_discrete(name="Counties") +
  theme(plot.title = element_text(hjust = 0.5, size = 36)
        , plot.subtitle = element_text(hjust = 0.5, size = 24)
        #, panel.background = element_rect(fill = "transparent")
        #, plot.background = element_rect(fill = "transparent")
        #, legend.background = element_rect(fill = "transparent")
        , axis.title = element_text(size = 24)
        , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        , axis.text = element_text(size=18))

plot_usmap(regions = "counties", data = agg.6, values = "median_ppsf", include = c("NY"), labels = F, color="tan", label_color = "white") +
  scale_fill_continuous(name = "Median Price Per Sqft YOY", label = scales::comma) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)
        , plot.subtitle = element_text(hjust = 0.5)
        , legend.text = element_text(color = "white"))+
  labs(title = "US States",
       subtitle = "Regions and their median price per Sqft YoY."
       , caption = "Map 1")









