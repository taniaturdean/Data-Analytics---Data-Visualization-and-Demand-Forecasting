
library(broom)
library(dbplyr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(ggrepel)
library(ggstance)
library(treemap)
library(geonames)
library(sf)
library(rpart)
library(neuralnet)
library(MASS)
library(plotly)
library(rpart.plot)
library(rattle)

ggmap::register_google(key = "AIzaSyAivaXiW_0OI-egldImvTrHf1ushy0cHvA")
options(geonamesUsername="hamv2000")

# Data Merging

trans_prod <- left_join(transactions,products,by=c('UPC'='UPC'))
trans_p_s <- left_join(trans_prod,stores[!duplicated(stores$STORE_ID),],by =c('STORE_NUM'='STORE_ID'))

# Deleting a row for price elasticity, because it has price=0 and it cannot be fit in the log function

trans_p_sPE<-trans_p_s[-382493,]


# Data Merging
train_prod <- left_join(train,products,by=c('UPC'='UPC'))
train_p_s <- left_join(train_prod,stores[!duplicated(stores$STORE_ID),],by =c('STORE_NUM'='STORE_ID'))

test_prod <- left_join(test,products,by=c('UPC'='UPC'))
test_p_s <- left_join(test_prod,stores[!duplicated(stores$STORE_ID),],by =c('STORE_NUM'='STORE_ID'))


#PED for each product

fitted_models = group_by(trans_p_sPE,UPC) %>% 
  do(model = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(fitted_models, model) 
Price_Elasticity<-tidy(fitted_models, model) 
Price_Elasticity_Percentages<-filter(Price_Elasticity, term=="log(PRICE)")


#lollipop graph (Price elasticity for each product)
Price_Elasticity_Percentages <- Price_Elasticity_Percentages %>% 
  mutate(mycolor = ifelse(estimate>0, "peachpuff3", "#A6761D"))
Price_Elasticity_Percentages$UPC<-as.character(Price_Elasticity_Percentages$UPC)
ggplot(Price_Elasticity_Percentages, aes(estimate, UPC,label=round(estimate,2))) +
  geom_segment(aes(x = 0, y = UPC, xend = estimate, yend =UPC,color=mycolor )) +
  geom_point( color="dark blue", size=0.5, alpha=0.6) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +labs(y="UPC",x="Price elasticity",
          title="Price Elasticity of Each Product")+geom_text(nudge_x = 0.01)+theme(
            plot.title = element_text(size = 30,hjust = 0.5))+geom_vline(data=Price_Elasticity_Percentages, aes(xintercept=-1),color="dark blue",linetype = "dashed", size=0.4)

#lollipop graph for BAGSNACKS

BAGSNACKS<-trans_p_sPE[trans_p_sPE$CATEGORY=="BAG SNACKS",]
fitted_models1=group_by(BAGSNACKS,UPC)%>%
  do(model1=lm(log(UNITS)~log(PRICE),data=.))
tidy(fitted_models1,model1)
Price_Elasticity1<-tidy(fitted_models1,model1)
Price_Elasticity_Percentages1<-filter(Price_Elasticity1,term=="log(PRICE)")
Price_Elasticity_Percentages1 <- Price_Elasticity_Percentages1 %>% 
  mutate(mycolor = ifelse(estimate>0, "peachpuff3", "#A6761D"))
Price_Elasticity_Percentages1$UPC<-as.character(Price_Elasticity_Percentages1$UPC)
ggplot(Price_Elasticity_Percentages1, aes(estimate, UPC,label=round(estimate,2))) +
  geom_segment(aes(x = 0, y = UPC, xend = estimate, yend =UPC,color=mycolor )) +
  geom_point( color="dark blue", size=0.5, alpha=0.6) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +labs(y="UPC",x="Price elasticity",
          title="Price Elasticity of Bag Snacks's products")+geom_text(nudge_x = 0.01)+theme(
            plot.title = element_text(size = 15,hjust = 0.5))

#lollipop graph for COLD CEREAL

COLD_CEREAL<-trans_p_sPE[trans_p_sPE$CATEGORY=="COLD CEREAL",]
fitted_models2=group_by(COLD_CEREAL,UPC)%>%
  do(model2=lm(log(UNITS)~log(PRICE),data=.))
tidy(fitted_models2,model2)
Price_Elasticity2<-tidy(fitted_models2,model2)
Price_Elasticity_Percentages2<-filter(Price_Elasticity2,term=="log(PRICE)")
Price_Elasticity_Percentages2 <- Price_Elasticity_Percentages2 %>% 
  mutate(mycolor = ifelse(estimate>0, "peachpuff3", "#A6761D"))
Price_Elasticity_Percentages2$UPC<-as.character(Price_Elasticity_Percentages2$UPC)
ggplot(Price_Elasticity_Percentages2, aes(estimate, UPC,label=round(estimate,2))) +
  geom_segment(aes(x = 0, y = UPC, xend = estimate, yend =UPC,color=mycolor )) +
  geom_point( color="dark blue", size=0.5, alpha=0.6) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +labs(y="UPC",x="Price elasticity",
          title="Price Elasticity of Cold Cereal's products")+geom_text(nudge_x = 0.01)+theme(
            plot.title = element_text(size = 15,hjust = 0.5))

#lollipop graph for FROZEN PIZZA


FROZEN_PIZZA<-trans_p_sPE[trans_p_sPE$CATEGORY=="FROZEN PIZZA" , ]
fitted_models3=group_by(COLD_CEREAL,UPC)%>%
  do(model3=lm(log(UNITS)~log(PRICE),data=.))
tidy(fitted_models3,model3)
Price_Elasticity3<-tidy(fitted_models3,model3)
Price_Elasticity_Percentages3<-filter(Price_Elasticity3,term=="log(PRICE)")
Price_Elasticity_Percentages3 <- Price_Elasticity_Percentages3 %>% 
  mutate(mycolor = ifelse(estimate>0, "peachpuff3", "#A6761D"))
Price_Elasticity_Percentages3$UPC<-as.character(Price_Elasticity_Percentages3$UPC)
ggplot(Price_Elasticity_Percentages3, aes(estimate, UPC,label=round(estimate,2))) +
  geom_segment(aes(x = 0, y = UPC, xend = estimate, yend =UPC,color=mycolor )) +
  geom_point( color="dark blue", size=0.5, alpha=0.6) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +labs(y="UPC",x="Price elasticity",
          title="Price Elasticity of Frozen Pizza's products")+geom_text(nudge_x = 0.01)+theme(
            plot.title = element_text(size = 15,hjust = 0.5))

#lollipop graph for ORAL HYGIENE PRODUCTS


ORAL_HYGIENE_PRODUCTS<-trans_p_sPE[trans_p_sPE$CATEGORY=="ORAL HYGIENE PRODUCTS",]
fitted_models4=group_by(COLD_CEREAL,UPC)%>%
  do(model4=lm(log(UNITS)~log(PRICE),data=.))
tidy(fitted_models4,model4)
Price_Elasticity4<-tidy(fitted_models4,model4)
Price_Elasticity_Percentages4<-filter(Price_Elasticity4,term=="log(PRICE)")
Price_Elasticity_Percentages4 <- Price_Elasticity_Percentages4 %>% 
  mutate(mycolor = ifelse(estimate>0, "peachpuff3", "#A6761D"))
Price_Elasticity_Percentages4$UPC<-as.character(Price_Elasticity_Percentages4$UPC)
ggplot(Price_Elasticity_Percentages4, aes(estimate, UPC,label=round(estimate,2))) +
  geom_segment(aes(x = 0, y = UPC, xend = estimate, yend =UPC,color=mycolor )) +
  geom_point( color="dark blue", size=0.5, alpha=0.6) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +labs(y="UPC",x="Price elasticity",
          title="Price Elasticity of Oral Hygiene's products")+geom_text(nudge_x = 0.01)+theme(
            plot.title = element_text(size = 15,hjust = 0.5))



#Histogram (Distribution of Price Elasticity of Demand)
Mode = function(d, adj=0.2) {
  d = density(d, adjust=adj) 
  d = data.frame(x=d$x, y=d$y)
  d$x[which.max(d$y)]
}

fnc = function(x, ...) {
  c(mean=mean(x), median=median(x), mode=Mode(x, ...))
}

labs = c("mean", "median", "mode")
ggplot(data=Price_Elasticity_Percentages, aes(estimate)) +
  geom_histogram(aes(fill=..count..), binwidth = 0.2) +
  stat_summaryh(fun.x=fnc, geom="vline",
                aes(xintercept=..x.., y=0, 
                    colour=factor(..x.., levels=unique(x)), 
                    linetype=factor(..x.., levels=unique(x)))) +
  theme_bw() +
  guides(colour=guide_legend(override.aes=list(size=3))) +
  theme(legend.key=element_rect(fill=NA, colour="white", size=3)) +
  scale_colour_discrete(name="", labels=labs) +
  scale_linetype_manual(name="", breaks=labs,
                        values=c("solid", "dashed", "dashed"))+labs(title="Distribution of Price Elasticity of Demand",x="Price Elasticity of each product", y = "Frequency")+scale_fill_gradient("Count", low = "yellow", high = "blue")

#scatterplot of demand patterns per state
par(mfrow = c(2,2))
IN<-trans_p_sPE[trans_p_sPE$ADDRESS_STATE_PROV_CODE=="IN",]
View(IN)
plot(log(IN$PRICE), log(IN$UNITS), main="Demand patterns of Indiana", cex=.1, col="#D95F02",ylab="log(units)",xlab="log(price)")
fit<-lm(log(PRICE)~log(UNITS), data=IN)
abline(fit, col=1)
KY<-trans_p_sPE[trans_p_sPE$ADDRESS_STATE_PROV_CODE=="KY",]
plot(log(KY$PRICE), log(KY$UNITS), main="Demand patterns of Kentucky", cex=.1, col="#E6AB02",ylab="log(units)",xlab="log(price)")
fit<-lm(log(PRICE)~log(UNITS), data=KY)
abline(fit, col=1)
OH<-trans_p_sPE[trans_p_sPE$ADDRESS_STATE_PROV_CODE=="OH" & trans_p_sPE$PRICE>0.01, ]
plot(log(OH$PRICE), log(OH$UNITS), main="Demand patterns of Ohio", cex=.1, col="#A6761D",ylab="log(units)",xlab="log(price)")
fit<-lm(log(PRICE)~log(UNITS), data=OH)
abline(fit, col=1)
TX<-trans_p_sPE[trans_p_sPE$ADDRESS_STATE_PROV_CODE=="TX" & trans_p_sPE$PRICE>0.01,]
plot(log(TX$PRICE), log(TX$UNITS), main="Demand patterns of Texas", cex=.1, col="peachpuff3", ylab="log(units)",xlab="log(price)")
fit<-lm(log(PRICE)~log(UNITS), data=TX)
abline(fit, col=1)

#PED for STATE_P
STATE = group_by(trans_p_sPE, ADDRESS_STATE_PROV_CODE) %>%
  do(PED = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(STATE, PED) 
STATE_PED<-tidy(STATE, PED) 
STATE_P<-filter(STATE_PED, term=="log(PRICE)")


#Bar graph (Price Elasticity of Deamand for each State)

p1<-ggplot(data=STATE_P, aes(x=ADDRESS_STATE_PROV_CODE, y=estimate,fill=ADDRESS_STATE_PROV_CODE)) +
  geom_bar(stat="identity",width=0.8)+scale_fill_manual(values=c("#D95F02", "#E6AB02", "#A6761D","peachpuff3"))+
  labs(y="Price Elasticity",x="State",fill="States",
       title="Price Elasticity of Demand for each State")+geom_text(aes(label = round(estimate,2)))+
  geom_hline(data=STATE_P, aes(yintercept=mean(estimate)),color="dark blue",linetype = "dashed", size=0.6)
p1



#treemap of Units Sold per Category
group <-trans_p_s$CATEGORY
subgroup<-paste(trans_p_s$SUB_CATEGORY)
value <- c(trans_p_s$UNITS)
data <-data.frame(group,subgroup,value)
treemap(data,
        index=c("group","subgroup"),
        vSize="value",
        type="index",palette = "Set3",
        fontsize.labels=c(15,8),                
        fontcolor.labels=c("black","dark blue"),    
        fontface.labels=c(2,1),                  
        bg.labels=c("transparent"),              
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom") ),                                   
        overlap.labels=0.5,                      
        inflate.labels=F,                       
        border.col=c("black","white"),             
        border.lwds=c(3,2)   ,
        title="Units sold per category",                     
        fontsize.title=20) 


# Google Map (Average Price Elasticity of each City)

# Data Cleaning (for map)

trans_p_s$ADDRESS_CITY_NAME<-as.character(trans_p_s$ADDRESS_CITY_NAME)
trans_p_s$ADDRESS_STATE_PROV_CODE<-as.character(trans_p_s$ADDRESS_STATE_PROV_CODE)
trans_p_s$FULL_ADDRESS<-paste(trans_p_s$ADDRESS_CITY_NAME, ",", trans_p_s$ADDRESS_STATE_PROV_CODE)
trans_p_s$FULL_ADDRESS<-as.character(trans_p_s$FULL_ADDRESS)

trans_p_sPE<-trans_p_s[-382493,]

PE_City<-(trans_p_sPE %>% 
            group_by(FULL_ADDRESS) %>% 
            do(tidy(lm(log(UNITS) ~ log(PRICE), data = .))) %>% 
            select(City = FULL_ADDRESS, PE = estimate) %>% 
            slice(2))
PE_City_df<-as.data.frame(PE_City)
US_Cities<-PE_City_df$City
US_Cities_Coordinates<-as.data.frame(geocode(US_Cities))
US_Cities_Final<-cbind(PE_City_df$PE, US_Cities_Coordinates)
p_usa <- ggmap(get_googlemap(location="Arkansas", zoom=5))
p_usa + geom_point(aes(x = lon, y = lat,  colour = PE_City_df$PE), data = US_Cities_Final, size = 2) + 
  theme(legend.position="bottom") + ggtitle("Average Price Elasticity") + labs(colour = "Price Elasticity")

#Bargraph (Units sold with respect to Promotion)
DIS<-trans_p_s %>%
  group_by(DISPLAY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
FEA<-trans_p_s %>%
  group_by(FEATURE) %>%
  summarise(AVGUNITS=mean(UNITS))

TPR<-trans_p_s %>%
  group_by(TPR_ONLY) %>%
  summarise(AVGUNITS=mean(UNITS))
promotions<- data.frame("FD" = c("Display","Display","Feature","Feature","TPR_ONLY","TPR_ONLY"), "AVGUNITS" = c(16.39510,45.67542,16.88070,49.33432,19.89581,17.79759), "Promotion" = c("NO","YES","NO","YES","NO","YES"))
str(promotions) 
ggplot(promotions,aes(x=Promotion,y=AVGUNITS,fill=FD))+
  geom_bar(stat="identity",width=0.8,position="dodge")+scale_fill_manual(values=c("lemonchiffon", "darkorchid","#333300"))+labs(y="Units sold",title="Units of products sold",fill="Promotion")

#Radar graph of average units sold for each state (Feature)

#linear regression for each state
PE_TX<-filter(trans_p_s, ADDRESS_STATE_PROV_CODE== "TX")

PE_IN<-filter(trans_p_s, ADDRESS_STATE_PROV_CODE== "IN")

PE_KY<-filter(trans_p_s, ADDRESS_STATE_PROV_CODE== "KY")

PE_OH<-filter(trans_p_s, ADDRESS_STATE_PROV_CODE== "OH")


FEA_IN<-PE_IN %>%
  group_by(FEATURE) %>%
  summarise(AVGUNITS=mean(UNITS)) 
FEA_KY<-PE_KY %>%
  group_by(FEATURE) %>%
  summarise(AVGUNITS=mean(UNITS)) 
FEA_OH<-PE_OH %>%
  group_by(FEATURE) %>%
  summarise(AVGUNITS=mean(UNITS)) 
FEA_TX<-PE_TX %>%
  group_by(FEATURE) %>%
  summarise(AVGUNITS=mean(UNITS))
p4 <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(73.49262, 67.44268,65.73552, 36.56665),
    theta = c('IN','KY','OH', 'TX'),
    name = 'Average Units For Feature',mode = 'markers'
  )%>% 
  add_trace(
    r = c(18.06246, 20.31115, 20.46171, 13.31558),
    theta = c('IN','KY','OH', 'TX'),
    name = 'Average Units For No Feature')%>%layout(title="Average units sold w/ or w/o Feature")%>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(10,80))))
p4

#bargraph with feature across states
trans_p_s %>%
  group_by(FEATURE,ADDRESS_STATE_PROV_CODE) %>%
  summarise(AVGUNITS = mean(UNITS)) %>%
  ggplot(.,aes(x=FEATURE,y=AVGUNITS,fill=ADDRESS_STATE_PROV_CODE))+
  geom_col(position="dodge")+
  scale_fill_manual(values=c("#D95F02", "#E6AB02", "#A6761D","peachpuff3"))+
  labs(y="Average units sold",fill="States", x="Feature",
       title=" Average units of products sold across 4 States with and without feature")

#Radar graph od average units sold for each state (Display)
DIS_IN<-PE_IN %>%
  group_by(DISPLAY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
DIS_KY<-PE_KY %>%
  group_by(DISPLAY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
DIS_OH<-PE_OH %>%
  group_by(DISPLAY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
DIS_TX<-PE_TX %>%
  group_by(DISPLAY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
p3 <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(43.02159, 54.96029, 55.25362, 35.36330),
    theta = c('IN','KY','OH', 'TX'),
    name = 'Average Units For Display',mode = 'lines'
  ) %>%
  add_trace(
    r = c(18.75754, 19.87917, 19.58778, 13.25803),
    theta = c('IN','KY','OH', 'TX'),
    name = 'Average Units For No Display',mode = 'markers'
  )%>%layout(title="Average units sold w/ or w/o Display")%>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(10,60)) ))
p3

#Bargraph with Display across 4 states
trans_p_s %>%
  group_by(DISPLAY,ADDRESS_STATE_PROV_CODE) %>%
  summarise(AVGUNITS = mean(UNITS)) %>%
  ggplot(.,aes(x=DISPLAY,y=AVGUNITS,fill=ADDRESS_STATE_PROV_CODE))+
  geom_col(position="dodge")+
  scale_fill_manual(values=c("#D95F02", "#E6AB02", "#A6761D","peachpuff3"))+
  labs(y="Average units sold",fill="States", x="Display",
       title=" Average units of products sold across 4 States with and without display")


#Radar graph of average units sold for each state (TPR_ONLY)
TPR_IN<-PE_IN %>%
  group_by(TPR_ONLY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
library(ggplot2)
TPR_KY<-PE_KY %>%
  group_by(TPR_ONLY) %>%
  summarise(AVGUNITS=mean(UNITS))  
library(ggplot2)
TPR_OH<-PE_OH %>%
  group_by(TPR_ONLY) %>%
  summarise(AVGUNITS=mean(UNITS)) 
library(ggplot2)
TPR_TX<-PE_TX %>%
  group_by(TPR_ONLY) %>%
  summarise(AVGUNITS=mean(UNITS)) 


p5 <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(20.88629, 23.61894,21.53348, 12.56916),
    theta = c('IN','KY','OH', 'TX'),
    name = 'Average Units For TPR_ONLY',mode = 'markers'
  )%>% 
  add_trace(
    r = c(22.21156, 23.87533, 24.15302, 15.92377),
    theta = c('IN','KY','OH', 'TX'),
    name = 'Average Units For No TPR_ONLY')%>%layout(title="Average units sold w/ or w/o TPR_ONLY")%>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(10,25)
      )
    )
  )

p5

#bargraph TPR
library(ggplot2)
trans_p_s %>%
  group_by(TPR_ONLY,ADDRESS_STATE_PROV_CODE) %>%
  summarise(AVGUNITS = mean(UNITS)) %>%
  ggplot(.,aes(x=TPR_ONLY,y=AVGUNITS,fill=ADDRESS_STATE_PROV_CODE))+
  geom_col(position="dodge")+
  scale_fill_manual(values=c("#D95F02", "#E6AB02", "#A6761D","peachpuff3"))+
  labs(y="Average units sold",fill="States", x="TPR",
       title=" Average units of products sold across 4 States with and without TPR")


# General Timeline (w/ Promotion vs. w/o Promotion Sales)

   #returning to the original format of the data without cleaning
trans_prod <- left_join(transactions,products,by=c('UPC'='UPC'))
trans_p_s <- left_join(trans_prod,stores[!duplicated(stores$STORE_ID),],by =c('STORE_NUM'='STORE_ID'))

  #change the date as date
trans_p_s$WEEK_END_DATE<-dmy(trans_p_s$WEEK_END_DATE)

promotions<-trans_p_s[trans_p_s$DISPLAY=="1",]
promotions_aggregate<-aggregate(promotions$UNITS~promotions$WEEK_END_DATE, FUN=mean)
no_promotions<-trans_p_s[trans_p_s$DISPLAY=="0",]
no_promotions_aggregate<-aggregate(no_promotions$UNITS~no_promotions$WEEK_END_DATE, FUN=mean)
feature<-trans_p_s[trans_p_s$FEATURE=="1",]
feature_aggregate<-aggregate(feature$UNITS~feature$WEEK_END_DATE, FUN=mean)
no_feature<-trans_p_s[trans_p_s$FEATURE=="0",]
no_feature_aggregate<-aggregate(no_feature$UNITS~no_feature$WEEK_END_DATE, FUN=mean)
plot(feature_aggregate$`feature$UNITS` ~ feature_aggregate$`feature$WEEK_END_DATE`, xaxt = "n", type = "l", col="red", xlab="Date", ylab="Average of Units Sold Weekly", main=" Units Sold w/ Promotion vs. w/o Promotion")
axis(1, feature_aggregate$`feature$WEEK_END_DATE`, format(feature_aggregate$`feature$WEEK_END_DATE`, "%Y %m %d"), cex.axis = .7)
lines(no_promotions_aggregate$`no_promotions$UNITS` ~ no_promotions_aggregate$`no_promotions$WEEK_END_DATE`, xaxt = "n", type = "l", col = "blue")
lines(no_feature_aggregate$`no_feature$UNITS` ~ no_feature_aggregate$`no_feature$WEEK_END_DATE`, xaxt = "n", type = "l", col = "green")
lines(display_aggregate$`display$UNITS` ~ display_aggregate$`display$WEEK_END_DATE`, xaxt = "n", type = "l", col = "purple")
legend("topright", inset=c(0.1,0.05), legend=c("Promotion","Display","Feature", "No Promotion","No Display","No Feature"),
       col=c("white", "purple","red","white","blue","green"), lty=1, cex=0.8)

#indiana timeline (w/ Promotion vs. w/o Promotion Sales)
IN<-trans_p_s[trans_p_s$ADDRESS_STATE_PROV_CODE=="IN",]
promotions<-IN[IN$DISPLAY=="1",]
promotions_aggregate<-aggregate(promotions$UNITS~promotions$WEEK_END_DATE, FUN=mean)
no_promotions<-IN[IN $DISPLAY=="0",]
no_promotions_aggregate<-aggregate(no_promotions$UNITS~no_promotions$WEEK_END_DATE, FUN=mean)
feature<-IN[IN$FEATURE=="1",]
feature_aggregate<-aggregate(feature$UNITS~feature$WEEK_END_DATE, FUN=mean)
no_feature<-IN[IN$FEATURE=="0",]
no_feature_aggregate<-aggregate(no_feature$UNITS~no_feature$WEEK_END_DATE, FUN=mean)
plot(feature_aggregate$`feature$UNITS` ~ feature_aggregate$`feature$WEEK_END_DATE`, xaxt = "n", type = "l", col="red", xlab="Date", ylab="Average of Units Sold Weekly", main=" Indiana's units Sold w/ Promotion vs. w/o Promotion")
axis(1, feature_aggregate$`feature$WEEK_END_DATE`, format(feature_aggregate$`feature$WEEK_END_DATE`, "%Y %m %d"), cex.axis = .7)
lines(no_promotions_aggregate$`no_promotions$UNITS` ~ no_promotions_aggregate$`no_promotions$WEEK_END_DATE`, xaxt = "n", type = "l", col = "blue")
lines(no_feature_aggregate$`no_feature$UNITS` ~ no_feature_aggregate$`no_feature$WEEK_END_DATE`, xaxt = "n", type = "l", col = "green")
lines(display_aggregate$`display$UNITS` ~ display_aggregate$`display$WEEK_END_DATE`, xaxt = "n", type = "l", col = "purple")
legend("topright", inset=c(0.1,0.05), legend=c("Promotion","Display","Feature", "No Promotion","No Display","No Feature"),
       col=c("white", "purple","red","white","blue","green"), lty=1, cex=0.8)

#ky timeline (w/ Promotion vs. w/o Promotion Sales)
KY<-trans_p_s[trans_p_s$ADDRESS_STATE_PROV_CODE=="KY",]
promotions<-KY[KY$DISPLAY=="1",]
promotions_aggregate<-aggregate(promotions$UNITS~promotions$WEEK_END_DATE, FUN=mean)
no_promotions<-KY[KY $DISPLAY=="0",]
no_promotions_aggregate<-aggregate(no_promotions$UNITS~no_promotions$WEEK_END_DATE, FUN=mean)
feature<-KY[KY$FEATURE=="1",]
feature_aggregate<-aggregate(feature$UNITS~feature$WEEK_END_DATE, FUN=mean)
no_feature<-KY[KY$FEATURE=="0",]
no_feature_aggregate<-aggregate(no_feature$UNITS~no_feature$WEEK_END_DATE, FUN=mean)
plot(feature_aggregate$`feature$UNITS` ~ feature_aggregate$`feature$WEEK_END_DATE`, xaxt = "n", type = "l", col="red", xlab="Date", ylab="Average of Units Sold Weekly", main=" Kentucky's units Sold w/ Promotion vs. w/o Promotion")
axis(1, feature_aggregate$`feature$WEEK_END_DATE`, format(feature_aggregate$`feature$WEEK_END_DATE`, "%Y %m %d"), cex.axis = .7)
lines(no_promotions_aggregate$`no_promotions$UNITS` ~ no_promotions_aggregate$`no_promotions$WEEK_END_DATE`, xaxt = "n", type = "l", col = "blue")
lines(no_feature_aggregate$`no_feature$UNITS` ~ no_feature_aggregate$`no_feature$WEEK_END_DATE`, xaxt = "n", type = "l", col = "green")
lines(display_aggregate$`display$UNITS` ~ display_aggregate$`display$WEEK_END_DATE`, xaxt = "n", type = "l", col = "purple")
legend("topright", inset=c(0.1,0.05), legend=c("Promotion","Display","Feature", "No Promotion","No Display","No Feature"),
       col=c("white", "purple","red","white","blue","green"), lty=1, cex=0.8)


#OH timeline (w/ Promotion vs. w/o Promotion Sales)
OH<-trans_p_s[trans_p_s$ADDRESS_STATE_PROV_CODE=="OH",]
display_oh<-OH[OH$DISPLAY=="1",]
display_aggregate_oh<-aggregate(display_oh$UNITS~display_oh$WEEK_END_DATE, FUN=mean)
no_display_oh<-OH[OH $DISPLAY=="0",]
no_display_aggregate_oh<-aggregate(no_display_oh$UNITS~no_display_oh$WEEK_END_DATE, FUN=mean)
feature_oh<-OH[OH$FEATURE=="1",]
feature_aggregate_oh<-aggregate(feature_oh$UNITS~feature_oh$WEEK_END_DATE, FUN=mean)
no_feature_oh<-OH[OH$FEATURE=="0",]
no_feature_aggregate_oh<-aggregate(no_feature_oh$UNITS~no_feature_oh$WEEK_END_DATE, FUN=mean)
plot(feature_aggregate_oh$`feature_oh$UNITS` ~ feature_aggregate_oh$`feature_oh$WEEK_END_DATE`, xaxt = "n", type = "l", col="red", xlab="Date", ylab="Average of Units Sold Weekly", main=" Ohio's units Sold w/ Promotion vs. w/o Promotion")
axis(1,feature_aggregate_oh$`feature_oh$WEEK_END_DATE`, format(feature_aggregate_oh$`feature_oh$WEEK_END_DATE`, "%Y %m %d"), cex.axis = .7)
lines(no_display_aggregate_oh$`no_display_oh$UNITS` ~ no_display_aggregate_oh$`no_display_oh$WEEK_END_DATE`, xaxt = "n", type = "l", col = "blue")
lines(no_feature_aggregate_oh$`no_feature_oh$UNITS` ~ no_feature_aggregate_oh$`no_feature_oh$WEEK_END_DATE`, xaxt = "n", type = "l", col = "green")
lines(display_aggregate_oh$`display_oh$UNITS` ~ display_aggregate_oh$`display_oh$WEEK_END_DATE`, xaxt = "n", type = "l", col = "purple")
legend("topright", inset=c(0.1,0.05), legend=c("Promotion","Display","Feature", "No Promotion","No Display","No Feature"),
       col=c("white", "purple","red","white","blue","green"), lty=1, cex=0.8)


#tx timeline
TX<-trans_p_s[trans_p_s$ADDRESS_STATE_PROV_CODE=="TX",]
promotions<-TX[TX$DISPLAY=="1",]
promotions_aggregate<-aggregate(promotions$UNITS~promotions$WEEK_END_DATE, FUN=mean)
no_promotions<-TX[TX $DISPLAY=="0",]
no_promotions_aggregate<-aggregate(no_promotions$UNITS~no_promotions$WEEK_END_DATE, FUN=mean)
feature<-TX[TX$FEATURE=="1",]
feature_aggregate<-aggregate(feature$UNITS~feature$WEEK_END_DATE, FUN=mean)
no_feature<-TX[TX$FEATURE=="0",]
no_feature_aggregate<-aggregate(no_feature$UNITS~no_feature$WEEK_END_DATE, FUN=mean)
plot(feature_aggregate$`feature$UNITS` ~ feature_aggregate$`feature$WEEK_END_DATE`, xaxt = "n", type = "l", col="red", xlab="Date", ylab="Average of Units Sold Weekly", main=" Texas's units Sold w/ Promotion vs. w/o Promotion")
axis(1, feature_aggregate$`feature$WEEK_END_DATE`, format(feature_aggregate$`feature$WEEK_END_DATE`, "%Y %m %d"), cex.axis = .7)
lines(no_promotions_aggregate$`no_promotions$UNITS` ~ no_promotions_aggregate$`no_promotions$WEEK_END_DATE`, xaxt = "n", type = "l", col = "blue")
lines(no_feature_aggregate$`no_feature$UNITS` ~ no_feature_aggregate$`no_feature$WEEK_END_DATE`, xaxt = "n", type = "l", col = "green")
lines(display_aggregate$`display$UNITS` ~ display_aggregate$`display$WEEK_END_DATE`, xaxt = "n", type = "l", col = "purple")
legend("topright", inset=c(0.1,0.05), legend=c("Promotion","Display","Feature", "No Promotion","No Display","No Feature"),
       col=c("white", "purple","red","white","blue","green"), lty=1, cex=0.8)



# Google Map (Units Sold w/ and w/o Promotion)

     # Data Cleaning (for map)
trans_p_s$FEATURE<-as.factor(trans_p_s$FEATURE)
trans_p_s$DISPLAY<-as.factor(trans_p_s$DISPLAY)
trans_p_s$ADDRESS_CITY_NAME<-as.character(trans_p_s$ADDRESS_CITY_NAME)
trans_p_s$ADDRESS_STATE_PROV_CODE<-as.character(trans_p_s$ADDRESS_STATE_PROV_CODE)
trans_p_s$FULL_ADDRESS<-paste(trans_p_s$ADDRESS_CITY_NAME, ",", trans_p_s$ADDRESS_STATE_PROV_CODE)
trans_p_s$FULL_ADDRESS<-as.character(trans_p_s$FULL_ADDRESS)

Avg_Units_Promotion<- data.frame(trans_p_s$UNITS,trans_p_s$DISPLAY, trans_p_s$FEATURE, trans_p_s$FULL_ADDRESS)
colnames(Avg_Units_Promotion)<-c("UNITS", "DISPLAY", "FEATURE", "FULL_ADDRESS")
View(Avg_Units_Promotion)
Avg_Units_Promotion$FULL_ADDRESS<-as.character(Avg_Units_Promotion$FULL_ADDRESS)
Avg_Units_Promotion<-as.data.frame(Avg_Units_Promotion %>%
                                     group_by(FULL_ADDRESS, DISPLAY, FEATURE) %>% summarise(average=mean(UNITS)))
Promotion_Cities_Coordinates<-as.data.frame(geocode(Avg_Units_Promotion$FULL_ADDRESS))
Avg_Units_P_Final<-cbind(Avg_Units_Promotion, Promotion_Cities_Coordinates)
Avg_Units_Promo<-Avg_Units_P_Final %>% filter(DISPLAY=="1" & FEATURE=="1")
Avg_Units_No_Promo<-Avg_Units_P_Final %>% filter(DISPLAY=="0" & FEATURE=="0")
p_usa <- ggmap(get_googlemap(location="Arkansas", zoom=5))

   #first map
p_usa + geom_point(aes(x = lon, y = lat,  colour = average), data = Avg_Units_Promo, size = 2) + 
  theme(legend.position="bottom") + ggtitle("Average Units Sold with Promotion") + coord_sf(xlim = c(-100, -90), ylim = c(27, 35))

    #second map
p_usa + geom_point(aes(x = lon, y = lat,  colour = average), data = Avg_Units_Promo, size = 2) + 
  theme(legend.position="bottom") + ggtitle("Average Units Sold with Promotion") + coord_sf(xlim = c(-90, -82), ylim = c(32, 41))

    #third map
p_usa + geom_point(aes(x = lon, y = lat,  colour = average), data = Avg_Units_No_Promo, size = 2) + 
  theme(legend.position="bottom") + ggtitle("Average Units Sold with No Promotion") + coord_sf(xlim = c(-100, -90), ylim = c(27, 35))

    #forth map
p_usa + geom_point(aes(x = lon, y = lat,  colour = average), data = Avg_Units_No_Promo, size = 2) + 
  theme(legend.position="bottom") + ggtitle("Average Units Sold with No Promotion") + coord_sf(xlim = c(-90, -82), ylim = c(32, 41))

#PED (Linear regression: Price Elasticity)

fitted_models = group_by(trans_p_sPE,UPC) %>% 
  do(model = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(fitted_models, model) 
Price_Elasticity<-tidy(fitted_models, model) 
Price_Elasticity_Percentages<-filter(Price_Elasticity, term=="log(PRICE)")
View(Price_Elasticity_Percentages)


#PED to view the Price Elasticity for each state
STATE = group_by(trans_p_sPE, ADDRESS_STATE_PROV_CODE) %>%
  do(PED = lm(log(UNITS) ~ log(PRICE), data = .))
tidy(STATE, PED) 
STATE_PED<-tidy(STATE, PED) 
STATE_P<-filter(STATE_PED, term=="log(PRICE)")
View(STATE_P)

#Linear Regression: Impact of Promotions on units sold
lm.fit=lm(UNITS~DISPLAY + TPR_ONLY+ FEATURE ,data=trans_p_s)
summary(lm.fit)

#transform in columns in numeric 
test_p_s_copy<-test_p_s
test_p_s_copy$SPEND<-as.numeric(test_p_s_copy$SPEND)
test_p_s_copy$PRICE<-as.numeric(test_p_s_copy$PRICE)
test_p_s_copy$BASE_PRICE<-as.numeric(test_p_s_copy$BASE_PRICE)
train_p_s_copy<-train_p_s
train_p_s_copy$SPEND<-as.numeric(train_p_s_copy$SPEND)
train_p_s_copy$PRICE<-as.numeric(train_p_s_copy$PRICE)
train_p_s_copy$BASE_PRICE<-as.numeric(train_p_s_copy$BASE_PRICE)



# reg line pred demand 8 variables

lm.fit=lm(UNITS~PRICE+BASE_PRICE+FEATURE+DISPLAY+TPR_ONLY+CATEGORY+MANUFACTURER+ADDRESS_CITY_NAME,data=train_p_s_copy)
summary(lm.fit) 

# reg tree pred demand 8 variables 

tree=rpart(UNITS~PRICE+BASE_PRICE+FEATURE+DISPLAY+TPR_ONLY+CATEGORY+MANUFACTURER+ADDRESS_CITY_NAME,data=train_p_s_copy)

    # first tree
plot(tree,margin=0.000001,cex=0.00001)
text(tree,pretty=TRUE)

    # second tree
plot(tree,margin=0.0001,cex=0.001)
text(tree,pretty=TRUE)
prp(tree)

#in-sample fit- 8 variables
yhat.reg=predict(lm.fit,newdata=train_p_s_copy) 
yhat.tree=predict(tree,newdata=train_p_s_copy)
inrmsereg=sqrt(mean((yhat.reg-train_p_s_copy$UNITS)^2))
inrmsetree=sqrt(mean((yhat.tree-train_p_s_copy$UNITS)^2))



#out-of-sample fit-8 variables
out.reg=predict(lm.fit,newdata=test_p_s_copy)
out.tree=predict(tree,newdata=test_p_s_copy)
outrmsereg=sqrt(mean((out.reg-test_p_s_copy$UNITS)^2))
outrmsetree=sqrt(mean((out.tree-test_p_s_copy$UNITS)^2))

#see rmse s- 8 variables 
print(inrmsereg)
print(outrmsereg)
print(inrmsetree)
print(outrmsetree)

#linear regression for the 5 chosen variables
lm.fit2=lm(UNITS~DISPLAY + TPR_ONLY+ FEATURE + PRICE + BASE_PRICE ,data=train_p_s_copy)
summary(lm.fit2)

#Regression tree for the 5 chosen variables 
tree.fit = rpart(UNITS~DISPLAY + TPR_ONLY+ FEATURE + PRICE + BASE_PRICE,train_p_s_copy, method="anova")
plot(tree.fit, uniform=TRUE, margin =0.1,cex=0.5)
text(tree.fit)
prp(tree.fit,main="How each variable affects units")

#in-sample fit
yhat.reg2=predict(lm.fit2, newdata=train_p_s_copy)
yhat.tree2=predict(tree.fit,newdata=train_p_s_copy)
sqrt(mean((yhat.reg2 - train_p_s_copy$UNITS)^2))
sqrt(mean((yhat.tree2- train_p_s_copy$UNITS)^2))

#out-of-sample fit
yhat.reg3=predict(lm.fit2,newdata=test_p_s_copy)
yhat.tree3=predict(tree.fit,newdata= test_p_s_copy)
sqrt(mean((yhat.reg3 - test_p_s_copy$UNITS)^2))
sqrt(mean((yhat.tree3- test_p_s_copy$UNITS)^2))

#neural-net-best
f <- as.formula(paste("UNITS~PRICE+TPR_ONLY+DISPLAY+BASE_PRICE+FEATURE"))
nn <-neuralnet(f,data=train_p_s_copy,hidden=2, stepmax = 1e7, rep = 3,err.fct="sse",linear.output = T)

plot(nn)

#neural-net-best-errors

#in-sample fit

yhat.net=predict(nn,train_p_s_copy)

inrmsenet=sqrt(mean((yhat.net-train_p_s_copy$UNITS)^2))

print(inrmsenet)


#out-of-sample fit

out.net=predict(nn,test_p_s_copy)

outrmsenet=sqrt(mean((out.net-test_p_s_copy$UNITS)^2))

print(outrmsenet)

#use best model to predict

tree=rpart(UNITS~PRICE+BASE_PRICE+FEATURE+DISPLAY+TPR_ONLY+CATEGORY+MANUFACTURER+ADDRESS_CITY_NAME,data=train_p_s_copy)


predict(tree,newdata=data.frame(PRICE=0.98,BASE_PRICE=1.58,FEATURE=0,DISPLAY=0,TPR_ONLY=1,MANUFACTURER="PRIVATE LABEL",CATEGORY="ORAL HYGIENE PRODUCTS",ADDRESS_CITY_NAME="DENTON"))


data=test_p_s_copy

predict(tree,newdata=data.frame(PRICE=4.76,BASE_PRICE=4.76,FEATURE=0,DISPLAY=0,TPR_ONLY=0,MANUFACTURER="WARNER",CATEGORY="ORAL HYGIENE PRODUCTS",ADDRESS_CITY_NAME="LAWRENCEBURG"))






