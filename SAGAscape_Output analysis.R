library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(plotly)
library(magrittr)
library(patchwork)


#### Gathering data ####

# setwd("pathToData")

result1 <- read.csv("./SagaScape CAA result1.csv",
                sep=",",
                header=TRUE,
                skip=6
)


result2 <- read.csv("./SagaScape CAA result2.csv",
                 sep=",",
                 header=TRUE,
                 skip=6
)

result3 <- read.csv("./SagaScape CAA result3.csv",
                 sep=",",
                 header=TRUE,
                 skip=6
)

result4 <- read.csv("./SagaScape CAA result4.csv",
                 sep=",",
                 header=TRUE,
                 skip=6
)

result5 <- read.csv("./SagaScape CAA result5.csv",
                 sep=",",
                 header=TRUE,
                 skip=6
) 

result6 <- read.csv("./SagaScape CAA result6.csv",
                 sep=",",
                 header=TRUE,
                 skip=6
)

result_list <- list(result1,result2,result3,result4,result5,result6)

#### Cleaning data ####

# Remove uninformative columns
for (i in c(1:6)){
  result_list[[i]]$landuse.visualization <- NULL
  result_list[[i]]$time.limit <- NULL
}

# Informative names for remaining columns
for (i in c(1:6)){
  colnames(result_list[[i]]) <- c(colnames(result_list[[i]])[1:13],
                               "step",
                               "saved.food.workdays","saved.wood.workdays","saved.clay.workdays",
                               "cumulative.food.stock","cumulative.wood.stock","cumulative.clay.stock",
                               "total.food.effort","total.wood.effort","total.clay.effort",
                               "forest.patches","agricultural.patches")
  
}

# Prepare dataframe to fill with data entries from splitting output strings
# 1 row per timestep, 1 column per variable - community combination, 32 community population columns

for(a in 1:6){
  additional_cols <-c()  
  for(i in 15:23){
    additional_cols <- append(additional_cols,paste0(colnames(result_list[[a]])[i],".",0:31))
  }
  
  additional_cols <- append(additional_cols,paste0("population",".",0:31))
  splitPerCommunity <- matrix(nrow = nrow(result_list[[a]]),
                              ncol = length(additional_cols))
  colnames(splitPerCommunity)<-additional_cols
  assign(paste0("splitPerCommunity",'.',a),splitPerCommunity)
}

# Helper functions to split up output data into separate entries. 
# NetLogo output cannot be split automatically in Excel due to the presence of multiple entries in one cell 

DFmaker <- function(x){ # Function that splits up a vector into even and uneven elements. 
  a <- x[c(TRUE,FALSE)]
  b <- x[c(FALSE,TRUE)]
  return(cbind(a,b))
}

DFextender <- function(x){ # Function to add in NAs for entries belonging to communities that are missing in certain year (e.g. Hellenistic community during Iron Age)
  a <- which(!c(0:31)%in%x[,1]) - 1
  b <- rep_len(NA,length(a))
  a <- c(x[,1],a)
  b <- c(x[,2],b)
  c <- cbind(a,b)
  return(c)
}

splitter <- function(column){ # Actual splitting function
  splitList <- sapply(as.list(column),function(x){str_split(x," ")})
  splitList <- sapply(splitList, function(x){gsub("[[]","",x)})
  splitList <- sapply(splitList, function(x){gsub("[]]","",x)})
  splitList <- sapply(splitList, function(x){as.numeric(x)})
  
  splitList<- sapply(splitList,DFmaker)
  
  splitList <- lapply(splitList,DFextender)
  
  splitList <- lapply(splitList,function(x){
    x[order(x[,1]),2]
  })
}

for(a in 1:6){
  
  assign(paste0("split_SWW",".",a),splitter(result_list[[a]]$saved.wood.workdays))
  assign(paste0("split_SCW",".",a),splitter(result_list[[a]]$saved.clay.workdays))
  assign(paste0("split_CFS",".",a),splitter(result_list[[a]]$cumulative.food.stock))
  assign(paste0("split_CWS",".",a),splitter(result_list[[a]]$cumulative.wood.stock))
  assign(paste0("split_CCS",".",a),splitter(result_list[[a]]$cumulative.clay.stock))
  assign(paste0("split_TFE",".",a),splitter(result_list[[a]]$total.food.effort))
  assign(paste0("split_TWE",".",a),splitter(result_list[[a]]$total.wood.effort))
  assign(paste0("split_TCE",".",a),splitter(result_list[[a]]$total.clay.effort))
  
  split_SFW <- sapply(as.list(result_list[[a]]$saved.food.workdays),function(x){str_split(x," ")}) # "saved.food.workdays" output variable also contains population count. Needs a small adaptation of standard procedure.
  split_SFW <- sapply(split_SFW, function(x){gsub("[[]","",x)})
  split_SFW <- sapply(split_SFW, function(x){as.numeric(gsub("[]]","",x))})
  split_SFW <- lapply(split_SFW,function(x){
    com <- x[c(TRUE,FALSE,FALSE)]
    pop <- x[c(FALSE,TRUE,FALSE)]
    FW <- x[c(FALSE,FALSE,TRUE)]
    return(cbind(com,pop,FW))
  })
  
  split_SFW <- lapply(split_SFW,function(x){
    com<- which(!c(0:31)%in%x[,1]) - 1
    pop <- rep_len(NA,length(com))
    FW <- rep_len(NA,length(com))
    return(rbind(x,cbind(com,pop,FW)))
  })
  
  assign(paste0("split_SFW",".",a),lapply(split_SFW,function(x){
    x[order(x[,1]),]
  }))
  
}

for(a in 1:6){ # Filling dataframe with split entries
  splitPerCommunity <- get(paste0("splitPerCommunity",".",a))
  split_SFW <-get(paste0("split_SFW",".",a))
  split_SWW <-get(paste0("split_SWW",".",a))
  split_SCW <-get(paste0("split_SCW",".",a))
  split_CFS <-get(paste0("split_CFS",".",a))
  split_CWS <-get(paste0("split_CWS",".",a))
  split_CCS <-get(paste0("split_CCS",".",a))
  split_TFE <-get(paste0("split_TFE",".",a))
  split_TWE <-get(paste0("split_TWE",".",a))
  split_TCE <-get(paste0("split_TCE",".",a))
  for(i in 1:nrow(result_list[[a]])){
    
    splitPerCommunity[i,1:32] <- split_SFW[[i]][,3]
    splitPerCommunity[i,33:64] <- split_SWW[[i]]
    splitPerCommunity[i,65:96] <- split_SCW[[i]]
    splitPerCommunity[i,97:128] <- split_CFS[[i]]
    splitPerCommunity[i,129:160] <- split_CWS[[i]]
    splitPerCommunity[i,161:192] <- split_CCS[[i]]
    splitPerCommunity[i,193:224] <- split_TFE[[i]]
    splitPerCommunity[i,225:256] <- split_TWE[[i]]
    splitPerCommunity[i,257:288] <- split_TCE[[i]]
    splitPerCommunity[i,289:320] <- split_SFW[[i]][,2]
  } 
  assign(paste0("splitPerCommunity",".",a),splitPerCommunity)
}

# Recompiling with run parameters from original dataset

for(a in 1:6){
  assign(paste0("DF_",a),
         cbind.data.frame(result_list[[a]][,1:14],
                          get(paste0("splitPerCommunity",".",a)),
                          result_list[[a]][,24:25]))
  
  DF <- get(paste0("DF_",a))
  colnames(DF)[1] <-"run"
  DF <- cbind(DF,select(DF,contains('population'))%>%rowSums(na.rm=TRUE)) # adding total population column
  colnames(DF)[ncol(DF)] <- "total.population"
  DF$run<-as.factor(DF$run)
  assign(paste0("DF_",a),DF)
}

# Adding separate datasets together

DF_1 <- DF_1 %>% arrange(run)
DF_2 <- DF_2 %>% arrange(run)
DF_3 <- DF_3 %>% arrange(run)
DF_4 <- DF_4 %>% arrange(run)
DF_5 <- DF_5 %>% arrange(run)
DF_6 <- DF_6 %>% arrange(run)

DF_2$run <- as.factor(as.numeric(DF_2$run) + length(unique(DF_1$run)))
DF_3$run <- as.factor(as.numeric(DF_3$run) + length(unique(DF_1$run)) + length(unique(DF_2$run)))
DF_4$run <- as.factor(as.numeric(DF_4$run) + length(unique(DF_1$run)) + length(unique(DF_2$run)) + length(unique(DF_3$run)))
DF_5$run <- as.factor(as.numeric(DF_5$run) + length(unique(DF_1$run)) + length(unique(DF_2$run)) + length(unique(DF_3$run)) + length(unique(DF_4$run)))
DF_6$run <- as.factor(as.numeric(DF_6$run) + length(unique(DF_1$run)) + length(unique(DF_2$run)) + length(unique(DF_3$run)) + length(unique(DF_4$run)) + length(unique(DF_5$run)))

DF <- rbind.data.frame(DF_1,
                       DF_2,
                       DF_3,
                       DF_4,
                       DF_5,
                       DF_6)

# Memory can become quite strained at this point. It helps to save DF, clear the full memory space and load DF back in.

#save(DF,file = "DF.R")
#load("DF.R")

# Melting dataset in order to split columns into variable - community pairs (first melting & splitting, then casting).

DF_melt <- DF%>%
  pivot_longer(cols = contains(c( "workdays.",
                                  "stock.",
                                  "effort.",
                                  "population.")),
               names_to ="variable"
               
  )

# Splitting "variable" column up into variable and community number
newName <- str_split_fixed(DF_melt$variable,"\\.",ifelse(grepl("population",DF_melt$variable),2,4)) # When "variable" column contains a population number,it has only a 2-part structure instead of the regular 4 parts.
newName[newName[,1]=="population",4] <- newName[newName[,1]=="population",2]
newName[newName[,1]=="population",2] <-""

newName <- cbind(paste(newName[,1],newName[,2],newName[,3],sep="."),newName[,4])
newName[endsWith(newName[,1],".."),1] <- "population"

DF_melt$variable <- newName[,1]
DF_melt$com <- newName[,2] # Adding in new "community" column.

# Same remark as before on memory constraints

#save(DF_melt,file="DF_melt.R")
#load("DF_melt.R")

# Casting back into 'wider' dataframe, allowing for easier per capita calculations for separate communities

DF_melt <- DF_melt %>% 
  pivot_wider(names_from=variable,values_from = value)

# Adding in site names, periodization and settlement type
Site_table <-data.frame(com = as.character(c(0:31)),
                        name = c("Sagalassos","Duzen Tepe","Korustan","Sandalion","Aykirikca", "Hisar","Seydikoy","Beloren","Catal Pinar","Kayis Kale",
                                 "Kepez Kalesi","Kokez Kale","Bereket","Hacilar","Kozluca","Duver Yarimada","Koca Pinar","Duver Cay 1","Duver Cay 3","Duver SE 1",
                                 "South of Saga","East of Catal Oluk","2004 site","Susakli","Akyamac","F085","Kapikaya","Cingirakli","Kale Mevkii","Oren","Taskapi Kale","Duver East"),
                        start = c("IA"),
                        altitude = c(1472,1400,1270,897,1185,1054,822,1283,1184,1399,1156,1840,1501,1037,971,940,919,922,
                                     910,949,1376,1269,1298,1302,1154,1159,1430,1240,1513,961,1589,920)
)

Site_table$start[Site_table$name%in%c("Sagalassos","Duzen Tepe","South of Saga","F085","Beloren","Catal Pinar")] <- "ACH" # List of Achaemenid settlements
Site_table$start[Site_table$name%in%c("Bereket","East of Catal Oluk","2004 site","Susakli",'Akyamac',"Kale Mevkii","Kapikaya","Cingirakli","Oren","Taskapi Kale")] <- "HELL" #List of Hellenistic settlements

DF_melt <- full_join(DF_melt,Site_table,by="com")
DF_melt$retrofitted.step <- recode(DF_melt$start, # The "retrofitted step" variable simplifies calculations using the number of timesteps for communities that join the simulation at a later stage (ACH and HELL)
                                   "IA" = as.character(DF_melt$step),
                                   "ACH" = as.character(DF_melt$step - 450),
                                   "HELL" = as.character(DF_melt$step - 650))

DF_melt$retrofitted.step %<>% as.numeric()

# Averaging out runs with same settings.

DF_averaged <-DF_melt %>% 
  group_by(clay.demand.pc,
           clay.threshold,
           food.demand.pc,
           active.percentage,
           wood.demand.pc,
           regeneration.time,
           step,
           com,
           name,start,altitude,retrofitted.step) %>%
  summarise(forest = mean(forest.patches,na.rm = TRUE),
            agri = mean(agricultural.patches,na.rm = TRUE),
            total.population = mean(total.population,na.rm = TRUE),
            saved.food.workdays = mean(saved.food.workdays,na.rm=TRUE),
            saved.wood.workdays = mean(saved.wood.workdays,na.rm = TRUE),
            saved.clay.workdays = mean(saved.clay.workdays,na.rm = TRUE),
            cumulative.food.stock = mean(cumulative.food.stock,na.rm = TRUE),
            cumulative.wood.stock = mean(cumulative.wood.stock,na.rm = TRUE),
            cumulative.clay.stock = mean(cumulative.clay.stock,na.rm = TRUE),
            total.food.effort = mean(total.food.effort,na.rm = TRUE),
            total.wood.effort = mean(total.wood.effort,na.rm = TRUE),
            total.clay.effort = mean(total.clay.effort,na.rm = TRUE),
            population = mean(population,na.rm = TRUE)
  )

#save(DF_averaged,file="DF_averaged.R")
#load("DF_averaged.R")


#### Analysis ####

# Creation of Figure 4: evolution of agricultural area/forest area

DF_averaged %>%
  filter(food.demand.pc%in%c(0.75,1.5) & wood.demand.pc%in%c(1,4))%>%
  ggplot(aes(x = step,
             y = agri/forest,
             color = as.factor(regeneration.time)))+
  geom_line()+
  facet_grid(rows = vars(wood.demand.pc),cols = vars(food.demand.pc))+ # Important: Clear effect of food demand on number of agricultural patches. Small effect of active percentage. 
  xlab("Timestep")+
  ylab("Agricultural area / forest area")+
  labs(color="Fertility regeneration time (years)")+
  theme_minimal()


# Separation of dataset into 4 different scenarios

low <- DF_averaged[DF_averaged[['clay.demand.pc']] == 1 &
                       DF_averaged[['clay.threshold']] == 0.25 &
                       DF_averaged[['food.demand.pc']] == 1.5 & 
                       DF_averaged[["wood.demand.pc"]] == 1 & 
                       DF_averaged[['active.percentage']]==25 &
                       DF_averaged[["regeneration.time"]] == 2,]

intermed <- DF_averaged[DF_averaged[['clay.demand.pc']] == 1 &
                          DF_averaged[['clay.threshold']] == 0.25 &
                          DF_averaged[['food.demand.pc']] == 1.50 &
                          DF_averaged[["active.percentage"]]== 25 &
                          DF_averaged[["wood.demand.pc"]] == 4 &
                          DF_averaged[["regeneration.time"]] ==3,]

high_FAndW <- DF_averaged[DF_averaged[['wood.demand.pc']] == 10 & 
                        DF_averaged[['clay.threshold']] == 0.25 &
                        DF_averaged[['clay.demand.pc']] == 1,]

high_C <- DF_averaged[DF_averaged[['food.demand.pc']] == 1.5 & 
                        DF_averaged[['clay.threshold']] == 0.5 &
                        DF_averaged[['clay.demand.pc']] == 10 &
                        DF_averaged[['active.percentage']] == 10 & 
                        DF_averaged[['wood.demand.pc']] == 4 & 
                        DF_averaged[['regeneration.time']] == 3,]

# Adding in altitude-determined wood demand 
# Wood demand is determined partly by altitude. This actual demand was present in the model runs, but was not registered as a parameter.
# It can be calculated again as in the original model. 

altitudeAdapter <- function(alt,baseDemand){return(baseDemand + 0.0661/365*alt)}

low %<>% mutate(wood.demand.pc = altitudeAdapter(altitude,wood.demand.pc))
intermed %<>% mutate(wood.demand.pc = altitudeAdapter(altitude,wood.demand.pc))
high_FAndW %<>% mutate(wood.demand.pc = altitudeAdapter(altitude,wood.demand.pc))
high_C %<>% mutate(wood.demand.pc = altitudeAdapter(altitude,wood.demand.pc))

# Creation of subplots Figures 5-6: Resource stock/resource requirement
# For example: evolution of food stocks/annual requirement over time in low scenario

low %>%
  ggplot(aes(x = step,
             y = (cumulative.food.stock * 1000 / (population * retrofitted.step * food.demand.pc * 365)), #conversion to tonnes and annual requirement
             color = name))+
  geom_line()+
  ylim(0.75,1.75)+
  geom_hline(yintercept = 1)+ # 1 -line= food demand is met.
  facet_wrap(facets=vars(factor(start,levels=c("IA","ACH","HELL"))))+
  theme_minimal()

# Evolution of wood stocks/annual requirement over time in low scenario

low %>%
  ggplot(aes(x = step,
             y = (cumulative.wood.stock / (population * retrofitted.step * wood.demand.pc * 365/695)), # conversion to m³ and annual requirement
             color = name))+
  geom_line()+
  ylim(0.75,5)+
  geom_hline(yintercept = 1)+
  facet_wrap(facets=vars(factor(start,levels=c("IA","ACH","HELL"))))+
  theme_minimal()

# Evolution of clay stocks/annual requirement over time in low scenario

low %>%
  ggplot(aes(x = step,
             y = (cumulative.clay.stock * 1000 / (population * retrofitted.step * clay.demand.pc)), # conversion to tonnes. Clay demand pc is already expressed on annual basis.
             color = name))+
  geom_line()+
  ylim(0.75,3)+ 
  geom_hline(yintercept=1)+
  facet_wrap(facets=vars(factor(start,levels=c("IA","ACH","HELL"))))+
  theme_minimal()

# Creation of Figures 7 and 8: Average walking times to agricultural plots and to exploited forest stands for Sagalassos and Düzen Tepe

# New dataframe from four scenarios, only Sagalassos and Düzen Tepe singled out
SandDT <- rbind.data.frame(cbind.data.frame(filter(low,name%in%c("Sagalassos","Duzen Tepe")),setting=c("low")),
                           cbind.data.frame(filter(intermed,name%in%c("Sagalassos","Duzen Tepe")),setting=c("intermed")),
                           cbind.data.frame(filter(high_FAndW,name%in%c("Sagalassos","Duzen Tepe")),setting=c("high_FAndW")),
                           cbind.data.frame(filter(high_C,name%in%c("Sagalassos","Duzen Tepe")),setting=c("high_C"))
)

SandDT %<>%
  arrange(setting,name,step)%>%
  ungroup()%>%
  mutate(foodDiff = cumulative.food.stock - lag(cumulative.food.stock), # Calculate total food gathered this year from difference in cumulative stock
         woodDiff = cumulative.wood.stock - lag(cumulative.wood.stock), # Calculate total wood gathered this year from difference in cumulative stock
         foodWDDiff = saved.food.workdays - lag(saved.food.workdays), # Calculate workdays spent on agriculture from difference between years
         woodWDDiff = saved.wood.workdays - lag(saved.wood.workdays), # Calculate workdays spent on wood exploitation from difference between years
         foodEffDiff = total.food.effort - lag(total.food.effort), # Calculate annual total walking time to agricultural plots (counted once) from difference between consecutive years
         woodEffDiff = total.wood.effort - lag(total.wood.effort), # Calculate annual total walking time to wooded plots (counted once) from difference between consecutive years
         noPlots = (foodWDDiff - 42 * 2/10 * foodEffDiff)/42, # Calculate number of agricultural plots used, taking into account back-and-forth movement, 10 hours per working day and 42 working days per ha
         woodEffAv = 29.21/695 * 10/2 * (woodWDDiff / woodDiff - 49/10), # Calculate average walking time to wood exploitation location, taking into account back-and-forth movement, 10 hours per working day, head loads of (on average) 29.21 kg, (average) gathering time of 49 hours per m³ and a wood density of 695 kg per m³
         woodEffLo = woodEffAv*4.5/29.21,
         woodEffHi = woodEffAv*57.5/29.21
         )

# Figure 7: agriculture

plot7 <- SandDT %>%
  arrange(foodDiff)%>%
  ggplot(aes(x=step,
             y=foodEffDiff/noPlots, # Show average walking distance per year
             color=foodDiff))+
  geom_smooth(method=lm,color='red',fullrange=TRUE)+
  geom_jitter()+
  xlim(400,1050)+
  scale_color_gradientn(colours=hcl.colors(3),limits=c(100,1400))+
  facet_grid(cols=vars(ifelse(name=="Duzen Tepe","Düzen Tepe","Sagalassos")),
             rows=vars(factor(setting,
                              levels=c("low","intermed","high_FAndW","high_C"),
                              labels=c("Low","Intermediate","High F&W","High C"))))+
  theme_minimal()+
  labs(color="Tons of\nfood harvested")+
  xlab("Timestep")+
  ylab("Average walking time (hours)")+
  theme(axis.title.x = element_text(margin = margin(t = 30)),
        axis.title.y = element_text(margin = margin(r = 30)))


# Figure 8: wood exploitation

plot8 <- SandDT %>%
  arrange(woodDiff)%>%
  ggplot(aes(x=step,
             y=woodEffAv,
             color=log(woodDiff)))+
  geom_smooth(method=lm,color='red',fullrange=T)+
  geom_jitter()+
  xlim(400,1050)+
  scale_color_gradientn(colours=hcl.colors(3),limits=c(2,11.5))+
  facet_grid(cols=vars(ifelse(name=="Duzen Tepe","Düzen Tepe","Sagalassos")),
             rows=vars(factor(setting,
                              levels=c("low","intermed","high_FAndW","high_C"),
                              labels=c("Low","Intermediate","High F&W","High C"))))+
  theme_minimal()+
  labs(color="log (Tons of\nwood harvested)")+
  xlab("Timestep")+
  ylab("Average walking time (hours)")+
  theme(axis.title.x = element_text(margin = margin(t = 30)),
        axis.title.y = element_text(margin = margin(r = 30)))

# Calculation of average agricultural area per capita

avArea <- DF_averaged %>% 
  filter(agri>0)%>%
  mutate(agriPC = agri/total.population)
  
mean(avArea$agriPC)
sd(avArea$agriPC)


##### Analysis of forest regeneration and fire activity in absence of humans (Supplementary Materials) ####
fireAndForest <- read.csv("./SagaScape Forest standing stock evolution-table.csv", # load in model runs without settlements
                          header=F,
                          skip=7)

fireAndForest[,2:15] <- NULL
colnames(fireAndForest) <- c("run","step","standingStock","burnsize")
fireAndForest$burnsize <- gsub("[[]","",fireAndForest$burnsize)
fireAndForest$burnsize <- gsub("[]]","",fireAndForest$burnsize)
burnsize <- lapply(fireAndForest$burnsize,function(x){as.numeric(unlist(str_split(x," ")))})
fireAndForest$noFires <- sapply(burnsize,function(x){length(x[is.na(x)==FALSE])}) # number of separate fire events
fireAndForest$totalBurned <- sapply(burnsize,sum)%>% # total area of burned patches in ha
  replace_na(0)
fireAndForest$forest <- 296471 - fireAndForest$totalBurned # calculate remaining number of forested patches. Maximum patches that can carry forest cover = 296471

# Creation of figure S1 (Supplementary Materials) 

avg_remaining_forest <- fireAndForest%>%
  group_by(step)%>%
  summarize(forest=mean(forest))%>%
  mutate(setting="none")

forestVis <- rbind.data.frame(cbind.data.frame(filter(low,name=="Kepez Kalesi"),setting=c("low")), # Kepez Kalesi chosen as a random Iron Age settlement (choice makes no difference)
                              cbind.data.frame(filter(intermed,name=="Kepez Kalesi"),setting=c("intermed")),
                              cbind.data.frame(filter(high_FAndW,name=="Kepez Kalesi"),setting=c("high_FAndW")),
                              cbind.data.frame(filter(high_C,name=="Kepez Kalesi"),setting=c("high_C"))
                              )%>%
  select(step,forest,setting)

forestVis <- rbind.data.frame(forestVis,avg_remaining_forest)
forestVis$setting <- factor(forestVis$setting,c("none","low","intermed","high_FAndW","high_C"))

plotS1<-forestVis %>% 
  ggplot(aes(x=step,
             y=100*forest/296471,
             color=setting)) + 
  geom_line()+
  ylim(60,100)+
  theme_minimal()

# Creation of figure S2 (Supplementary Materials)
plotS2<- SandDT %>%
  arrange(woodDiff)%>%
  ggplot(aes(x=step,
             y=woodEffAv,
             color=log(woodDiff)))+
  geom_smooth(method=lm,color='red',fullrange=T)+
  geom_jitter(size=0.8)+
  geom_errorbar(aes(ymin=woodEffLo,ymax=woodEffHi),alpha=0.2)+
  xlim(400,1050)+
  ylim(0,12.5)+ # All data points are plotted, but not all errorbars. 
  scale_color_gradientn(colours=hcl.colors(3),limits=c(2,11.5))+
  facet_grid(cols=vars(ifelse(name=="Duzen Tepe","Düzen Tepe","Sagalassos")),
             rows=vars(factor(setting,
                              levels=c("low","intermed","high_FAndW","high_C"),
                              labels=c("Low","Intermediate","High F&W","High C"))))+
  theme_minimal()+
  labs(color="log (Tons of\nwood harvested)")+
  xlab("Timestep")+
  ylab("Average walking time (hours)")+
  theme(axis.title.x = element_text(margin = margin(t = 30)),
        axis.title.y = element_text(margin = margin(r = 30)))


