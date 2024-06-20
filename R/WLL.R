################################################################################
## Project: WLL-FBM
## Script purpose:  equations for a forest biodiversity model that is driven by
##                  wood, liter, and light.
## Date: Fri Jun 14 08:10:29 2024
## Author: Yannek Kaeber <y.kaeber@posteo.de>
################################################################################
library(data.table)

# the input csv has the 6 columns with different attributes. This table defines
# the rates with which carbon flows are distributed in the system.
# Each row represents a flow rate between two components of the model.
# source: 
#   "wood" - woody components
#   "sapro" - saprotroph organism groups that have wood as energy source
#   "predator" - predators that have saprotroph organism groups or other 
#                predators as energy source
# h_level_source: 
#   The horizontal level indicating the wood decomposition stage of "wood", and
#   "sapro" relying on this stage of wood decomposition.
#   1 is are living trees including litter. Stages 2 and higher are 
#   stages of increasing decomposition. The highest stage is always defined as
#   the final decomposition stage which corresponds to soil organic carbon that
#   is not part of any chemical storage
# 
parameters_dt <- fread("data/parameters/rates/rates-rates.csv")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## initialize model ####
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

initial_wood = c(100, 0, 0, 0)

horizontal_levels = sort(unique(parameters_dt$hID))
Nhorizontal = sum(horizontal_levels > 0)
vertical_levels = sort(unique(parameters_dt$vID))
Nvertical = sum(vertical_levels > 0)

biomass <- list(
  wood = initial_wood,
  sapro = rep(0, length(initial_wood)-1),
  predator = rep(0,Nvertical)
)

atmospheric_carbon = 0
soc = 0

#### set parameters ####
parameters <- list()
for(rate_type_i in c("feeding", "decomp_frac", "respiration")){
  parameters[["sapro"]][[rate_type_i]] <- as.matrix(parameters_dt[agent_type == "sapro" & rate_type == rate_type_i, .(hID, vID, value)])
}

parameters[["predators"]][["feeding"]] <- as.matrix(parameters_dt[agent_type == "predator" & rate_type == "feeding", .(hID, vID, value)])
parameters[["predators"]][["respiration"]] <- as.matrix(parameters_dt[agent_type == "predator" & rate_type == "respiration", .(hID, vID, value)])

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## plot model ####
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

library(ggplot2)

model_stucture_dt <- rbind(
  data.table(
    agent_type = c("alive", rep("dead",Nhorizontal-1)  ,"soil"),
    hID = 1:(length(horizontal_levels)),
    vID = 1
  ),
  parameters_dt,fill =T)
model_stucture_dt[,boxID := paste0(agent_type, "_", hID, vID),]

boxes <- unique(model_stucture_dt[, .(agent_type, boxID, hID, vID)])
boxes[, ":="(x = as.numeric(hID), y = as.numeric(vID)),]
boxes[hID == 0, x := median(range(model_stucture_dt$vID,max(model_stucture_dt$vID)+1)),]
boxes[,":="(width = 0.6, height = 0.4),]
boxes[agent_type == "sapro",":="(x = x+0.5),]

arrows <- boxes
arrows <- merge(arrows, model_stucture_dt[,.(boxID, rate_type, value)], by= "boxID")

arrows[rate_type == "respiration", ":="(
  x_start = x + width/2, 
  y_start = y,
  x_end = (x + width/2)+0.2,
  y_end = y + 0.2,
  curvature = -0.3
  ) ,]
arrows[rate_type == "decomp_frac", ":="(
  x_start = x - 0.5, 
  y_start = y - 1 + height/2,
  x_end = x + 0.5,
  y_end = y - 1 + height/2,
  curvature = -0.5
  ) ,]
arrows[rate_type == "feeding" & hID != 0, ":="(
  x_start = x - 0.5, 
  y_start = y - 1 + height/2,
  x_end = x,
  y_end = y - height/2,
  curvature = 0.5
  ) ,]

# define arrows for generalists (hID)

generalists <- arrows[rate_type == "feeding" & hID == 0]
arrows_generalists <- data.table()
v=3
for(v in generalists$vID){
  v_prey = v - 1
  temp_dt <- boxes[vID == v_prey]
  temp_dt[,":="(
    x_start = x, 
    y_start = y + height/2,
    x_end = generalists[vID == v]$x,
    y_end = generalists[vID == v]$y - generalists[vID == v]$height/2
  ),]
  arrows_generalists <- rbind(arrows_generalists, temp_dt)
}


ggplot(boxes, aes(x = x, y = y))+
  geom_tile(aes(width = width, height = height,fill = agent_type), linewidth = 3)+
  geom_label(aes(label = agent_type))+
  geom_curve(aes(
    x = x_start, y = y_start, xend = x_end, yend = y_end, color = rate_type),
    data = arrows[rate_type %in% c("respiration")], 
    curvature = 0.3, size = 1,lineend = "round",
    arrow = arrow(length = unit(0.1, "inches")))+
  geom_curve(aes(
    x = x_start, y = y_start, xend = x_end, yend = y_end, color = rate_type),
    data = arrows[rate_type %in% c("decomp_frac")], 
    curvature = -0.5, size = 1,lineend = "round",
    arrow = arrow(length = unit(0.1, "inches")))+
  geom_curve(aes(
    x = x_start, y = y_start, xend = x_end, yend = y_end, color = rate_type),
    data = arrows[rate_type %in% c("feeding")], 
    curvature = 0.2, size = 1,lineend = "round",
    arrow = arrow(length = unit(0.1, "inches")))+
  geom_curve(aes(
    x = x_start, y = y_start, xend = x_end, yend = y_end, color = "feeding"),
    data = arrows_generalists, 
    curvature = 0, size = 1,lineend = "round",
    arrow = arrow(length = unit(0.1, "inches")))+
  theme_void()

bitheme_minimal()biomass$wood
biomass$sapro
biomass$predator

t_max = 300
out_df = data.frame()
t=1
for(t in 0:t_max){
  
  if(t > 0){
    
    # simulate wood and sapro interactions
    
    sapro_feed <- c()
    sapro_resp <- c()
    wood_decomp <- c()
    for(v in 1:length(biomass$wood)){
      # calculate flows caused by sapro
      sapro_feed[v] = biomass$wood[v]*parameters$sapro$feeding[v]
      sapro_resp[v] = biomass$wood[v]*parameters$sapro$respiration[v]
      wood_decomp[v] = sapro_feed*parameters$sapro$decomp_frac[v]
    }
    
    # sapro1 feeding on alive
    
    parameters$sapro$feeding
    
    flux_alive_to_sapro1 = biomass$alive*parameters$sapro1$feeding_rate
    resp_sapro1 = biomass$sapro1*parameters$sapro1$respiration_rate
    biomass$sapro1 = biomass$sapro1 + flux_alive_to_sapro1 - resp_sapro1
    
    # sapro2 feeding on dead1
    flux_dead1_to_sapro2 = biomass$dead1*parameters$sapro2$feeding_rate
    biomass$dead1 = biomass$dead1 - flux_dead1_to_sapro2
    resp_sapro2 = biomass$sapro2*parameters$sapro2$respiration_rate
    biomass$sapro2 = biomass$sapro2 + flux_dead1_to_sapro2 - resp_sapro2
    
    # sapro3 feeding on dead2
    flux_dead2_to_sapro3 = biomass$dead2*parameters$sapro3$feeding_rate
    biomass$dead2 = biomass$dead2 - flux_dead2_to_sapro3
    resp_sapro3 = biomass$sapro3*parameters$sapro3$respiration_rate
    biomass$sapro3 = biomass$sapro3 + flux_dead2_to_sapro3 - resp_sapro3
    
    # growth of alive wood and decay of dead wood
    biomass$dead2 = biomass$dead2 + flux_dead1_to_sapro2*flux_dead1_to_sapro2
  }
  
  out_df_temp <- rbindlist(
    list(
      data.table(pool = "wood", poolsubID = 1:length(biomass$wood), biomass = biomass$wood),
      data.table(pool = "sapro", poolsubID = 1:length(biomass$sapro), biomass = biomass$sapro),
      data.table(pool = "predator", poolsubID = 1:length(biomass$predator), biomass = biomass$predator),
      data.table(pool = "atmospheric_carbon", poolsubID = 1, biomass = atmospheric_carbon),
      data.table(pool = "soc", poolsubID = 1, biomass = 0)
      )
    )
  out_df_temp$t = t
  out_df <- rbind(out_df, out_df_temp)
}

par(mfrow = c(2,1))
plot(biomass_dead1~time,out_df, type = "l", ylim = c(0,60))
lines(biomass_dead2~time,out_df, type = "l", col = "red")
plot(biomass_sapro1~time,out_df, type = "l", col = "blue")
plot(biomass_dead1~time,out_df, type = "l", ylim = c(0,60))
lines(biomass_dead2~time,out_df, type = "l", col = "red")

