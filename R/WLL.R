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
parameters_dt <- fread("data/parameters/rates.csv")


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## initialize model ####
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

initial_wood = c(100, 0, 0, 0)

horizontal_levels = sort(unique(c(parameters_dt$source_h_lev, parameters_dt$target_h_lev)))
Nhorizontal = uniqueN(horizontal_levels)
vertical_levels = sort(unique(c(parameters_dt$source_v_lev, parameters_dt$target_v_lev)))
Nvertical = uniqueN(vertical_levels)

biomass <- list(
  wood = initial_wood,
  sapro = rep(0, length(initial_wood)-1),
  predator = rep(0,Nvertical-1)
)

atmospheric_carbon = 0

#### set parameters ####
parameters <- list()
for(rate_type_i in c("feeding", "decomp_frac", "respiration")){
  parameters[["sapro"]][[rate_type_i]] <- as.matrix(parameters_dt[agent_type == "sapro" & rate_type == rate_type_i, .(h_level = source_h_lev, v_level = source_v_lev,value)])
}

parameters[["predators"]][["feeding"]] <- as.matrix(parameters_dt[agent_type == "predator" & rate_type == "feeding", .(h_level = source_h_lev, v_level = target_v_lev, value)])
parameters[["predators"]][["respiration"]] <- as.matrix(parameters_dt[agent_type == "predator" & rate_type == "respiration", .(h_level = source_h_lev, v_level = source_v_lev, value)])

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## plot model ####
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

library(ggplot2)

model_stucture_dt <- data.table(
  expand.grid(
    list(
      x_pos = as.numeric(1:Nhorizontal),
      # x_names = horizontal_levels,
      y_pos = as.numeric(1:Nvertical)
      # y_names = vertical_levels
      )
    ))
model_stucture_dt[,":="(
  width = 0.6,
  height = 0.4),]
model_stucture_dt[x_pos == 1 & y_pos == 1, ":="(label = "alive", type = "wood"),]
model_stucture_dt[x_pos > 1 & x_pos < Nhorizontal & y_pos == 1, ":="(label = "dead", type = "wood"),]
model_stucture_dt[x_pos == Nhorizontal-1 & y_pos == 1, ":="(label = "soil", type = "soil"),]
model_stucture_dt[y_pos > 1 & y_pos < min(parameters$predators$feeding[,2]), ":="(label = "sapro", type = "saprotrophs"),]
model_stucture_dt[y_pos < Nvertical & x_pos < Nhorizontal & y_pos >= min(parameters$predators$feeding[,2]), ":="(
  label = "predators", type = "predators", x_pos = mean(1:Nvertical), width = Nvertical-1+0.6
  ),]
model_stucture_dt[y_pos == Nvertical, ":="(label = "atmosphere", type = "atmosphere", x_pos = mean(1:Nvertical)+0.5, width = Nvertical+0.6),]
model_stucture_dt[x_pos == Nhorizontal, ":="(label = "atmosphere", type = "atmosphere", y_pos = mean(1:Nhorizontal)-0.5, height = Nvertical-1+0.4),]

model_stucture_dt <- model_stucture_dt[!(type == "saprotrophs" & x_pos == Nhorizontal-1)]
model_stucture_dt[type == "saprotrophs", x_pos := x_pos+0.5]

model_stucture_dt <- unique(model_stucture_dt)
ggplot(model_stucture_dt, aes(x = x_pos, y = y_pos))+
  geom_tile(aes(fill = type, width = width, height = height), linewidth = 3)+
   geom_label(aes(label = label))
  

directions <- igraph::graph_from_data_frame(model_stucture_dt, directed = T)
igraph::layout_as_tree(directions)
t_max = 300
out_df = data.frame()
t=1
for(t in 0:t_max){
  
  if(t > 0){
    
    # sapro1 feeding on alive
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
  
  out_df <- rbind(
    out_df,
      data.frame(
      )
    )
}

par(mfrow = c(2,1))
plot(biomass_dead1~time,out_df, type = "l", ylim = c(0,60))
lines(biomass_dead2~time,out_df, type = "l", col = "red")
plot(biomass_sapro1~time,out_df, type = "l", col = "blue")
plot(biomass_dead1~time,out_df, type = "l", ylim = c(0,60))
lines(biomass_dead2~time,out_df, type = "l", col = "red")

