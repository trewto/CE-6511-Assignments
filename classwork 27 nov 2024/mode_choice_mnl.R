rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_RP",
  modelDescr      = "Simple MNL model on mode choice RP data",
  indivID         = "ID", 
  outputDirectory = "output_RP"
)


database = read.csv("apollo_modeChoiceData.csv", header = TRUE)

### Use only RP data
database = subset(database,database$RP==1)



### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car   = 0,
              asc_bus   = 0,
              asc_air   = 0,
              asc_rail  = 0,
              b_tt_car  = 0,
              b_tt_bus  = 0,
              b_tt_air  = 0,
              b_tt_rail = 0,
              b_access  = 0,
              b_cost_car  = 0,
              b_cost_bus = 0 ,
              b_cost_air = 0 ,
              b_cost_rail = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if 
apollo_fixed = c("asc_car")
#apollo_fixed = c()


apollo_inputs = apollo_validateInputs()



apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt_car  * time_car                           + b_cost_car * cost_car
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost_bus * cost_bus 
  V[["air"]]  = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost_air * cost_air   
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost_rail * cost_rail  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)




# ################################################################# #
#### VTT AND VAT CALCULATIONS                                    ####
# ################################################################# #

# Extract estimates from the model
estimates = model$estimate

# Calculate VTT and VAT
vtt_car  = -estimates["b_tt_car"] / estimates["b_cost_car"] * 60
vtt_bus  = -estimates["b_tt_bus"] / estimates["b_cost_bus"] * 60
vtt_air  = -estimates["b_tt_air"] / estimates["b_cost_air"] * 60
vtt_rail = -estimates["b_tt_rail"] / estimates["b_cost_rail"] * 60

vat_car  = -estimates["b_access"] / estimates["b_cost_car"] * 60
vat_bus  = -estimates["b_access"] / estimates["b_cost_bus"] * 60
vat_air  = -estimates["b_access"] / estimates["b_cost_air"] * 60
vat_rail = -estimates["b_access"] / estimates["b_cost_rail"] * 60

# Create a table for VTT and VAT
vtt_vat_table = data.frame(
  Mode = c("Car", "Bus", "Air", "Rail"),
  VTT_EuroPerHour = c(vtt_car, vtt_bus, vtt_air, vtt_rail),
  VAT_EuroPerHour = c(vat_car, vat_bus, vat_air, vat_rail)
)

# Print the table
print("Value of Travel Time (VTT) and Value of Access Time (VAT):")
print(vtt_vat_table)

# Save the table to a CSV file
write.csv(vtt_vat_table, file = paste0(apollo_control$outputDirectory, "/vtt_vat_results.csv"), row.names = FALSE)
