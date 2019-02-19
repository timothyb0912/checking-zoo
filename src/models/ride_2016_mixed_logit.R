# Load the necessary packages
library("mlogit")
library("MASS")

# Load the needed data
df = as.data.frame(read.csv("../../data/processed/ride_2016/ride_2016_final_data.csv"))
df['neg_cost'] = -1 * df['cost']

# Create an mlogit data object
ride_data = mlogit.data(df,
                        shape="long",
                        choice="choice",
                        id.var="id",
                        chid.var="obs_id",
                        alt.var="alt")

# Write the formula for the mixed logit model 
model_variables = c('ASC',
                    'couns',
                    'combo',
                    'peer',
                    'group',
                    'epc',
                    'herb',
                    'MYE',
                    'neg_cost',
                    'effect',
                    'home',
                    'phone',
                    'online',
                    'child',
                    'Age',
                    'Employed',
                    'Unemployed',
                    'past_experience_any_type',
                    'low_support',
                    'seek_help',
                    'income_times_cost',
                    'insurance_times_cost',
                    'prior_experience_of_treatment_type',
                    'couns_highschool',
                    'combo_highschool',
                    'peer_highschool',
                    'group_highschool',
                    'epc_highschool',
                    'herb_highschool',
                    'MYE_highschool',
                    'couns_breastfeeding',
                    'combo_breastfeeding',
                    'peer_breastfeeding',
                    'group_breastfeeding',
                    'epc_breastfeeding',
                    'herb_breastfeeding',
                    'MYE_breastfeeding',
                    'couns_pregnant',
                    'combo_pregnant',
                    'peer_pregnant',
                    'group_pregnant',
                    'epc_pregnant',
                    'herb_pregnant',
                    'MYE_pregnant')

mixl_formula = as.formula(paste("choice ~ 0 +",
                                paste(model_variables,
                                      collapse=" + ")))

# Estimate an MNL model to get starting values for the mixed logit
mnl_model = mlogit(mixl_formula,
                   data=ride_data,
                   method="bhhh")

# Try using starting values from the paper itself
mixl_start = c(3.337,
               1.198,
               1.749,
               0.855,
               0.743,
               1.198,
               1.094,
               1.601,
               0.0562,
               0.797,
               0.131,
               -0.216,
               0.0467,
               0.224,
               0.0548,
               -0.257,
               -2.515,
               1.152,
               -0.869,
               1.113,
               -1.21e-5,
               4.08e-3,
               0.606,
               -0.251,
               -0.402,
               -0.256,
               -0.337,
               -0.906,
               -0.454,
               -0.232,
               -1.32e-3,
               -0.8,
               -0.0727,
               -0.194,
               -0.347,
               -0.427,
               -7.32e-3,
               -0.0593,
               -0.981,
               -0.477,
               -0.324,
               -0.268,
               -0.343,
               -0.463)

# Also make use of the standard deviation values
# from the paper
rpar_start = c(2.883,
               -0.334,
               0.901,
               -0.542,
               0.502,
               -0.941,
               1.205,
               0.845,
               0.125,
               2.187,
               -0.0253,
               -0.250,
               -0.153,
               0.451)

# Combine all estimated values from the paper
full_mixl_start = c(mixl_start, rpar_start)

# Set the random seed for estimation
set.seed(601)

# Estimate the mixed logit model
mixl_model = mlogit(mixl_formula,
                    data=ride_data,
                    id="id",
                    id.var="id",
                    chid.var="obs_id",
                    alt.var="alt",
                    choice="choice",
                    shape="long",
                    method="bhhh",
                    rpar=c(ASC="n",
                           couns="n",
                           combo="n",
                           peer="n",
                           group="n",
                           epc="n",
                           herb="n",
                           MYE="n",
                           neg_cost="ln",
                           effect="ln",
                           home="n",
                           phone="n",
                           online="n",
                           child="n"),
                    correlation=FALSE,
                    halton=NULL,
                    panel=TRUE,
                    print.level=1,
                    R=800)
                    # R=400,
                    # start=mnl_model$coefficients)
                    # R=400,
                    # start=full_mixl_start)

# Look at the model results
summary(mixl_model)

# Store the model coefficients
# write.csv(mixl_model$coefficients, "../data/mixl_coefs.csv", row.names=TRUE)
# 
# # Get the forecast predictions and save them to a csv
# forecast_probs = as.vector(predict(mixl_model, forecast_data, returnData = FALSE))
# write.csv(forecast_probs, "../data/mixl_forecast_probs.csv", row.names=FALSE)
# 
# # Get the asymptotic covariance matrix from the hessian
# mixl_cov = -1 * solve(mixl_model$hessian)
# 
# # Determine the number of simulations
# num_simulations = 1000
# 
# # Sample from the asymptotic covariance matrix
# beta_samples = mvrnorm(n=num_simulations, mixl_model$coefficients, mixl_cov)
# 
# # Save various items
# orig_coefs = mixl_model$coefficients

# Get and save the original long format probabilities
# orig_long_probs = as.vector(mixl_model$probabilities)
# write.csv(x=orig_long_probs,
#           file="../data/mixl_long_format_probs.csv",
#           row.names=FALSE)
# 
# # Initialize an array for the simulated probabilities
# sim_probs = matrix(data = NA,
#                    nrow=length(orig_long_probs),
#                    ncol=num_simulations)
# 
# # Create a progress bar
# progress = txtProgressBar(min=0, max=num_simulations, style=3)
# 
# # Fill in the matrix of simulated probabilities
# for (i in 1:num_simulations){
#   # Extract the current samples from the matrix of sampled model coefficients
#   current_beta = beta_samples[i, ]
#   
#   # Assign the current value of beta to the model object
#   mixl_model$coefficients <- current_beta
#   
#   # Make predictions with the new coefficients
#   new_predictions = as.vector(predict(mixl_model, car_data, returnData = FALSE))
#   
#   # Store the new predictions
#   sim_probs[, i] = new_predictions
#   
#   # Update the progress bar
#   setTxtProgressBar(progress, i)
# }
# 
# # Store the array of simulated probabilities
# write.csv(sim_probs, file="../data/mixl_sim_probs.csv", row.names=FALSE)
