# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py,md
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.10.2
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# Paper: "The Role of Patients’ Age on Their Preferences for Choosing Additional Blood Pressure-Lowering Drugs: A Discrete Choice Experiment in Patients with Diabetes"
#
# Author(s): Sieta T. de Vries, Folgerdiena M. de Vries, Thijs Dekker, Flora M. Haaijer-Ruskamp, Dick de Zeeuw, Adelita V. Ranchor, and Petra Denig
#
# Year: 2015
#
# Model(s): Multinomial Logit
#
# Main findings: \_\_\_\_

# # Import needed libraries

# + jupyter={"outputs_hidden": false}
import sys
from collections import OrderedDict

import scipy.stats

import numpy as np
import pandas as pd

import seaborn as sbn
import matplotlib.pyplot as plt

from statsmodels.formula.api import logit

sys.path.insert(0, '/Users/timothyb0912/Documents/pylogit/')
import pylogit as pl

sys.path.insert(0, '../../src/')
from visualization import predictive_viz as viz

# %matplotlib inline
# -

# # Import data

# + jupyter={"outputs_hidden": false}
data_path =\
    '../../data/raw/deVries_2015/deVries_2015_data.dta'
df = pd.read_stata(data_path)
# -

# # Look at data

# + jupyter={"outputs_hidden": false}
df.shape

# + jupyter={"outputs_hidden": false}
df.head().T

# + jupyter={"outputs_hidden": false}
set(df.columns.tolist())
# -

# # Clean data

# + jupyter={"outputs_hidden": false}
# Create an integer choice column
df['choice'] = df.choice.map({'Not chosen': 0, 'Chosen': 1})

# Create an integer alternative id column
alt_id_map = {'No additional drug': 1,
              'Additional drug A': 2,
              'Additional drug B': 3}
alt_id_reverse_map = {alt_id_map[k]: k for k in alt_id_map}
df['alt_id'] = df.choice_possibility.map(alt_id_map)

# Make age_75, i.e. age >= 75 an integer column
df['age_75'] =\
    df['age_75'].map({'<75 years': 0, '>= 75 years': 1})

# Create an additional_drug column
df['additional_drug'] =\
    df.noconstant23.map({'No additional drug': 0,
                         'Additional drug': 1})

# Create a column for one_in_morning_one_in_evening
df['one_in_morning_one_in_evening'] =\
    df.dintake1.map({'not 1 morning, 1 evening': 0,
                     '1 morning, 1 evening': 1})
    
# Create a column for combination_tablet
df['combination_tablet'] =\
    df.dintake2.map({'not combi in the morning': 0,
                     'Combi in the morning': 1})

# Filter out the individuals choosing the non-dominant alternative
non_dominant_filter = df.dominant_correct == 'Correct'

# + jupyter={"outputs_hidden": false}
clean_df =\
    df.loc[non_dominant_filter].copy().sort_values('seq')

# Look at the shape of clean_df
print('clean_df.shape == {}'.format(clean_df.shape))

# Look at some records in clean_df
clean_df.head(6).T

# -

# # Compute needed transformations / derived features

# + jupyter={"outputs_hidden": false}
# Create needed interactions with age_75
clean_df['additional_drug_age75'] =\
    clean_df.additional_drug * clean_df.age_75.values

clean_df['one_in_morning_one_in_evening_age75'] =\
    (clean_df.one_in_morning_one_in_evening *
     clean_df.age_75.values)

clean_df['combination_tablet_age75'] =\
    clean_df.combination_tablet * clean_df.age_75.values
# -

# # Define the model specification and parameter names

# + jupyter={"outputs_hidden": false}
base_explanatory_vars =\
    ['additional_drug',
     'bloodpressure',
     'death',
     'heartattack',
     'stroke',
     'ADRs',
     'one_in_morning_one_in_evening',
     'combination_tablet',
    ]

base_display_names = ['Constant (additional drug)',
                      'Blood pressure',
                      'Death within the next 5 years',
                      'Limitations heart attack',
                      'Limitations stroke',
                      'Adverse drug events',
                      'Additional tablet in the morning',
                      'Combination tablet']

interaction_explanatory_vars =\
    (base_explanatory_vars + 
     [col + '_age75' for col in base_explanatory_vars])
    
interaction_display_names =\
    (base_display_names + 
     [col + ' (age >= 75)' for col in base_display_names])

missing_vars =\
    [x for x in interaction_explanatory_vars
     if x not in clean_df.columns]
if len(missing_vars) > 0:
    msg = 'These explanatory variables are not in the data file:'
    raise ValueError(msg + '\n{}'.format(missing_vars))


# Populate the specification and name dictionaries
class SpecInfo(object):
    def __init__(self,
                 variable_list,
                 display_names):
        self.variable_list = variable_list
        self.name_list = display_names
        self.spec_dict = OrderedDict()
        self.name_dict = OrderedDict()
        
        self.populate_spec_and_name_dicts()
        return None
        
    def populate_spec_and_name_dicts(self):
        num_vars = len(self.variable_list)
        for pos in range(num_vars):
            current_column = self.variable_list[pos]
            self.spec_dict[current_column] = 'all_same'
            self.name_dict[current_column] = self.name_list[pos]
        return None

base_model_info = SpecInfo(base_explanatory_vars,
                           base_display_names)

interaction_info = SpecInfo(interaction_explanatory_vars,
                            interaction_display_names)
# -

# # Set model parameters

# + jupyter={"outputs_hidden": true}
choice_col = 'choice'
alt_id_col = 'alt_id'
obs_id_col = 'seq'
# -

# # Create the model object(s)

# + jupyter={"outputs_hidden": false}
model_obj =\
    pl.create_choice_model(
        data=clean_df,
        alt_id_col=alt_id_col,
        obs_id_col=obs_id_col,
        choice_col=choice_col,
        specification=base_model_info.spec_dict,
        model_type='MNL',
        names=base_model_info.name_dict)

interaction_obj =\
    pl.create_choice_model(
        data=clean_df,
        alt_id_col=alt_id_col,
        obs_id_col=obs_id_col,
        choice_col=choice_col,
        specification=interaction_info.spec_dict,
        model_type='MNL',
        names=interaction_info.name_dict)
# -

# # Estimate and view the model

# + jupyter={"outputs_hidden": false}
model_obj.fit_mle(np.zeros(len(base_display_names)))
model_obj.get_statsmodels_summary()

# + jupyter={"outputs_hidden": false}
interaction_obj.fit_mle(np.zeros(len(interaction_display_names)))
interaction_obj.get_statsmodels_summary()
# -

# # Set checking parameters

# + jupyter={"outputs_hidden": true}
NUM_SAMPLES = 200
RANDOM_SEED = 100

np.random.seed(RANDOM_SEED)


# -

# # Sample from the posterior or approximate sampling distribution of model parameters

# + jupyter={"outputs_hidden": true}
class CheckingObject(object):
    def __init__(self, pylogit_obj, num_samples, seed=None):
        # Set object attributes
        self.model = pylogit_obj
        self.hessian = pylogit_obj.hessian
        self.asym_cov = pylogit_obj.cov
        self.asym_dist =\
            scipy.stats.multivariate_normal(
                mean=pylogit_obj.params, cov=self.asym_cov)
        self.posterior_probs = None
        self.sim_y = None
        
        # Set the random seed, if desired
        if seed is not None:
            np.random.seed(seed)

        # Get and set the posterior parameter samples
        self.param_samples = self.asym_dist.rvs(num_samples)
        # Compute and set the posterior probabilities
        self.compute_posterior_probs()
        # Compute and set the simulated choices
        self.simulate_choices()
        return None
        
    def compute_posterior_probs(self):
        self.posterior_probs =\
            self.model.predict(self.model.data,
                               param_list=[self.param_samples.T,
                                           None, None, None])
        return None

    def simulate_choices(self):
        self.sim_y =\
            viz.simulate_choice_vector(
                self.posterior_probs,
                self.model.data[self.model.obs_id_col].values)
        return None


# + jupyter={"outputs_hidden": false}
model_checker =\
    CheckingObject(model_obj, NUM_SAMPLES, seed=RANDOM_SEED)

interaction_checker =\
    CheckingObject(interaction_obj, NUM_SAMPLES, seed=RANDOM_SEED)
# -

# # Generate posterior predictive datasets

# # Save all model generated data

# + jupyter={"outputs_hidden": true}
# Save posterior parameter samples

# Save posterior predictive datasets
# -

# # Determine the measurement scales of the explanatory variables

# + jupyter={"outputs_hidden": false}
print('Number of unique values per column:')
unique_values_per_variable =\
    clean_df[interaction_explanatory_vars].agg(
        lambda x: x.unique().size, axis='index')
    
print(unique_values_per_variable)

# + jupyter={"outputs_hidden": true}
# Determine which variables are continuous and which are not
continuous_variables =\
    (unique_values_per_variable[unique_values_per_variable > 4]
                               .index.tolist())

categorical_variables =\
    (unique_values_per_variable[unique_values_per_variable <= 4]
                               .index.tolist())
# -

# # <font color=darkred> Should place all checking related cells in a second notebook.</font>
#
# # Perform the desired posterior predictive checks of the interaction model

# ### 1. Predictive Performance plots

# + jupyter={"outputs_hidden": false}
# Generate the simulated log-likelihoods
sim_log_likes =\
    viz.compute_prior_predictive_log_likelihoods(
        interaction_checker.sim_y,
        clean_df,
        choice_col,
        interaction_checker.model)

# Plot the simulated versus observed log-likelihood
log_like_path = None
viz.plot_predicted_log_likelihoods(sim_log_likes,
                                   interaction_checker.model.llf,
                                   output_file=log_like_path)

# + jupyter={"outputs_hidden": false}
# Plot the simulated versus observed log-likelihood for each
# alternative
log_like_path = None
for alt_id in np.sort(clean_df[alt_id_col].unique()):
    alt_idx = clean_df[alt_id_col] == alt_id

    current_sim_y = interaction_checker.sim_y[alt_idx, :]
    current_obs_y = interaction_checker.model.choices[alt_idx]

    current_probs =\
        interaction_checker.model.long_fitted_probs[alt_idx]

    current_sim_log_likes =\
        current_sim_y.T.dot(np.log(current_probs))

    current_log_likelihood =\
        current_obs_y.dot(np.log(current_probs))

    current_alt_label = alt_id_reverse_map[alt_id]

    current_x_label =\
        'Log-Likelihood for {}'.format(current_alt_label)

    viz.plot_predicted_log_likelihoods(current_sim_log_likes,
                                       current_log_likelihood,
                                       x_label=current_x_label,
                                       output_file=log_like_path)
# -

# ### 2. Outcome Boxplot

# + jupyter={"outputs_hidden": false}
market_path = None
num_obs = interaction_checker.model.nobs


viz.plot_simulated_market_shares(
    clean_df[alt_id_col].values,
    interaction_checker.sim_y,
    interaction_checker.model.choices,
    x_label='Alternative ID',
    y_label='Number\nof times\nchosen',
    output_file=market_path)
# -

# ### 3. Binned Reliability Plot

# + jupyter={"outputs_hidden": false}
reload(viz)
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    
    current_filter = interaction_checker.model.alt_IDs == alt
    current_probs =\
        interaction_checker.model.long_fitted_probs[current_filter]
    current_choices =\
        interaction_checker.model.choices[current_filter]
    current_sim_y = interaction_checker.sim_y[current_filter, :]
    
    current_alt = alt_id_reverse_map[alt]
    current_line_label =\
        'Observed vs Predicted ({})'.format(current_alt)
    current_sim_label =\
        'Simulated vs Predicted ({})'.format(current_alt)

    current_sim_color = '#a6bddb'
    current_obs_color = '#045a8d'

    viz.plot_binned_reliability(
        current_probs,
        current_choices,
        sim_y=current_sim_y,
        line_label=current_line_label,
        line_color=current_obs_color,
        sim_label=current_sim_label,
        sim_line_color=current_sim_color,
        figsize=(10, 6),
        ref_line=True,
        output_file=None)
# -

# ### 4. 'Bagged' Reliability Plot

# + jupyter={"outputs_hidden": false}
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    current_probs =\
        interaction_checker.model.long_fitted_probs[current_filter]
    current_choices =\
        interaction_checker.model.choices[current_filter]
    current_sim_y = interaction_checker.sim_y[current_filter, :]
    
    current_alt = alt_id_reverse_map[alt]
    current_line_label =\
        'Observed vs Predicted ({})'.format(current_alt)
    current_sim_label =\
        'Simulated vs Predicted ({})'.format(current_alt)

    filename = None

    fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(10, 6))
    fig_and_ax = [fig, ax]
    viz.make_bagged_marginal_model_plot(
        current_sim_y,
        current_choices,
        current_probs,
        y_label=current_line_label,
        prob_label=current_sim_label,
        x_label='Predicted P(Y={})'.format(current_alt),
        alpha=0.5,
        fig_and_ax=fig_and_ax,
        output_file=filename)

    # Determine the maximum value of the x-axis or y-axis
    max_ref_val = max(ax.get_xlim()[1], ax.get_ylim()[1])
    min_ref_val = max(ax.get_xlim()[0], ax.get_ylim()[0])
    # Determine the values to use to plot the reference line
    ref_vals = np.linspace(min_ref_val, max_ref_val, num=100)
    # Plot the reference line as a black dashed line
    ax.plot(ref_vals, ref_vals, 'k--', label='Perfect Calibration')
    ax.legend(loc='best', fontsize=12)
    # Show the plot
    fig.show();
# -

# ### 5. Binned marginal model plots

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    current_probs =\
        interaction_checker.model.long_fitted_probs[current_filter]
    current_choices =\
        interaction_checker.model.choices[current_filter]
    current_sim_y = interaction_checker.sim_y[current_filter, :]
    
    current_alt = alt_id_reverse_map[alt]
    current_line_label =\
        'Observed P(Y={})'.format(current_alt)
    current_sim_label =\
        'Simulated P(Y={})'.format(current_alt)
    current_predicted_label =\
        'Predicted P(Y={})'.format(current_alt)
    for col in continuous_variables:
        current_x = clean_df.loc[current_filter, col].values
    
        viz.make_binned_marginal_model_plot(
            current_probs,
            current_choices,
            current_x,
            partitions=10,
            sim_y=current_sim_y,
            y_label=current_line_label,
            prob_label=current_predicted_label,
            sim_label=current_sim_label,
            x_label=col,
            alpha=0.5,
            figsize=(10, 6),
            output_file=filename)
# -

# ### 6. Bagged marginal model plots

# #### 6a. Check the relationships with the raw explanatory variables

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    current_probs =\
        interaction_checker.model.long_fitted_probs[current_filter]
    current_choices =\
        interaction_checker.model.choices[current_filter]
    current_sim_y = interaction_checker.sim_y[current_filter, :]
    
    current_alt = alt_id_reverse_map[alt]
    current_line_label =\
        'Observed P(Y={})'.format(current_alt)
    current_sim_label =\
        'Simulated P(Y={})'.format(current_alt)
    current_predicted_label =\
        'Predicted P(Y={})'.format(current_alt)
    for col in continuous_variables:
        current_x = clean_df.loc[current_filter, col].values

        viz.make_bagged_marginal_model_plot(
            current_probs,
            current_choices,
            current_x,
            sim_y=current_sim_y,
            y_label=current_line_label,
            prob_label=current_predicted_label,
            sim_label=current_sim_label,
            x_label=col,
            alpha=0.5,
            figsize=(10, 6),
            output_file=filename)
# -

# #### 6b. Check the relationship with the estimated index, $V = X \beta$

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    
    current_filter = interaction_checker.model.alt_IDs == alt
    current_probs =\
        interaction_checker.posterior_probs[current_filter]
    current_choices =\
        interaction_checker.model.choices[current_filter]
    current_sim_y = interaction_checker.sim_y[current_filter, :]
    
    current_alt = alt_id_reverse_map[alt]
    current_x =\
        interaction_checker.model.design.dot(
         interaction_checker.model.params)[current_filter]
        
    current_y_label = 'Observed P(Y={})'.format(current_alt)
    current_prob_label = 'Predicted P(Y={})'.format(current_alt)
    current_sim_label = 'Simulated P(Y={})'.format(current_alt)

    viz.make_bagged_marginal_model_plot(
        current_probs,
        current_choices,
        current_x,
        sim_y=current_sim_y,
        y_label=current_y_label,
        prob_label=current_prob_label,
        sim_label=current_sim_label,
        x_label=r'$V = X \beta$',
        alpha=0.5,
        figsize=(10, 6),
        fontsize=13,
        output_file=filename)
# -

# ### 7. Simulated KDEs

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    current_title = ''

    for col in continuous_variables:
        viz.plot_simulated_kde_traces(
            interaction_checker.sim_y,
            clean_df,
            current_filter,
            col,
            choice_col,
            label='Simulated {}'.format(col),
            title=current_title,
            figsize=(10, 6),
            output_file=filename)
# -

# ### 8. Simulated CDFs

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    current_title = ''

    for current_col in continuous_variables:
        viz.plot_simulated_cdf_traces(
            iteraction_checker.sim_y,
            clean_df,
            current_filter,
            current_col,
            choice_col,
            label='Simulated ({})'.format(col),
            title=current_title,
            figsize=(10, 6),
            output_file=filename)
# -

# ### 9. Simulated Histograms

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    alt_name = alt_id_reverse_map[alt]
    current_filter_name = '{} choices'.format(alt_name)
    current_title = ''

    for current_col in categorical_variables:
        viz.plot_categorical_predictive_densities(
            clean_df,
            None,
            interaction_checker.sim_y,
            current_col,
            current_filter,
            interaction_checker.model.choices.astype(int),
            title=current_title,
            filter_name=current_filter_name,
            post_color=sbn.color_palette('colorblind')[0],
            figsize=(10, 6),
            legend_loc='best',
            output_file=filename)
# -

# ###  10. Simulated Lagrange Multiplier tests for mixing

# ### Generate artificial variables for Lagrange Multiplier Checks of the interaction model
# Note that this is based on the Lagrange Multiplier tests described in McFadden, D., Train, K., 2000. Mixed MNL Models for Discrete Response. Journal of Applied Econometrics 15, 447–470.

# + jupyter={"outputs_hidden": true}
# Get the rows_to_obs matrix
rows_to_obs = interaction_obj.get_mappings_for_fit()['rows_to_obs']

# Get the mean attribute values for each observation
probs_to_obs = rows_to_obs.multiply(interaction_obj.long_fitted_probs[:, None])

# Will have shape (num_obs, design[1])
x_mean_per_obs = probs_to_obs.T.dot(interaction_obj.design)

# Will have same shape as the design matrix
long_x_mean_per_obs = rows_to_obs.dot(x_mean_per_obs)

# X - X_mean_per_obs
augmented_x = interaction_obj.design - long_x_mean_per_obs

# z = 0.5 * (x - x_bar)^2
artificial_x = 0.5 * augmented_x**2
# -

# ### Make the desired plots

# + jupyter={"outputs_hidden": false}
filename = None
for alt in np.sort(np.unique(interaction_checker.model.alt_IDs)):
    current_filter = interaction_checker.model.alt_IDs == alt
    alt_name = alt_id_reverse_map[alt]
    current_filter_name = '{} choices'.format(alt_name)
    current_title = '' 

    current_probs =\
        interaction_checker.posterior_probs[current_filter]
    current_choices =\
        interaction_checker.model.choices[current_filter]
    current_sim_y = interaction_checker.sim_y[current_filter, :]
    
    current_y_label = 'Observed P(Y={})'.format(alt_name)
    current_prob_label = 'Predicted P(Y={})'.format(alt_name)
    current_sim_label = 'Simulated P(Y={})'.format(alt_name)
    
    for col in range(interaction_checker.model.design.shape[1]):
        column_name = (interaction_checker.model
                                          .params
                                          .index
                                          .tolist()[col])
        current_x = artificial_x[current_filter, col]
        current_x_label =\
            'Artificial {} {}'.format(alt_name, column_name)


        viz.make_bagged_marginal_model_plot(
            current_probs,
            current_choices,
            current_x,
            sim_y=current_sim_y,
            y_label=current_y_label,
            prob_label=current_prob_label,
            sim_label=current_sim_label,
            x_label=current_x_label,
            alpha=0.5,
            figsize=(10, 6))
# -

# # Findings and Recommendations based on the posterior predictive checks

# 1. The posterior predictive checks indicate great model mis-specification for "Additional drug A" and "Additional drug B". This misfit is reiterated in the various predictive performance plots, the market share plots, the reliability plots, as well as the simulated histograms for these alternatives. Basically, "Additional drug A" is systematically over-predicted and "Additional drug B" is systematically under-predicted.
# 2. An alternative specific constant is likely needed for "Additional drug A" and for "Additional drug B".
# 3. Based on the Lagrange Multiplier checks, a mixed logit specification may perhaps be necessary for some variables in the utility specifications for "Additional drug A" and "Additional drug B" alternatives. However, I think this is just an artifact of having all the predicted probabilities for those two alternatives be systematically over/under-estimated.

# + jupyter={"outputs_hidden": true}

