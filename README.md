---
title: "README"
output: html_document
---

## Code to run simulations for the paper titled 'Is more data always better? A simulation study of benefits and limitations of integrated distribution models'

This repository hosts functions to simulate presence-absence (PA) and presence-only (PO) data and to run species distribution models on these to evaluate under which scenarios integrated models perform better than individual dataset models. 

This work is funded by the NERC International Opportunities Fund.

An overview of the data generation, sampling and modelling can be found in [runall.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/runall.R)



Descriptions of each script are as follows:

### Data generation and sampling

All data generation and sampling functions can be run using [setParams.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/setParams.R) and [Functions to generate data and sample.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Functions%20to%20generate%20data%20and%20sample.R). This will generate a point process, thin it to take unstructured samples with ability to include some spatial bias and create a new realisation which is sampled in a non-biased stratified manner to create the structured data.

[setParams.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/setParams.R) : List of default parameters for data generation and sampling

Scripts sourced by [Functions to generate data and sample.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Functions%20to%20generate%20data%20and%20sample.R) : 

[genData.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/genData.R) : Simulate a log Cox Gaussian process using the rLCGP function from the spatstat package. This can be with or without an environmental covariate effect on the simulated intensity. The user can also specify the domain, the variance and shape parameters of the matern covariance and the mean of the intensity. Seed can be specified to make repeatable or left as NULL to be stochastic

[Generate strata levels Lam.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Generate%20strata%20levels%20Lam.R) : This function splits the domain specified in 'genData.R' into a number of strata, the number and pattern of which can be specified by the user. These strata are used to ensure equal coverage in the PA data sampling

[addSpatialBias.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/addSpatialBias.R) : This function assigns probabilities of sampling (thinning) used to generate the PO data. The user can specify the maximum probability of detection and whether bias in PO data is correlated with the environmental covariate.

[make_truth_grid.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/make_truth_grid.R) : This function averages the true intensity within grid squares. The center of each grid square corresponds to a predicted point in prediction data.

[thinData.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/thinData.R) : This function uses the output of 'genData.R' with the output of 'addSpatialBias.R' to thin the point pattern to create the PO data

[sampleStructured.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/sampleStructured.R) : This function uses the output of 'genData.R' and the output of 'Generate strata levels Lam.R' to take stratified random points from the domain using the 'sampleStrata' function from 'Sample from strata.R' then creates structured presence/absence data by assessing overlap between the stratified random points and a new realisation from the point process generated with 'genData.R'.

[Sample from strata.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Sample%20from%20strata.R) : Function to conduct stratified random sampling used within 'sampledStructured.R'

## Modelling

[Run models.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models.R) : This function runs an PO only model in INLA (model A). Data are modelled as point Poisson following the Simpson 2016 approach

[Run models structured.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20structured.R) : This function runs a PA only SDM model in INLA (model B). Data are assumed to come from a binomial distribution

[Run models joint.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20joint.R) : This function jointly models PA and PO data assuming PA data come from a binomial and PO data come from a Poisson point process (model C)

[Run models unstructured bias covariate.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20unstructured%20bias%20covariate.R) : This function runs a PO only model in INLA including a covariate on the bias in the data (model D). Data are modelled as point Poisson following the Simpson 2016 approach

[Run models joint covariate for bias.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20joint%20covariate%20for%20bias.R) : This model adds a covariate for the bias in the unstructured data to the integrated model described above (model E).

[Run models joint second spatial field.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/run%20models%20joint%20second%20field.R) : This joint model does not include a covariate on the bias in the PO data but instead includes a second spatial field with the aim that this spatial field can capture residual variation in the PO data that is not shared with the PA data (model F).

[Create prediction stack.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Create%20prediction%20stack.R) : Required for all modelling scripts to create a stack in which to predict from the models

[validation_function.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/validation_function.R) : This function runs our validation procedures on any fitted model. Outputs produced are the mean absolute error, correlation and fixed effects table. Plots can also be produced


## Parallel code

Scripts that run models in a parallel format.

[run_function_multiple.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/run_function_multiple.R) : This function has the option to run structured, unstructured or joint models with validation steps and returns absolute and relative validation as well as truth and predicted datasets.

[run_scenario.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/run_scenario.R) : A function to run models in parallel based on a scenario. It automatically saves out the model results from validation.

[scenario_sample_size.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/scenario_sample_size.R) : This script runs and saves the output from all models for the structured sample size scenario. 

[scenario_abundance.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/scenario_abundance.R) : This script runs and saves the output from all models for the low abundance scenario (not included in manuscript)

[scenario_bias.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/scenario_bias.R) : This script runs and saves the output from all models for the high bias scenario.

[scenario_rho.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/scenario_bias.R) : This script runs a scenario to compare different covariates for the bias in detection probability (SI only)

[scenario_correlation](https://github.com/NERC-CEH/IOFFsimwork/blob/master/scenario_correlation.R) : This script runs and saves the output from all models for the correlated environment and bias scenario.

[parallel_summary.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/parallel_summary.R) : This script creates a summary from the output from the run_function_multiple.R script.

[Evaluation_of_scenarios.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Evaluation_of_scenarios.R) : This script evaluates each scenario and produces figures and tables. 

[Evaluation_of_scenarios_rho.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Evaluation_of_scenarios.R) : This script evaluates the bias covariate scenario and produces figures and tables. 

