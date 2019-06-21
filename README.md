---
title: "README"
output: html_document
---

## New master branch

The following changes have been made in this branch which are added to the master branch:

1.	Default parameters in setParams changed to simulate unbiased sampling of the unstructured data and no environmental covariate

2.	Unstructured models corrected to correctly use the expectation calculated from the mesh

3.	Dimensions of grid increased to the same as the resolution of the grid (also changes to sigma2x and kappa defaults to provide a surface of reasonable complexity, added ability to specify sigma2x in data generation)

4.	Validation changed to a) have the option for either absolute or relative validation b) show the truth on a grid for easier comparison c) present untransformed values

5.	Function to generate prediction stack changed to provide different stacks for different model types

6.	Added ability to change “quadrat” (i.e. site) size of the structured sample (qsize)

7.	Added function to adjust for the quadrat/site size in structured model output – i.e. calculate the per-unit area occupancy probability from the occupancy probability in the quadrat/site


## Code to run simulations for the IOFF project.

This repository hosts functions to simulate structured and unstructured data and to run species distribution models on these to evaluate under which scenarios integrated/joint models perform better than individual dataset models. 

This work is funded by the NERC International Opportunities Fund.

An overview of the data generation, sampling and modelling can be found in [runall.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/runall.R)



Descriptions of each script are as follows:

### Data generation and sampling

All data generation and sampling functions can be run using [setParams.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/setParams.R) and [Functions to generate data and sample.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Functions%20to%20generate%20data%20and%20sample.R). This will generate a point process, thin it to take unstructured samples with ability to include some spatial bias and create a new realisation which is sampled in a non-biased stratified manner to create the structured data.

[setParams.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/setParams.R) : List of default parameters for data generation and sampling

Scripts sourced by [Functions to generate data and sample.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Functions%20to%20generate%20data%20and%20sample.R) : 

[genData.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/genData.R) : Simulate a log Cox Gaussian process using the rLCGP function from the spatstat package. This can be with or without an environmental covariate effect on the simulated intensity. The user can also specify the domain, the variance and shape parameters of the matern covariance and the mean of the intensity. Seed can be specified to make repeatable or left as NULL to be stochastic

[Generate strata levels Lam.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Generate%20strata%20levels%20Lam.R) : This function splits the domain specified in 'genData.R' into a number of strata, the number and pattern of which can be specified by the user. These strata are used to represent areas which may have different thinning probabilities and are used to ensure equal coverage in the structured data sampling

[addSpatialBias.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/addSpatialBias.R) : This function uses the strata created with 'Generate strata levels Lam.R' and assigns probabilities of sampling (thinning) used to generate the unstructured data. The user can specify these probabilites with a vector equal in length to the number of strata or they can be randomly generated

[make_truth_grid.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/make_truth_grid.R) : This function averages the true intensity within grid squares. The center of each grid square corresponds to a predicted point in prediction data.

[thinData.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/thinData.R) : This function uses the output of 'genData.R' with the output of 'addSpatialBias.R' to thin the point pattern to create the unstructured data

[sampleStructured.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/sampleStructured.R) : This function uses the output of 'genData.R' and the output of 'Generate strata levels Lam.R' to take stratified random points from the domain using the 'sampleStrata' function from 'Sample from strata.R' then creates structured presence/absence data by assessing overlap between the stratified random points (and the 5 by 5 neighbourhood around each point) and a new realisation from the point process generated with 'genData.R'.

[Sample from strata.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Sample%20from%20strata.R) : Function to sample points from random field. Samples can either be taken equally from strata ("Stratified" type), or with probablity dependent on given strata sampling probabilities ("Unstructured" type), or with probability inversely related to the strata sampling probabilities ("Intelligent" type). "Stratified" sampling most closely resembles structured surveys whereas "Intelligent" sampling simulates the potential to take more samples from areas where unstructured samples are less likely to be taken (i.e. an adaptive sampling strategy)

## Modelling

[Run models.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models.R) : This function runs an unstructured data only model in INLA. Data are modelled as point Poisson following the Simpson 2016 approach

[Run models covariate for bias.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20covariate%20for%20bias.R) : This function runs a unstructured data only model in INLA including a covariate on the bias in the data. Data are modelled as point Poisson following the Simpson 2016 approach

[Run models structured.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20structured.R) : This function runs a structured data only SDM model in INLA. Data are assumed to come from a binomial distribution

[Run models joint.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20joint.R) : This function jointly models structured and unstructured data assuming structured data come from a binomial and unstructured data are Poisson distributed.

[Run models joint covariate for bias.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20joint%20covariate%20for%20bias.R) : This model adds a covariate for the bias in the unstructured data to the joint model described above.

[run models joint second field.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/run%20models%20joint%20second%20field.R) : This joint model does not include a covariate on the bias in the unstructured data but instead includes a second spatial field with the aim that this spatial field can capture residual variation in the unstructured data that is not shared with the structured data.

[validation_function.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/validation_function.R) : This function runs our validation procedures on any fitted model. Outputs produced are: PLOT - of the truth inc data, predicted mean intensity, standard deviation of predicted intensity, and the relative differences between estimate and truth. SUMMARY_RESULTS - the beginnings of an output table. List includes MAE, model name, all differences, the worst performing grid squares, and the best (i.e. lowest relative difference).

## Parallel code

Scripts that run models in a parallel format.

[run_function_multiple.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/run_function_multiple.R) : This function has the option to run structured, unstructured or joint models with validation steps and returns absolute and relative validation as well as truth and predicted datasets.

[Evaluation_structured_n.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Evaluation_structured_n.R) : This script runs all models for the structured sample size scenario, produces figures and tables. Will be reworked to be a function to run any of the scenarios we want.

[parallel_summary.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/parallel_summary.R) : This script creates a summary from the output from the run_function_multiple.R script.

[scenario_sample_size.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/scenario_sample_size.R) : This script runs and saves the output from all models for the structured sample size scenario. 

## Steps to code


These steps are largely taken from the pipeline diagram  


### Data generation

 
- [x] Simulate an intensity surface with ability to change lambda (abundance) and the size and shape of the area (currently rectangular to test code is correct)   
- [x] Simulate an intensity surface with environmental covariate effect     

### Sampling


- [x] Simulate taking different numbers of structured samples (currently these are stratified to ensure equal coverage of the space and randomly sampled within strata mimicking real world sampling designs). Currently uses arbitrary neighbourhood - may want to change this too    
- [x] Simulate taking different numbers of unstructured samples  
- [x] Simulate spatial bias in unstructured sampling (currently this uses the strata to determine a probability of visit which is used to thin the point process, may want to change this to make a continuous spatial effort surface)  
- [x] Allow spatial bias in unstructured sampling (i.e. effort) to vary (this can be adjusted currently by changing stratum sampling probabilities)   
- [x] Allow spatial bias in unstructured sampling to be correlated with an environmental covariate  

These are now lower priority:  
- [ ] Allow coverage of structured survey to change  
- [ ] Vary coverage in relation to environmental gradient length
- [ ] Allow detection probability to vary non-spatially in structured data  
- [ ] Allow detection probability to vary non-spatially in unstructured data  

### Modelling


- [x] Model for structured data only  
- [x] Model for unstructured data only  
- [x] Joint model with continuous data  
- [x] Allow models to have different knowledge of covariates  
- [x] Allow models to have a second spatial field  


### Evaluation


- [x] Extract and compare parameters (covariate effects...) 
- [x] Mean absolute error by grid
- [x] Correlation by grid
- [x] Visual bias inspection  
 



