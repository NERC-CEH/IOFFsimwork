---
title: "README"
output: html_document
---

## Code to run simulations for the IOFF project.

[Generate field and sample.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Generate%20field%20and%20sample.R) : This script runs through the existing data generation steps


[Generate strata levels Lam.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Generate%20strata%20levels%20Lam.R) : Function to generate sampling effort strata with given numbers of levels for any intensity surface


[Sample from strata.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Sample%20from%20strata.R) : Function to sample points from random field. Samples can either be taken equally from strata ("Stratified" type), or with probablity dependent on given strata sampling probabilities ("Unstructured" type), or with probability inversely related to the strata sampling probabilities ("Intelligent" type). "Stratified" sampling most closely resembles structured surveys whereas "Intelligent" sampling simulates the potential to take more samples from areas where unstructured samples are less likely to be taken (i.e. an adaptive sampling strategy)

[Emily field generation.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Emily%20field%20generation.R) : This script is a manual way to create a Poisson point process of abundance with an environmental covariate. Does not look as pretty as the SpatStat version and the spatial autocorrelation is quite forced. Based loosely on the way Diana created her simulations.

[Run models structured.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Run%20models%20structured.R) : This script takes the beginning of the Run models script to generate data then runs a structured only model in INLA. Both a Binomial and Poisson model are tried to capture the surface. Validation is then performed.


## Steps to code


These steps are largely taken from the pipeline diagram  


### Data generation

 
- [x] Simulate an intensity surface with ability to change lambda (abundance) and the size and shape of the area (currently rectangular to test code is correct)   
- [ ] Simulate an intensity surface with environmental covariate effect     

### Sampling


- [x] Simulate taking different numbers of structured samples (currently these are stratified to ensure equal coverage of the space and randomly sampled within strata mimicking real world sampling designs). Currently uses arbitrary neighbourhood - may want to change this too    
- [x] Simulate taking different numbers of unstructured samples  
- [x] Simulate spatial bias in unstructured sampling (currently this uses the strata to determine a probability of visit which is used to thin the point process, may want to change this to make a continuous spatial effort surface)  
- [x] Allow spatial bias in unstructured sampling (i.e. effort) to vary (this can be adjusted currently by changing stratum sampling probabilities)   
- [ ] Allow spatial bias in unstructured sampling to be correlated with an environmental covariate  
- [ ] Allow detection probability to vary non-spatially in structured data  
- [ ] Allow detection probability to vary non-spatially in unstructured data   
- [ ] Allow coverage of structured survey to change  
- [ ] Vary coverage in relation to environmental gradient length  

### Modelling


- [ ] Model for structured data only  
- [ ] Model for unstructured data only  
- [ ] Joint model with continuous data  
- [ ] Joint model with gridded data  
- [ ] Allow models to have different knowledge of covariates  
- [ ] Allow models to have a second spatial field  


### Evaluation


- [ ] Extract and compare parameters (covariate effects...)  
- [ ] Cross validation by grid (on?)  
- [ ] Cross validation by AUC  
- [ ] Visual bias inspection  
 



