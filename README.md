---
title: "README"
output: html_document
---

## Code to run simulations for the IOFF project.

[Generate field and sample.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Generate%20field%20and%20sample.R) : This script runs through the existing data generation steps


[Generate strata levels Lam.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Generate%20strata%20levels%20Lam.R) : Function to generate sampling effort strata with given numbers of levels for any intensity surface


[Sample from strata.R](https://github.com/NERC-CEH/IOFFsimwork/blob/master/Sample%20from%20strata.R) : Function to sample points from random field. Samples can either be taken equally from strata ("Stratified" type), or with probablity dependent on given strata sampling probabilities ("Unstructured" type), or with probability inversely related to the strata sampling probabilities ("Intelligent" type). "Stratified" sampling most closely resembles structured surveys whereas "Intelligent" sampling simulates the potential to take more samples from areas where unstructured samples are less likely to be taken (i.e. an adaptive sampling strategy)