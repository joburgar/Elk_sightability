# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#####################################################################################
# 03_sightability_simulations.R
# script to simulate sightability models to inform sightabilty survey study design
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 9-Mar-2021
#####################################################################################

# overall process:
#- Run basic mHT sightablity model simulations

data("obs.m")
data("exp.m")
data("sampinfo.m")

# experimental data frame consists of:
# year (year of test trial)
# observed (binary; 1 if telem animal observed, 0 otherwise)
# voc (covariate; amount of screening cover within 10 m of first animal seen)
# grpsize (group size)
# each row represents an independent sightability trial with observed representing the random variables
exp.m[1:5,]

# operational data frame consists of:
# each row corresponds to an independently sighted group with animal-specific covariates
obs.m[1:5,]
