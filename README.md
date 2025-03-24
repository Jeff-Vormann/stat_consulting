This chapters main use is to make the navigation of our Github repository easier to-do and for you to find what-ever information or plots you could want directly. We will also briefly talk about, how to use our code for different datasets, and what you need to understand to properly manage it.

# Data
This branchs only purpose is to save the data that we were provided with as well as the new Datasets that we created. If you don't want to adjust the code to try and run our models with different datasets you can ignore this branch.

# Clean code
Clean code includes all the code that was used to create our models and more importantly you can find all our Visualization that we created using our model.

## Rds

This branch contains all our visualizations. Specifically, you can find all density and histogram plots that we initially created using MoveHMM when first exploring the datasets. However, since we later transitioned to MomentuHMM, we created a sub-branch called covariates. In this sub-branch, we refined the wording of the plots and conducted a more detailed exploration of the potential impacts of covariates. These plots, however only include results for a our chosen number of states.

## custom code animal covariates
This branch provides further insights into each individual animal. For each dataset provided, you will find:


    Our best model as an RDS file

    The corresponding density plot

    The scatter plot

    The R code used to generate these plots


Additionally, where possible we further categorized data by winter or summer habitats or high-activity periods. For each of these categories, we also created density and scatter plots.

# Visualization

This Branch includes Both our Gps data Tracking in html form, for easy execution as well as ourTime of Day Plots.

# Config + Main

    The Config file includes multiple varaibles that you can adjust to create and alter whatever model you could want, more specificly you can chose the Dataset, the step lenght calculation the temperature that you may want to include and many more.


    The Main file is the main R script not adjusted to any specific animal and is the script that you need to run if you adjusted the config file
