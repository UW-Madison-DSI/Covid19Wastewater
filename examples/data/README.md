# Data
This package includes traditional (case based) and wastewater based data about Wisconsin communities.

## Longitudinal Data
This dataset includes longitudinal wastewater data collected 1-6 times per week from 2019 to 2023. It is accompanied by case data. The dataset files are:  

WasteWater_data.RData: Longitudinal wastewater data  
Case_data.RData: Corresponding case data  
To access the longitudinal data, load the datasets using the following code:
```
# Load the longitudinal wastewater data
load("WasteWater_data.RData")

# Load the corresponding case data
load("Case_data.RData")
```
## High Frequency Data
This dataset contains high-frequency testing data collected over a 7-day testing period. It includes 9 samples reported per day and is accompanied by corresponding case data. The dataset files are:  
HFGWaste_data.RData: High-frequency wastewater data  
HFGCase_data.RData: Corresponding case data  

To access the high-frequency data, load the datasets using the following code:  
```
# Load the high-frequency wastewater data  
load("HFGWaste_data.RData")  

# Load the corresponding case data  
load("HFGCase_data.RData")
```

## Extra Information
The package also provides additional information in the form of dataframes. The extra info datasets are as follows:  

pop_data.RData: Contains the population of each community in the dataset  
Covariants_data.RData: Contains the percentage contribution of each variant in Wisconsin  
To load the extra info datasets, use the following code:  

```
# Load the population data
load("pop_data.RData")

# Load the covariants data
load("Covariants_data.RData")
```