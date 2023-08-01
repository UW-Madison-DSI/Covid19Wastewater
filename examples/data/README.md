# Data
This package includes traditional (case based) and wastewater based data about Wisconsin communities.

## 1) Longitudinal Data
This dataset includes longitudinal wastewater data collected 1-6 times per week from 2019 to 2023. It is accompanied by case data. The dataset files are:  

1) Longitudinal wastewater data:
[WasteWater_data.RData](../../docs/vignettes/longitudinal_data_waste.pdf)

2) Corresponding case data:
[Case_data.RData](../../docs/vignettes/longitudinal_data_case.pdf)

To access the longitudinal data, load the datasets using the following code:
```
# Load the longitudinal wastewater data
load("Was# Analysis Tools
The following are the analysis tools used in this package:

## Examples:

### DHS method
This code comes from our collaborators at the DHS and is an example of of their flagging method that is used on their dashboard (link to dashboard)

### Time series offset
This vignette demonstrates how to use the offset analysis tools to find the temporal offset in days between wastewater and case measurements.

### Linear forest
This package contains a Linear Forest modeling tool for finding an optimal breakdown of linear components. This is most useful for looking for cofactors to the N1 vs case data.
teWater_data.RData")

# Load the corresponding case data
load("Case_data.RData")
```
## 2) High Frequency Data
This dataset contains high-frequency testing data collected over a 7-day testing period. It includes 9 samples reported per day and is accompanied by corresponding case data. The dataset files are:  

1) High-frequency wastewater data:
[HFG_data.RData](../../docs/vignettes/HFG_data_waste.pdf)

2) Corresponding case data:
[HFGCase_data.RData](../../docs/vignettes/HFG_data_case.pdf)

To access the high-frequency data, load the datasets using the following code:  
```
# Load the high-frequency wastewater data  
load("HFGWaste_data.RData")  

# Load the corresponding case data  
load("HFGCase_data.RData")
```

## 3) Extra Information
The package also provides additional information in the form of dataframes. The extra info datasets are as follows:  

1) Population of each community in the dataset:
[Pop_data.RData](../../docs/vignettes/population_data.pdf)

2) The percentage contribution of each variant in Wisconsin: 
[Covariants_data.RData](../../docs/vignettes/variant_data.pdf)

To load the extra info datasets, use the following code:  

```
# Load the population data
load("Pop_data.RData")

# Load the covariants data
load("Covariants_data.RData")
```
