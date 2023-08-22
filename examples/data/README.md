# Data
This package includes traditional (case-based) and wastewater-based data about Wisconsin communities.

## 1) Longitudinal Data
This dataset includes longitudinal wastewater data collected 1-6 times per week from 2019 to 2023. It is accompanied by case data. The dataset files are:  

1) Longitudinal wastewater data:
[WasteWater_data.RData](../../docs/vignettes/longitudinal_data_waste.pdf)

2) Corresponding case data:
[Case_data.RData](../../docs/vignettes/longitudinal_data_case.pdf)

To access the longitudinal data, load the datasets using the following code:
```
# Load the longitudinal wastewater data
load("WasteWater_data.RData")

# Load the corresponding case data
load("Case_data.RData")
```
## 2) High-Frequency Data
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

3) Auxillary information from longitudinal wastewater data:
[Aux_info_data.RData](../../docs/vignettes/Aux_info_data.pdf)

4) Example data to test with (longitudinal wastewater and case data merged):
[example_data.RData](../../docs/vignettes/example_data.pdf)

5) Traditionally collected case data for the Madison interceptors:
[Intercepter_data_case.RData](../../docs/vignettes/Intercepter_data_case.pdf)

To load the extra info datasets, use the following code:  

```
# Load the population data
load("Pop_data.RData")

# Load the covariants data
load("Covariants_data.RData")

# Load the auxiliary data
load("Aux_info_data.RData")

# Load the example data
load("example_data.RData")

# Load the intercepter data
load("Intercepter_data_case.RData")
```
