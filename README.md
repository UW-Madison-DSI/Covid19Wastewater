<p align="center">
	<div align="center">
		<img src="./images/covid-droplet.svg" alt="Logo" style="width:33%">
	</div>
</p>

# AFIDSI Wastewater Analysis Package

![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/AFIDSI/DSIWastewater?include_prereleases)
![GitHub last commit](https://img.shields.io/github/last-commit/AFIDSI/DSIWastewater)
[![GitHub pull requests](https://img.shields.io/github/issues-pr/AFIDSI/DSIWastewater)](https://github.com/AFIDSI/DSIWastewater/pulls)
[![License](https://img.shields.io/badge/license-Sustainable_Use_License-green)](./LICENSE.md)
[![contributors](https://img.shields.io/github/contributors/AFIDSI/DSIWastewater)](https://github.com/AFIDSI/DSIWastewater/graphs/contributors)
![codesize](https://img.shields.io/github/languages/code-size/AFIDSI/DSIWastewater) 
 
This is an R package of utilities to perform wastewater data analysis for pathogenic surveillance and monitoring. 

This project is a collaboration between the University of Wiscosin's Data Science Institute (DSI), the Wisconsin Department of Health Services (DHS), and the State Lab of Hygiene (SLH).

DSI:
- <https://datascience.wisc.edu>

DHS:
- <https://www.dhs.wisconsin.gov>
- <https://www.dhs.wisconsin.gov/covid-19/wastewater.htm>

SLH:
- <http://www.slh.wisc.edu/environmental/covid-19-wastewater/>


## Code sections
There are three parts of the package; Data, Data Prep, Analysis tools.

### Data
This package includes Traditional and Wastewater based data about Wisconsin communities.

#### Longitudinal Data
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
#### High Frequency Data
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

#### Extra Information
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

### Data Preparation
This includes functions to create calculated columns, smoothings and filtering.

### Analysis Tools
This includes tools to generate DHS flags for both case and wastewater data. It also includes tools to run random linear forests.

## Continuous results

- View our analysis repo [here](https://github.com/AFIDSI/Covid19-Wastewater-Analysis)

## Installation

If you already have devtools installed you can install the package with
```
devtools::install_github("AFIDSI/DSIWastewater")
```
otherwise we have a comprehensive instructions [here](./docs/Install.md).

## Documentation

- View the [API Reference](./docs/api/api.md).

<p>
    <div align="center">
        <img src="./images/architecture.svg" alt="API" style="width:200px">
        <div>
            <label>API Architecture Overview</label>
        </div>
    </div>
    <br />
</p>
 
- View the package vignettes with:

```
vignette(package = "DSIWastewater")
```

- Look at all package functionality with:

```
help(package = "DSIWastewater")
```
 

<!-- LICENSE -->
## License

Distributed under the Sustainable Use License. See the [license](./LICENSE.md) for more information.

<!-- Contact -->
## Team
Email:
- Marlin Lee - (mailto:mrlee6@wisc.edu)

- Abe Megahed - (mailto:amegahed@wisc.edu)

- Kyllan Wunder - (mailto:kwunder@wisc.edu)

Repos:
- Package Link: [https://github.com/AFIDSI/DSIWastewater](https://github.com/AFIDSI/DSIWastewater)

- Analysis Link: [https://github.com/AFIDSI/Covid19-Wastewater-Analysis](https://github.com/AFIDSI/Covid19-Wastewater-Analysis)
