<p align="center">
	<div align="center">
		<img src="./images/covid-droplet.svg" alt="Logo" style="width:33%">
	</div>
</p>

# AFIDSI Wastewater Analysis Package
 
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
This includes tools to genearte DHS flags for both case and wastewater data. It also includes tools to run random linear forests.

## Continuous results

- View our analysis repo [here](https://github.com/AFIDSI/Covid19-Wastewater-Analysis)

## Installation
There are two ways to install the AFIDSI Wastewater Analysis package:

### 1. Using Devtools

If you do not have the [Devtools](https://www.r-project.org/nosvn/pandoc/devtools.html) R package install it by run this command:

```
install.packages("devtools")
```

Once Devtools is installed anyone can install the package by running the command:

```
devtools::install_github("AFIDSI/DSIWastewater")
```

When running this command, you will be prompted to select which packages you want updated/installed.  We strongly suggest you select '1: ALL' so that you don't need to install any dependencies later.  

To install the dependencies later, you can run the following command:

```
devtools::install_dev_deps(pkg = "DSIWastewater")
```

If you would ever like to recive an update of our package simply remove the package using this command:

```
remove.packages("DSIWastewater")
```

Then reinstall the package using the above commands.


### 2. Using a TAR (.tgz) file

You can also install the package using a tar file.

1. Download the tar file

You can download the AFIDSI wastewater package tar file from the following location(in this current repo):
https://github.com/AFIDSI/DSIWastewater/blob/main/DSIWastewater_0.5.1.tar.gz

---
**Tip:** 
When you download it, make sure that it remains zipped.  It should be a .tar.gz file rather than a .tar file.   If the file has been unzipped, then you can re-zip it using the following linux command:

```
gzip DSIWastewater_0.5.1.tar.gz
```

---

2. Install the tar file

To install the tar file, you can use the following command:

```
install.packages("DSIWastewater_0.5.1.tar.gz", repos = NULL, type="source") 
```

Alternatively, if you are using RStudio, you can install the tar file [using the user interface](./docs/r-studio/installing-packages.md).


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

Distributed under the Sustainable Use License. See `LICENSE.md` for more information.

<!-- Contact -->
## Team
Email:
- Marlin Lee - (mailto:mrlee6@wisc.edu)

- Abe Megahed - (mailto:amegahed@wisc.edu)

- Kyllan Wunder - (mailto:kwunder@wisc.edu)

Repos:
- Package Link: [https://github.com/AFIDSI/DSIWastewater](https://github.com/AFIDSI/DSIWastewater)

- Analysis Link: [https://github.com/AFIDSI/Covid19-Wastewater-Analysis](https://github.com/AFIDSI/Covid19-Wastewater-Analysis)

## Code style
 
- Camel case for functions and file names
 
- Snake case for variables
 
- _data for data objects

- _Plot if the function creates a plot

## Color standards

- ![#0571b0](https://via.placeholder.com/15/0571b0/0571b0.png) `#0571b0` Major decrease
- ![#92c5de](https://via.placeholder.com/15/92c5de/92c5de.png) `#92c5de` Moderate decease
- ![#979797](https://via.placeholder.com/15/979797/979797.png) `#979797` Flucuating
- ![#ffffff](https://via.placeholder.com/15/ffffff/ffffff.png) `#ffffff` No Change
- ![#f4a582](https://via.placeholder.com/15/f4a582/f4a582.png) `#f4a582` Moderate increase
- ![#ca0020](https://via.placeholder.com/15/ca0020/ca0020.png) `#ca0020` Major increase
