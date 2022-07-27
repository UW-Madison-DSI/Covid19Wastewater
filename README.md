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

## Sample results

- [Top Level Analysis](inst/doc/vignettes_DHSTopLevelAnalysis_Base.pdf)

- [Top Level Outlier Analysis](inst/doc/vignettes_DHSTopLevelAnalysis_Outlier.pdf)

## Installation
There are two ways to install the AFIDSI Wastewater Analysis package:

### 1. Using Devtools

If you have a GitHub account, then you can install the package by running the Devtools command below:

```
devtools::install_github(
    "AFIDSI/DSIWastewater",
    auth_token = "REPLACEWITHYOURAUTHTOKEN"
)
```

To get a AUTHTOKEN you need to go to https://github.com/settings/tokens and create one with at least [repo level access](./docs/github/repo-level-access.md).

When running this command, you will be prompted to select which packages you want updated/installed.  We strongly suggest you select '1: ALL' so that you don't need to install any dependencies later.  To install the dependencies later, you can run the following R command:

```
devtools::install_dev_deps(pkg = "DSIWastewater")
```

### 2. Using a TAR (.tgz) file

If you do not have a GitHub account or would rather not use GitHub, you can also install the package using a tar file.

1. Download the tar file

You can download the AFIDSI wastewater package tar file from the following location:
https://github.com/AFIDSI/DSIWastewater/blob/main/DSIWastewater_0.2.01.tar.gz

---
**Tip:** 
When you download it, make sure that it remains zipped.  It should be a .tar.gz file rather than a .tar file.   If the file has been unzipped, then you can re-zip it using the following command:

```
gzip DSIWastewater_0.2.01.tar
```

---

2. Install the tar file

To install the tar file, you can use the following command:

```
install.packages("DSIWastewater_0.2.01.tar.gz", repos = NULL, type="source") 
```

Alternatively, if you are using RStudio, you can install the tar file [using the user interface](./docs/r-studio/installing-packages.md).


## Viewing
 
- View the package vignettes with:

```
vignette(package = "DSIWastewater")
```

- Look at all package functionality with:

```
help(package = "DSIWastewater")
```
 
- Release notes are stored here:
 
https://docs.google.com/document/d/1-Rbd0YTyPZ2slbW9ksvF36n_nhhEBNi0vJZAmhwNHsg/edit


<!-- LICENSE -->
## License

Distributed under the Sustainable Use License. See `LICENSE.md` for more information.

<!-- CONTACT -->
## Contact

Steve Goldstein - (mailto:sgoldstein@wisc.edu) - email

Project Link: [https://github.com/AFIDSI/DSIWastewater](https://github.com/AFIDSI/DSIWastewater)

## Organization practice
 
Thursday at 5 PM - freeze code. Have package ready for friday meeting and make no changes between then and friday meeting

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
