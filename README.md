<p align="center">
  <div align="center">
    <img src="./images/covid-droplet.svg" alt="Logo" style="width:33%">
  </div>
</p>

# DSI Covid Wastewater Analysis
 
This is a collaborative project between the University of Wiscosin's Data Science Institute (DSI) and the Wisconsin Department of Health Services (DHS) on data analsysis for statewide covid wastewater surveillance. 

DSI:
- <https://datascience.wisc.edu>

DHS:
- <https://www.dhs.wisconsin.gov>
- <https://www.dhs.wisconsin.gov/covid-19/wastewater.htm>
- <http://www.slh.wisc.edu/environmental/covid-19-wastewater/>

## Sample results

- [Top Level Analysis](inst/doc/vignettes_DHSTopLevelAnalysis_Base.pdf)

- [Top Level Outlier Analysis](inst/doc/vignettes_DHSTopLevelAnalysis_Outlier.pdf)

## Installation

Run this R command to install devtools  
```
install.packages("devtools")
```

Download the package using the R command below  
```
devtools::install_github(
    "AFIDSI/DSIWastewater",
    auth_token = "REPLACEWITHYOURAUTHTOKEN"
)
```
 
To get an AUTHTOKEN you need to go to https://github.com/settings/tokens and create one with at least [repo level access](./docs/repo-level-access.md).


If you do not have a github account, you can install this package using a .tar.gz file. 

Open rStudios, click Tools, and install packages  
Under 'Install from:' select 'Package Archive File (.tar.gz)'  
Click browse and select the DSIWastewater.tar.gz file from where you downloaded it to  
Make sure your 'Install to Library:' is the correct R package library (Default is usually correct)  
Then click install.  

After the R console says 'DONE' 

Install devtools (see above)  

Run this R command to install all of our dependences for our package  
```
devtools::install_dev_deps(pkg = "DSIWastewater")
```
This will prompt you to select which packages you want updated/installed  
We STRONGLY suggest you select '1: ALL' so that you don't need to install any dependeces later  

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

## Code style
 
- Camel case for functions and file names
 
- Snake case for variables
 
- _data for data objects

- _Plot if the function creates a plot

<!-- LICENSE -->
## License

Distributed under the Sustainable Use License. See `LICENSE.md` for more information.

<!-- CONTACT -->
## Contact

Steve Goldstein - (mailto:sgoldstein@wisc.edu) - email

Project Link: [https://github.com/AFIDSI/DSIWastewater](https://github.com/AFIDSI/DSIWastewater)

## Organization practice
 
Thursday at 5 PM - freeze code. Have package ready for friday meeting and make no changes between then and friday meeting
