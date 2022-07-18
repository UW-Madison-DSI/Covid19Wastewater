<p align="center">
  <div align="center">
    <img src="./images/covid-droplet.svg" alt="Logo" style="width:33%">
  </div>
</p>

# DSI Covid Wastewater Analysis
## Package Branch
 
This is a collaborative project with Wisconsin Department of Health Services (DHS) on statewide covid wastewater surveillance. 

The key DHS sites are:
- <https://www.dhs.wisconsin.gov/covid-19/wastewater.htm>
- <http://www.slh.wisc.edu/environmental/covid-19-wastewater/>
 
 
## Installation
Download package using the R command below
```
devtools::install_github(
                        "AFIDSI/DSIWastewater",
                        auth_token = "REPLACEWITHYOURAUTHTOKEN"
)
```
 
To get a AUTHTOKEN you need to go to https://github.com/settings/tokens and create one with at least repo level access

## Viewing
 
View The package vignettes with:
 
vignette(package = "DSIWastewater")

Look at all package functionality with:

help(package = "DSIWastewater")
 
Release notes are stored here:
 
https://docs.google.com/document/d/1-Rbd0YTyPZ2slbW9ksvF36n_nhhEBNi0vJZAmhwNHsg/edit
 
## Code style
 
Camel case for functions and file names
 
Snake case for variables
 
_data for data objects

_Plot if the function creates a plot

## Organization practice
 
Thursday at 5 PM - freeze code. Have package ready for friday meeting and make no changes between then and friday meeting
