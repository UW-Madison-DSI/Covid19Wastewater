# COVID-19_WastewaterAnalysis
# Package Branch

Collaboration with DHS on statewide wastewater surveillance. Their key sites are

- <https://www.dhs.wisconsin.gov/covid-19/wastewater.htm>
- <http://www.slh.wisc.edu/environmental/covid-19-wastewater/>


Download package using the R command below

devtools::install_github(
"AFIDSI/COVID-19_WastewaterAnalysis",
ref="MainBranchInPackageFormat", subdir = "DSIWasteWater",
auth_token = "REPLACEWITHYOURAURTHTOKEN"
)

To get a AURTHTOKEN you need to go to https://github.com/settings/tokens and create one

View The package vignnettes with:

vignette(package = "DSIWasteWater")

Are Release notes are stored here:

https://docs.google.com/document/d/1-Rbd0YTyPZ2slbW9ksvF36n_nhhEBNi0vJZAmhwNHsg/edit

##Code style

Camle case for functions

Snake case for variables

_data for data objects

add _Plot if the function creates a plot
