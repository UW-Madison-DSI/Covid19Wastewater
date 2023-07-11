| Column Name | Description |
| ----------- | ----------- | 
| avg_sars_cov2_conc | Average of the two Covid-19 genes in gene copies per liter  |
| BCoV / bcov_rec_rate / bcov_spike_conc | The lab added a set amount of Bovine Respiratory Coronavirus (BCoV) then measured what percent they recovered. This is used to estimate what amount they might be underestimating the Covid-19 signal |
| CollectedCases* | One of the methods used to count the number of positive tests. |
| composite_freq* |  |
| concentration_method | There have been a couple of methods of measuring the concentration of the gene in the sample. |
| conductivity | The conductivity of the sample. |
| conf_case / ConfirmedCases* | One of the methods used to count the number of positive tests. |
| conf_death | The number of people confirmed to have died of covid on the day. |
| county | The county the sample is from. |
| date / Date | The date the sample was collected. Back dated a bit because it takes a couple days to analyze the samples | 
| EpisodeCase / EpisodeCases* |  One of the methods used to count the number of positive tests.  |
| Filter | For the High frequency wastewater data they had three filters for each location. This notes which of the Filter the sample came from. |
| FirstConfirmed* |  One of the methods used to count the number of positive tests.  |
| flow | The measured flow of the wastewater over the time the sample was taken. |
| hf183 / HF183 | Hf183 is a sewage-associated marker used to normalize use |
| lab_submitter | These datasets include samples from both the Wisconsin State lab of Hygiene(WSLH) and the Milwaukie lab. If not in included all the data is from the WSLH |
| N1 / N2 | The two Covid-19 genes that are detected in the wastewater and reported. It is measured in gene copies per liter|
| lod / loq | Level of detection (lod) and Level of quantification. Lod is the concentration at which the lab can not be sure the gene is there and loq is the point where they can’t be confident about the measured result. |
| n1_sars_cov2_error / n2_sars_cov2_error | For some samples they were measured three times and a standard error of the measurements was given. The data is not normally distributed so they need to be used with caution |
| PMMoV / PMMOV | Concentration of Pepper mild mottle virus (PMMoV) is used as a measure of how many people are using the sewage system. |
| ph | The pH of the sample. Could affect measuring ability. |
| FirstConfirmed* |  One of the methods used to count the number of positive tests. |
| pop | The population that the sewage facility supports. Normally an approximation based on the city is serves |
| site / Site | The name of the location the sample is collected from. Often the city name |
| prob_case* |  One of the methods used to count the number of positive tests. |
| prob_death| The number of people who it was suspected to have died of covid on the day. |
| regions | What region of Wisconsin The site is from. Based on the DHS classification [here] (https://www.dhs.wisconsin.gov/aboutdhs/regions.htm) |
| ReportedCases* |  One of the methods used to count the number of positive tests. |
| SpecCollectedCase* |  One of the methods used to count the number of positive tests. |
| state | Most of the data in the dataset is from Wisconsin but some fraction is from Minnesota |
| temperature | The temperature of the sample. Might affect the validity of the measurements |
| tests | Number of Covid-19 tests given to people. Slight underestimate due to only having clinic data* |
| Total_sequences | Total number of sequences detected in the time frame. Used to convert variant info into proportions |
| week | What week it is |
| Well | Largely redundant. It communicates which technical replicate it was but as there is no difference between being the first or third done it largely functions as an id.