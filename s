[33mcommit 25d69343aec6f9b33cf4f1244ebc572cf8acbd3c[m[33m ([m[1;36mHEAD -> [m[1;32mRobustBinning[m[33m, [m[1;31morigin/RobustBinning[m[33m)[m
Author: Steve Goldstein <sgoldstein@wisc.edu>
Date:   Thu Oct 14 14:20:09 2021 -0500

    removing unneeded code chunks

 Code-Notebooks/RobustBinning/MakeScatterplot.Rmd   | 114 [32m+[m[31m---------[m
 .../test/model/stdout/MakeScatterplot.02.html      | 248 [32m+++++++++++++++++++++[m
 .../test/testScripts/refactoringSetup.man          |  13 [32m++[m
 3 files changed, 264 insertions(+), 111 deletions(-)

[33mcommit ebcf6e1600c454d10415106453dfbd937f4d9638[m
Author: Steve Goldstein <sgoldstein@wisc.edu>
Date:   Thu Oct 14 14:00:03 2021 -0500

    Turned off chunks after binning

 Code-Notebooks/RobustBinning/MakeScatterplot.Rmd   |   8 [32m+[m[31m-[m
 .../test/model/stdout/MakeScatterplot.00.html      | 222 [32m++++++++++++++++++++[m
 .../test/model/stdout/MakeScatterplot.01.html      | 232 [32m+++++++++++++++++++++[m
 3 files changed, 458 insertions(+), 4 deletions(-)

[33mcommit 7cb51cedcd6ad33de63d4528050fbd844f470f6a[m
Author: Steve Goldstein <sgoldstein@wisc.edu>
Date:   Thu Oct 14 13:45:03 2021 -0500

    tweaking file structure

 Code-Notebooks/RobustBinning/{NoTSCorrelation.Rmd => MakeScatterplot.Rmd} | 0
 Code-Notebooks/RobustBinning/test/{error => model/stderr}/.placeholder    | 0
 .../RobustBinning/test/{model_out => model/stdout}/NoTSCorrelation.html   | 0
 .../RobustBinning/test/{model_err => output/stderr}/.placeholder          | 0
 Code-Notebooks/RobustBinning/test/output/{ => stdout}/.placeholder        | 0
 5 files changed, 0 insertions(+), 0 deletions(-)

[33mcommit 88beed1737ae5aa16f21385a90a8552b37f9e1c0[m
Author: Steve Goldstein <sgoldstein@wisc.edu>
Date:   Thu Oct 14 12:36:04 2021 -0500

    Setting up dir structure for testing;

 Code-Notebooks/RobustBinning/NoTSCorrelation.Rmd   |   5 [32m+[m[31m-[m
 .../RobustBinning/test/error/.placeholder          |   0
 .../RobustBinning/test/input/.placeholder          |   0
 .../RobustBinning/test/model_err/.placeholder      |   0
 .../test/model_out/NoTSCorrelation.html            | 496 [32m+++++++++++++++++++++[m
 .../RobustBinning/test/output/.placeholder         |   0
 6 files changed, 499 insertions(+), 2 deletions(-)

[33mcommit 89559c5f9fd60f2fa4d5c491681a9f23c2f320f4[m
Author: Steve Goldstein <sgoldstein@wisc.edu>
Date:   Thu Oct 14 11:01:26 2021 -0500

    NoTSCorrelation is starting point for refactoring to support binning

 Code-Notebooks/RobustBinning/NoTSCorrelation.Rmd | 282 [32m+++++++++++++++++++++++[m
 Code-Notebooks/RobustBinning/README              |   5 [32m+[m
 README                                           |   3 [31m-[m
 3 files changed, 287 insertions(+), 3 deletions(-)

[33mcommit 6a743bad3ca29ea3b5cdb6974ff601c4db066b0c[m
Author: Steve Goldstein <sgoldstein@wisc.edu>
Date:   Wed Oct 13 17:16:03 2021 -0500

    Created .gitignore

 .Rhistory  | 512 [31m-------------------------------------------------------------[m
 .gitignore |   2 [32m+[m
 2 files changed, 2 insertions(+), 512 deletions(-)

[33mcommit 0bc063df7dbe499762b183ec7cd4853bc3d66da8[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 11:46:13 2021 -0500

    Colname analysis

 Code-Notebooks/DataExplanation/OverviewOfData.Rmd | 46 [32m+++++++++++++++++++++[m[31m--[m
 1 file changed, 42 insertions(+), 4 deletions(-)

[33mcommit 77b2672d715fec375a0b0b932cc06f8e4d1a6b0d[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 08:09:51 2021 -0500

    basic data outline done

 Code-Notebooks/DataExplanation/OverviewOfData.Rmd  | 281 [32m+++++++++++++++++++[m[31m-[m
 Code-Notebooks/DataExplanation/OverviewOfData.html | 288 [32m++++++++++++++++++++[m[31m-[m
 .../VarianceAnalysis/HFGVarianceAnalysis.Rmd       |   8 [31m-[m
 lib/CasesDataProccess.R                            |  13 [32m+[m[31m-[m
 4 files changed, 569 insertions(+), 21 deletions(-)

[33mcommit cb74a7d83af3674508b6c031f04a0e3fa1683b40[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 05:25:07 2021 -0500

    removed signal decay into own branch

 Figures/HFG-varience-Figure.pdf           | Bin [31m37696[m -> [32m0[m bytes
 Figures/HFGPredictVar.pdf                 | Bin [31m199226[m -> [32m0[m bytes
 Figures/MEMThreeSets.PNG                  | Bin [31m11071[m -> [32m0[m bytes
 Figures/MEMThreeSetsExclude.PNG           | Bin [31m12395[m -> [32m0[m bytes
 Figures/SixLM.PNG                         | Bin [31m14025[m -> [32m0[m bytes
 Figures/WasteWaterDifficulties.pdf        | Bin [31m382408[m -> [32m0[m bytes
 Figures/WasteWaterDifficultysExtended.pdf | Bin [31m139701[m -> [32m0[m bytes
 Figures/medrxivFig.pdf                    | Bin [31m83624[m -> [32m0[m bytes
 8 files changed, 0 insertions(+), 0 deletions(-)

[33mcommit 7c7c9aff2a16b357176e8743443f85f919d4329b[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 05:24:32 2021 -0500

    removed signal decay into own branch

 Code-Notebooks/README                          |   4 [31m-[m
 Code-Notebooks/SignalDecay/NSignalDecay.Rmd    | 333 [31m------------------------[m
 Code-Notebooks/SignalDecay/NSignalDecay2.Rmd   | 320 [31m-----------------------[m
 Code-Notebooks/SignalDecay/NSignalDecaySup.Rmd | 341 [31m-------------------------[m
 4 files changed, 998 deletions(-)

[33mcommit b8cf5969c2663d878cdadcb4fb317985ac1361e8[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 05:17:09 2021 -0500

    removed files stored in other branch

 Code-Notebooks/RecreateingPlots/.Rhistory          |   1 [31m-[m
 Code-Notebooks/RecreateingPlots/StackedBarPlot.Rmd | 225 [31m--------[m
 Code-Notebooks/RecreateingPlots/medrxivFig.Rmd     |  85 [31m---[m
 docs/2021-07-01PresentationOutline                 |  67 [31m---[m
 docs/MMSD_SARS-CoV-2_OutlineV04222021.docx         | Bin [31m17766[m -> [32m0[m bytes
 docs/SignalDecayAnalysis_2021-08-03.docx           | Bin [31m723278[m -> [32m0[m bytes
 docs/SignalDecayAnalysis_2021-08-03.pdf            | Bin [31m357574[m -> [32m0[m bytes
 docs/StackedBarPlot.html                           | 410 [31m--------------[m
 docs/WasteWater-Analysis-2.0-Extended.html         | 567 [31m-------------------[m
 docs/WasteWaterAnalysis2.html                      | 466 [31m----------------[m
 docs/WasteWaterAnalysis2N2.html                    | 466 [31m----------------[m
 docs/WasteWaterAnalysis2SlideShow.html             | 617 [31m---------------------[m
 docs/presentationPrepList                          |  16 [31m-[m
 13 files changed, 2920 deletions(-)

[33mcommit 37096834bed4319df0422c488aa37a790b49f392[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 05:08:29 2021 -0500

    Prep StackedBarPlot

 Code-Notebooks/RecreateingPlots/StackedBarPlot.Rmd | Bin [31m7807[m -> [32m7662[m bytes
 1 file changed, 0 insertions(+), 0 deletions(-)

[33mcommit 34f9f0dfb16c97ad2c8ff6208ac78f676fb855b8[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 8 04:58:19 2021 -0500

    Some changes

 Code-Notebooks/DataExplanation/OverviewOfData.Rmd  |  58 [32m+++[m
 Code-Notebooks/DataExplanation/OverviewOfData.html | 219 [32m+++++++++[m
 .../LongTermTrendAnalysis/NoTSCorrelation.Rmd      |  56 [32m+[m[31m--[m
 Code-Notebooks/RecreateingPlots/.Rhistory          |   1 [32m+[m
 Code-Notebooks/RecreateingPlots/StackedBarPlot.Rmd | Bin [31m7486[m -> [32m7807[m bytes
 .../VarianceAnalysis/ApproximatingSELogN1.Rmd      |  65 [32m++[m[31m-[m
 .../VarianceAnalysis/ApproximatingSELogN1.nb.html  | 529 [32m+++++++++++++++++++++[m
 .../VarianceAnalysis/HFGTrueVariance.Rmd           |  34 [32m+[m[31m-[m
 .../VarianceAnalysis/HFGVarianceAnalysis.Rmd       |  10 [32m+[m[31m-[m
 Code-Notebooks/VarianceAnalysis/README             |  14 [32m+[m
 lib/WasteWaterDataProccess.R                       |   2 [32m+[m[31m-[m
 11 files changed, 920 insertions(+), 68 deletions(-)

[33mcommit a3e3986ce3ee85f9fe2bb173725a606c4e1ade45[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 1 09:57:30 2021 -0500

    deleated unneeded plots

 .../figure-html/Intercepters-1.png                      | Bin [31m23597[m -> [32m0[m bytes
 .../figure-html/Intercepters-10.png                     | Bin [31m24724[m -> [32m0[m bytes
 .../figure-html/Intercepters-11.png                     | Bin [31m22020[m -> [32m0[m bytes
 .../figure-html/Intercepters-12.png                     | Bin [31m23269[m -> [32m0[m bytes
 .../figure-html/Intercepters-13.png                     | Bin [31m23306[m -> [32m0[m bytes
 .../figure-html/Intercepters-14.png                     | Bin [31m23236[m -> [32m0[m bytes
 .../figure-html/Intercepters-15.png                     | Bin [31m24411[m -> [32m0[m bytes
 .../figure-html/Intercepters-16.png                     | Bin [31m21376[m -> [32m0[m bytes
 .../figure-html/Intercepters-17.png                     | Bin [31m21163[m -> [32m0[m bytes
 .../figure-html/Intercepters-18.png                     | Bin [31m23641[m -> [32m0[m bytes
 .../figure-html/Intercepters-19.png                     | Bin [31m22357[m -> [32m0[m bytes
 .../figure-html/Intercepters-2.png                      | Bin [31m23610[m -> [32m0[m bytes
 .../figure-html/Intercepters-20.png                     | Bin [31m23628[m -> [32m0[m bytes
 .../figure-html/Intercepters-21.png                     | Bin [31m23027[m -> [32m0[m bytes
 .../figure-html/Intercepters-22.png                     | Bin [31m23536[m -> [32m0[m bytes
 .../figure-html/Intercepters-23.png                     | Bin [31m22099[m -> [32m0[m bytes
 .../figure-html/Intercepters-24.png                     | Bin [31m23321[m -> [32m0[m bytes
 .../figure-html/Intercepters-25.png                     | Bin [31m24228[m -> [32m0[m bytes
 .../figure-html/Intercepters-26.png                     | Bin [31m22869[m -> [32m0[m bytes
 .../figure-html/Intercepters-27.png                     | Bin [31m23902[m -> [32m0[m bytes
 .../figure-html/Intercepters-28.png                     | Bin [31m23983[m -> [32m0[m bytes
 .../figure-html/Intercepters-29.png                     | Bin [31m22264[m -> [32m0[m bytes
 .../figure-html/Intercepters-3.png                      | Bin [31m23093[m -> [32m0[m bytes
 .../figure-html/Intercepters-30.png                     | Bin [31m22633[m -> [32m0[m bytes
 .../figure-html/Intercepters-31.png                     | Bin [31m21149[m -> [32m0[m bytes
 .../figure-html/Intercepters-32.png                     | Bin [31m20947[m -> [32m0[m bytes
 .../figure-html/Intercepters-33.png                     | Bin [31m23421[m -> [32m0[m bytes
 .../figure-html/Intercepters-34.png                     | Bin [31m22152[m -> [32m0[m bytes
 .../figure-html/Intercepters-35.png                     | Bin [31m23342[m -> [32m0[m bytes
 .../figure-html/Intercepters-36.png                     | Bin [31m22822[m -> [32m0[m bytes
 .../figure-html/Intercepters-37.png                     | Bin [31m23393[m -> [32m0[m bytes
 .../figure-html/Intercepters-38.png                     | Bin [31m21845[m -> [32m0[m bytes
 .../figure-html/Intercepters-39.png                     | Bin [31m23190[m -> [32m0[m bytes
 .../figure-html/Intercepters-4.png                      | Bin [31m22956[m -> [32m0[m bytes
 .../figure-html/Intercepters-40.png                     | Bin [31m24021[m -> [32m0[m bytes
 .../figure-html/Intercepters-41.png                     | Bin [31m22647[m -> [32m0[m bytes
 .../figure-html/Intercepters-42.png                     | Bin [31m23698[m -> [32m0[m bytes
 .../figure-html/Intercepters-43.png                     | Bin [31m23737[m -> [32m0[m bytes
 .../figure-html/Intercepters-44.png                     | Bin [31m22122[m -> [32m0[m bytes
 .../figure-html/Intercepters-45.png                     | Bin [31m22431[m -> [32m0[m bytes
 .../figure-html/Intercepters-5.png                      | Bin [31m23611[m -> [32m0[m bytes
 .../figure-html/Intercepters-6.png                      | Bin [31m24790[m -> [32m0[m bytes
 .../figure-html/Intercepters-7.png                      | Bin [31m24148[m -> [32m0[m bytes
 .../figure-html/Intercepters-8.png                      | Bin [31m22458[m -> [32m0[m bytes
 .../figure-html/Intercepters-9.png                      | Bin [31m24145[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-1.png                | Bin [31m22798[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-2.png                | Bin [31m22224[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-3.png                | Bin [31m22606[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-4.png                | Bin [31m27912[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-5.png                | Bin [31m26579[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-6.png                | Bin [31m27949[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-7.png                | Bin [31m27640[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-8.png                | Bin [31m26597[m -> [32m0[m bytes
 .../figure-html/LM binning heatmap-9.png                | Bin [31m27671[m -> [32m0[m bytes
 54 files changed, 0 insertions(+), 0 deletions(-)

[33mcommit dae28d95c544ab4a63efb708552d37cb49f102d4[m
Author: MarlinRLee <Marlinrlee0@gmail.com>
Date:   Fri Oct 1 09:56:21 2021 -0500

    structure change and archive progress

 .../{CaseDataComp.Rmd => CaseDataCompare.Rmd}      |   0
 ...GUpdatedData.Rmd => HFGParseingExploration.Rmd} |   0
 ...AndAnalysis.Rmd => LIMSParseingExploration.Rmd} |  32 [32m++++[m[31m---[m
 ...aordMain.R => ShinyCovidDashboardExploration.R} |   0
 .../LongTermTrendAnalysis/NoTSCorrelation.Rmd      |   7 [32m+[m[31m-[m
 .../figure-html/Intercepters-1.png                 | Bin [31m0[m -> [32m23597[m bytes
 .../figure-html/Intercepters-10.png                | Bin [31m0[m -> [32m24724[m bytes
 .../figure-html/Intercepters-11.png                | Bin [31m0[m -> [32m22020[m bytes
 .../figure-html/Intercepters-12.png                | Bin [31m0[m -> [32m23269[m bytes
 .../figure-html/Intercepters-13.png                | Bin [31m0[m -> [32m23306[m bytes
 .../figure-html/Intercepters-14.png                | Bin [31m0[m -> [32m23236[m bytes
 .../figure-html/Intercepters-15.png                | Bin [31m0[m -> [32m24411[m bytes
 .../figure-html/Intercepters-16.png                | Bin [31m0[m -> [32m21376[m bytes
 .../figure-html/Intercepters-17.png                | Bin [31m0[m -> [32m21163[m bytes
 .../figure-html/Intercepters-18.png                | Bin [31m0[m -> [32m23641[m bytes
 .../figure-html/Intercepters-19.png                | Bin [31m0[m -> [32m22357[m bytes
 .../figure-html/Intercepters-2.png                 | Bin [31m0[m -> [32m23610[m bytes
 .../figure-html/Intercepters-20.png                | Bin [31m0[m -> [32m23628[m bytes
 .../figure-html/Intercepters-21.png                | Bin [31m0[m -> [32m23027[m bytes
 .../figure-html/Intercepters-22.png                | Bin [31m0[m -> [32m23536[m bytes
 .../figure-html/Intercepters-23.png                | Bin [31m0[m -> [32m22099[m bytes
 .../figure-html/Intercepters-24.png                | Bin [31m0[m -> [32m23321[m bytes
 .../figure-html/Intercepters-25.png                | Bin [31m0[m -> [32m24228[m bytes
 .../figure-html/Intercepters-26.png                | Bin [31m0[m -> [32m22869[m bytes
 .../figure-html/Intercepters-27.png                | Bin [31m0[m -> [32m23902[m bytes
 .../figure-html/Intercepters-28.png                | Bin [31m0[m -> [32m23983[m bytes
 .../figure-html/Intercepters-29.png                | Bin [31m0[m -> [32m22264[m bytes
 .../figure-html/Intercepters-3.png                 | Bin [31m0[m -> [32m23093[m bytes
 .../figure-html/Intercepters-30.png                | Bin [31m0[m -> [32m22633[m bytes
 .../figure-html/Intercepters-31.png                | Bin [31m0[m -> [32m21149[m bytes
 .../figure-html/Intercepters-32.png                | Bin [31m0[m -> [32m20947[m bytes
 .../figure-html/Intercepters-33.png                | Bin [31m0[m -> [32m23421[m bytes
 .../figure-html/Intercepters-34.png                | Bin [31m0[m -> [32m22152[m bytes
 .../figure-html/Intercepters-35.png                | Bin [31m0[m -> [32m23342[m bytes
 .../figure-html/Intercepters-36.png                | Bin [31m0[m -> [32m22822[m bytes
 .../figure-html/Intercepters-37.png                | Bin [31m0[m -> [32m23393[m bytes
 .../figure-html/Intercepters-38.png                | Bin [31m0[m -> [32m21845[m bytes
 .../figure-html/Intercepters-39.png                | Bin [31m0[m -> [32m23190[m bytes
 .../figure-html/Intercepters-4.png                 | Bin [31m0[m -> [32m22956[m bytes
 .../figure-html/Intercepters-40.png                | Bin [31m0[m -> [32m24021[m bytes
 .../figure-html/Intercepters-41.png                | Bin [31m0[m -> [32m22647[m bytes
 .../figure-html/Intercepters-42.png                | Bin [31m0[m -> [32m23698[m bytes
 .../figure-html/Intercepters-43.png                | Bin [31m0[m -> [32m23737[m bytes
 .../figure-html/Intercepters-44.png                | Bin [31m0[m -> [32m22122[m bytes
 .../figure-html/Intercepters-45