<div align="center">
    <img src="../../docs/images/covid-droplet.svg" alt="Logo" style="width:200px">
</div>

# Getting Started

## Overview

This package is intended to make the process of analyzing epidemiological wastewater data easier and more insightful. It’s intended primarily for researchers and epidemiologists but could potentially be used by anyone with an interest in the topic who has a working knowledge of R programming, epidemiology, and statistics.   

Requirements:
- Knowledge of R programming
- Familiarity with epidemiology
- A base understanding of statistics

This package provides a set of core utilities for preparing your data, analyzing your data, and visualizing the results.  

Features:
- Outlier detection
- Various data smoothing techniques
- Normalization techniques
- Time series analysis
- Wastewater / case offset analysis
- Visualization / graphing tools
- Sample data sets courtesy of the Wisconsin Department of Health Services

Note that the package also includes a set of example data.   This was done in order to provide a set of real-world instructive examples which make the code easier to understand and apply to your own data sets.

## What is Wastewater Based Epidemiology?

Epidemiology is the process of investigating and monitoring the prevalence of disease agents within a population or an environment. This has been most commonly performed by collecting case data from hospitals and other public health agencies. Wastewater based epidemiology takes a different approach by looking for disease agents in the population’s wastewater as it is collected in sewage treatment plants.

## Problems with Traditional Case Based Epidemiology

During the Covid-19 pandemic, many problems associated with the traditional case based epidemiology approach became evident.  

- Variations in testing
- Variations in reporting
- Privacy concerns associated with collection on individual health data
- Disease can take time before onset and presentation in a doctor’s office so the technique is not very timely.

## The Process of Wastewater Based Epidemiology
To address these problems, attention has shifted to an alternate / complementary approach - wastewater based epidemiology.   Wastewater epidemiology is conducted using the following process:

- Wastewater samples are periodically collected from sewersheds
- Samples are sent to state laboratories for analysis
- Analysis results are communicated to the state health services agencies, where they are compared with reported case rates

<div align="center">
    <img src="../../docs/images/getting-started/wastewater-process.png" alt="The Wastewater Epidemiology Process" style="width:75%">
    <div>
        <label>The Wastewater Epidemiology Process</label>
    </div>
</div>

## Benefits of Wastewater Based Epidemiology

The advantages of wastewater based epidemiology compared with case based epidemiology are as follows:

- Can provide more timely, almost “real-time” information
- Is well suited for an epidemiological early warning system
- Maintains the anonymity of individuals
- Does not rely upon voluntary testing by individuals so it has the potential to be more consistent.

<div align="center">
    <img src="../../docs/images/getting-started/wastewater-offset.png" alt="Onset of Symptoms and Wastewater Detection" style="width:75%">
    <div>
        <label>Onset of Symptoms and Wastewater Detection</label>
    </div>
</div>

## Challenges Associated with Wastewater Based Epidemiology

Despite the very attractive characteristics and promising results of wastewater based testing, there are also a number of potential challenges associated with this approach which can make it difficult to implement:

- Data is inherently noisy
- There are often significant sampling differences between communities (once per day verses once per week, for example)
- There are often differences in methodology (qpcr verses dpcr etc.)
- There are many cofactors related to wastewater collection and testing which can make interpretation of results difficult.

<div align="center">
    <img src="../../docs/images/getting-started/sample-frequency.png" alt="Differences in Sampling Frequency" style="width:75%">
    <div>
        <label>Differences in Sampling Frequency</label>
    </div>
</div>

## The Role of This Software
Because of these various complicating factors and difficulty in performing wastewater based analysis and interpreting results, software such as this can serve as a valuable aid in making the analysis and interpretation of this data easier and more reliable. 

# Loading and Viewing Data

# Data Preparation

# Data Analysis

# Conclusion

We hope that you have had a successful and enjoyable experience using this software package.   If you would like to share your results and/or feedback with the package authors, contact information is listed below:

- Marlin Lee - (mailto:mrlee6@wisc.edu)
- Kyllan Wunder - (mailto:kwunder@wisc.edu)
- Abe Megahed - (mailto:amegahed@wisc.edu)
You may also submit comments, feedback, feature requests, and bug reports through the GitHub repository at:  https://github.com/UW-Madison-DSI/Covid19Wastewater

# Acknowledgements

This package was made possible through support from the University of Wisconsin Data Science Institute, in collaboration with the Wisconsin Department of Health Services (DHS), and the State Lab of Hygiene (SLH).

