# Meta-research-global-indicators

This repository contains the R scripts used for the analysis of bibliometric data on **medical errors**, categorized by **WHO region**, and their associations with **health indicators from the WHO Global Health Observatory**.

## Purpose
This repository provides a reproducible workflow for the analysis of research on medical errors, integrating bibliometric metrics (publications, citations, H-index) with regional health indicators. The analyses aim to:

- Explore the relationship between **bibliometric metrics** and **health indicators**.
- Evaluate regional differences in research output related to medical errors.
- Perform **linear regression models** for each WHO region.
- Conduct **meta-analysis** to estimate pooled effects across regions.
- Visualize associations using **hierarchical clustering** and **heatmaps**.

## Required R packages
The following R packages are necessary to execute the analyses:
- **MASS**
- **readxl**
- **dplyr**
- **tidyr**
- **cluster**
- **pheatmap**
- **ggplot2**
- **broom**
- **car**
- **tools**
- **metafor**
- **forestplot**
- **knitr**

## Analyses included
This script performs the following analyses:

1. **Data loading and cleaning**  
   - Reading the WHO health indicator datasets (stored in `/indicators/`).  
   - Standardizing indicator formats.

2. **Data summarization**  
   - Aggregating total **publications, citations, and H-index** by WHO region and year.  
   - Computing **average values** for each health indicator per region-year.

3. **Linear regression models**  
   - Evaluating associations between **health indicators** and **bibliometric metrics**.
   - Constructing separate **regional models** for each WHO region.  
   - Reporting only significant models while making all results available.

4. **Hierarchical clustering and heatmaps**  
   - Creating a **regression coefficient matrix** with Z-score normalization.  
   - Performing **clustering analysis** to group related indicators.  
   - Generating **heatmaps** with significance annotations.

5. **Meta-analysis**  
   - Applying **random-effects meta-analysis** to summarize associations across regions.  
   - Estimating pooled regression coefficients with the **Restricted Maximum Likelihood (REML)** method.  

6. **Result extraction and reporting**  
   - Extracting **significant models** based on p-values.  
   - Formatting results for **publication and supplementary materials**.  
   - Storing output tables in structured CSV files.

## How to use
1. **Clone or download this repository.**  
2. **Ensure the WHO health indicators (`/indicators/`) are in place.**  
3. **Open the script (`main_analysis.R`) in RStudio (or another R environment).**  
4. **Install the required R packages** (see above).  
5. **Run the script** to perform the analyses and generate output files (tables, plots, and results).

## Data availability
The repository includes **preprocessed health indicators** from the WHO in the `/indicators/` folder. **No bibliometric data is included in this repository.** The script demonstrates the full workflow using the available WHO indicators.

## Folder structure
The repository is organized as follows:

```
/Meta-research-global-indicators
│── main_analysis.R         # Main R script for all analyses
│── /indicators/            # Health indicator CSV files from WHO
│── README.md               # Documentation
```

## License
This repository is licensed under the **MIT License**, allowing free use, modification, and distribution with attribution. See the `LICENSE` file for more details.
