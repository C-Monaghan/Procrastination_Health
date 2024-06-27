<h2 align = "center" > Procrastination and preventive health-care in the older U.S. population </h2>
<h4 align = "center"> Cormac Monaghan<sup>1,3</sup>, Dr. Rafael de Andrade Moral<sup>2</sup>, & Dr. Joanna McHugh Power<sup>3</sup> </h4>

<sup>1</sup> Hamilton Institute, Maynooth University  
<sup>2</sup> Department of Mathematics and Statistics, Maynooth University  
<sup>3</sup> Department of Psychology, Maynooth University

> [!Important]
> The Health and Retirement Study (HRS) provides a special file called a cross-wave tracker file to help analyze data across different HRS surveys (called waves). This file is used for the included script, [Processing.R](https://github.com/C-Monaghan/Procrastination_Health/blob/main/02__Models/01__Processing.R). However, the file is quite large (185 MB) and exceeds typical file size limits.
>
> To run the script, download the cross-wave tracker file from the [HRS website](https://hrsdata.isr.umich.edu/data-products/cross-wave-tracker-file) and save it in the following directory with the filename **./01__Data/01__Raw_Data/Tracker.sav**.

## Overview
Protective health management includes behaviours such as engaging in regular physical activity, eating a balanced and nutritious diet, and getting adequate sleep. Additionally, routine health check-ups and screenings, collectively termed preventative health behaviours, are essential for the early detection and treatment of potential health issues. However, such health preventive behaviours often require effort and discipline to adopt and maintain, making them particularly susceptible to procrastination. Previous research has shown that procrastination is associated with less frequent engagement in preventive health behaviours. However, such research has predominately focused on student and younger adult populations, with little attention given to older adults.

As such, this study aimed to explore the associations between procrastination and six preventive health behaviours taken from the 2020 wave of the [Health and Retirement Study](https://hrs.isr.umich.edu/).

## Repository Structure
The repository is organized as follows:
- **00__Documentation:** Contains .pdf files about health protective measures within the HRS.
- **01__Data:** Contains both the raw and processed data files.
- **02__Models:** Contains all the project code.
  - **00__Functions.R:** Contains repeatedly used functions throughout the project.
  - **01__Processing.R:** Code to process the raw data.
  - **02__Exploratory.R:** Code to create exploratory visualisations.
  - **03__GAM.R:** Code to create, process, and visualise GAM models.

## Getting started
To get started with using this repository, follow these steps:
Clone the repository to your local machine using the following command:

```bash
https://github.com/C-Monaghan/Procrastination_Health.git
```

Open the R project

```bash
start Procrastination_Health.Rproj
```

## Contact
If you have any questions or need further assistance, please contact the corresponding author at [cormacmonaghan@protonmail.com](mailto:cormacmonaghan@protonmail.com)
