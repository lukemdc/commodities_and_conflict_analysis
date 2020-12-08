# commodities_and_conflict_analysis
This repository contains the scrips used in an honors thesis project examining the effects of commodity price fluctuations and dependence on probability of violent conflict.

These scripts were used to perform the analysis presented in an honors thesis analyzing the effects of commodity price fluctuations and dependence on the probability of violent conflict. The procedures and findings are detailed in this paper: https://www.academia.edu/39677590/The_Commodities_Conundrum_Applying_Machine_Learning_to_Unravel_the_Effects_of_Commodity_Exports_on_Political_Stability

There are two files in this repository:

UNCOMTRADE_data_scraper.R
This file presents a script written in R 3.2 to scrape commodity price data from the UN COMTRADE web api. This also appends additional datasets used as controls in the analysis.

thesis_analysis.DO
This file contains a STATA script to perform the machine learning analysis and regressions and also generate the visualizations used in the final paper.

The underlying data files are not uploaded because they exceed the file size limit of the free version of this platform or are copyrighted by their originators. Users will need to edit the directory variable in both of the above scrips to point R and Stata to the local location of the data files on their local machine.

