---
title: "ffrmv"
author: "Rubén Martín Vázquez"
date: "25 junio, 2017"
output: rmarkdown::html_document
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


# Vignette using knitr and R Markdown  ![](https://camo.githubusercontent.com/5134d05f6dd511adb7fc9847532e8a7b4704a04e/68747470733a2f2f7472617669732d63692e6f72672f6d6963726f707974686f6e2f6d6963726f707974686f6e2e706e673f6272616e63683d6d6173746572)

## Description


This file has been created for the [Coursera](www.coursera.org)	 *Building R Packages* course.

This package contains 5 functions:  
- __fars_read.__ Loads a CSV file with FARS data.  
- __make_filename.__ Creates a filename for a given year.  
- __fars_read_years.__ Processes data from the wanted years.  
- __fars_summarize_years.__ Summarizes number of accidents per month and year.  
- __fars_map_state.__ Plots the results on a map.  

## Installation
```{r eval=FALSE}
devtools::install_github("rmartinvazquez/Building-R-Packages")
```

## Using the package
```{r eval=FALSE}
library(ffrmv)
```





