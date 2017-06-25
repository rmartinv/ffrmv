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


# Vignette using knitr and R Markdown [![Build Status](https://api.travis-ci.org/rmartinvazquez/ffrmv.svg?branch=master)](https://travis-ci.org/rmartinvazquez/ffrmv)

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





