---
title: "Step 2: Generate dataframe"
---  
  
<!-- #### 2: Generate dataframe  -->

#### Options:

##### (1) Edit the online table

Required parameters are:

  - **_n_**: number of observations.  Acceptable range is '4' to '1024'
  
  - **_scale_**: scale name 

  - **_mean_**: mean value of each variable. Default = '3'. 

  - **_sd_**: standard deviation value of each variable. Default = '1'. 
  
   - **_lowerbound_**: a value for the lower bound of each variable _(e.g., '1' in a 1&nbsp;to&nbsp;5 rating scale)_. Default = '1'.

  - **_upperbound_**: a value for the upper bound of each variable _(e.g., '7' in a 1&nbsp;to&nbsp;7 rating scale)_. Default = '5'.
  
  - **_nItems_**: number of items in the scale. 
  
     * set at `1` if you are creating individual items (questions)

Default, starter, values are given in the input array.


##### (2) Upload parameters in a CSV file

A simple CSV file with scale-name, mean, sd, lower-bound, upper-bound and number-of-items.

A **template** like this is downloadable.

    "scale","mean","sd","lower","upper","nItems"
    "Scale01", 3, 1, 1, 5, 1
    "Scale02", 3, 1, 1, 5, 1
    "Scale03", 3, 1, 1, 5, 1
    "Scale04", 3, 1, 1, 5, 1

alternative template

```
"scale","mean","sd","lower","upper","nItems"
"Scale01", 3, 1, 1, 5, 1
"Scale02", 3, 1, 1, 5, 1
"Scale03", 3, 1, 1, 5, 1
"Scale04", 3, 1, 1, 5, 1
```




#### Citations

  - Milojev, P., D. Osborne, L.M. Greaves, F.K. Barlow, C.G. Sibley, (2013),
"The Mini-IPIP6: Tiny yet highly stable markers of Big Six personality",
_Journal of Research in Personality_, 47/6:936-944. 
<https://doi.org/10.1016/j.jrp.2013.09.004>

  - Robinson M.A., (2018) 
  "Using multi-item psychometric scales for research and practice in human resource management". 
  _Human Resources Management_; 57:739–750. <https://doi.org/10.1002/hrm.21852> 
  
  
  
