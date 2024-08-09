

### LikertMakeR online

**_LikertMakeR_** synthesises and correlates Likert-scale and related 
rating-scale data. 
You decide the mean and standard deviation, and the correlations 
among vectors, and the package will generate data with those same predefined 
properties. 

<!---
The program should be useful for teaching in the Social Sciences,
and for scholars who wish to "replicate" rating-scale data for further
analysis and visualisation when only summary statistics have been reported.
-->


#### Generate a dataframe from Cronbach's Alpha and predefined moments

This is a two-step process ... plus validation:
  
  1. generate a correlation matrix from desired alpha and desired dimensions 
  
  2. generate rating-scale items from the correlation matrix and desired item properties
  
  3. validate the data 
  
  
  
##### Step 1: Generate a correlation matrix

Required parameters are:

  -  **_items_**: number items/ columns / variables
  
  - **_alpha_**: a target Cronbach's Alpha. 

  - **_variance_**: amount of variation in the correlations generated 
  (default = '0.5'):  
  '0' causes all correlations to be equal; 
  '1' will make some correlations high and some quite low or negative; 
  '2', or more, risks generating a non-positive-definite matrix
  
#### Step 2: Generate dataframe

Required parameters are:

  - **_n_**: number of observations.  Acceptable range is '4' to '1024'

  - **_mean_**: mean value of each variable. Default = '3'. 

  - **_sd_**: standard deviation value of each variable. Default = '1'. 
  
   - **_lowerbound_**: a value for the lower bound of each variable _(e.g., '1' in a 1&nbsp;to&nbsp;5 rating scale)_. Default = '1'.

  - **_upperbound_**: a value for the upper bound of each variable _(e.g., '7' in a 1&nbsp;to&nbsp;7 rating scale)_. Default = '5'.

Default, starter, values are given in the input array.

#### Step 3: Validate the data

Hit the "Update plot" button to see if data have changed.

