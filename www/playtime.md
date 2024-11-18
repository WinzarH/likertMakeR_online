
### Things to try

#### Experiment with 'Variance' in the correlation matrix

_Chronbach's Alpha_ is a function of the mean of the paired correlations of a set of variables. 

The _variance_ parameter adjusts the variation around the mean while giving the desired _Alpha_ value. 

 - Setting the _variance_ at '0' produces a correlation matrix with all off-diagonal values at the desired mean correlation. 
 
 - Setting _variance_ at '0.25' produces a matrix with a moderate amount of variation around the desired mean - what we might expect with a well-established scale where items are of similar value contributing to the scale.
 
 - The default value of '0.50' produces a matrix with a range of correlation values typically ranging from high to near zero, or even a few negative values. 
 
 - Setting _variance_ at '1.0' produces a matrix with a wide variation around the desired mean - typically with eigenvalues that would suggest a multidimensional scale. 
 
 - Setting _variance_ around '2.0', or above, risks creating a matrix that is non-positive-definite, and so not feasible. 
 
#### Experiment with different _Alpha_ values



_Cronbac's Alpha_ is a function of:

 - _k_, the number of items used to make the summated (or average) scale
 
 - _r&#x305;_, the mean of all pairwise correlations of the _k_ items
 
and is given by the formula:

<img title = "Cronbach's Alpha Formula" alt = "Alpha is a function of the average paired correlations and the number of items" src="alpha.svg">


<!---
$$ 
\alpha = {{k * \bar{r} } \over {1 + (k - 1) * \bar{r} }} 
$$
--->
<br>
<br>

See how different values for _Alpha_ affect higher and lower values in the correlation matrix.

 - See also how different numbers of items in the scale (columns and rows in the matrix) affect higher and lower values in the matrix at diffrent _Alpha_ values.

 - Note that fewer items require higher correlations to achieve the required _Cronbach's Alpha_. 
 
<!---
 ![mean r to alpha relationship](meanr2alpha_label.png)
 --->
 
##### Relationship between _&alpha;_ and _r&#x305;_ at different number of items, _k_.
 
 <figure>
    <img src="meanr2alpha_label.png"
         alt="mean r to alpha relationship">
    <figcaption>Mean correlation must must increase when the scale has fewer items.</figcaption>
</figure>

---- 

