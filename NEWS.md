## Changelog

### Note on DIF

- The previously found bug in the `psychotools` package when mixing dichotomous and polytomous items has been [fixed in version 0.7-3](https://cran.rstudio.com/web/packages/psychotools/NEWS), so make sure to upgrade that package. This means that the "old" DIF-functions should be used primarily.
- The LR-based DIF-functions (described below) seem to produce inflated DIF sizes and should probably only be used with careful interpretation by those who fully understand the eRm package's `LRtest()` output.

### 0.1.16

- `RImissingP()` now has option `n = 10` as a default, to limit/choose how many participants to display.
- Multiple new DIF functions added, based on the eRm package's `LRtest()` function. These are also useful if encountering the issue found with `psychotree` when mixing dichotomous and polytomous items in the data. However, it seems like LRtest only works with two groups, which clearly is a limitation. The new functions also show standard errors/95% confidence intervals.
  - `RIdifTableLR()` for average item locations table (sortable)
  - `RIdifThreshTblLR()` for item threshold locations table
  - `RIdifFigureLR()` for average item locations figure
  - `RIdifThreshFigLR()` for item threshold locations figure

### 0.1.15

- `RImissingP()` for analysis of missing items per respondent added

### 0.1.14

- added grouping function for `RIpfit()` and changed default setting to binned hex heatmap to better show distribution of respondents.
- `RIdifTable()` can now optionally output a dataframe 
- removed `RIrespCats()` since it was not working and wasn't going to be fixed. A new RIrespCats function is forthcoming, but `RIitemCats()`, which uses eRm, works for now.

### 0.1.13

New option:

- `RItif(samplePSI = TRUE)` is now available if you want information about your sample added to the TIF curve.

Bug found:

- `RIdifThresh()` does not seem to produce stable output when items have different numbers of response categories. This seems to be related to the underlying function `threshpar()`. Further investigations are coming.

### 0.1.12

New functions:

- `RImissing()` to generate a figure that shows missing data per item, arranged by amount of missing data.
- `RIdifTable2()` enables DIF interaction analysis between two variables, using the [`psychotree`](https://cran.r-project.org/web/packages/psychotree/index.html) package.

Fixes:

- Changes to use [`dplyr`](https://cran.r-project.org/web/packages/dplyr/index.html) 1.1.0 syntax for `mutate(across(), ~ function(.x))`, mostly used for rounding numbers in tables/dataframes.
- `RItargeting(data, dich = TRUE)` - bug fix when analyzing dichotomous data - now the first item is also included in item threshold sections.

### 0.1.8.4

New functions:

- `RIestThetas()` and `RIestThetas2()` for estimating person locations/scores using the [`catR`](https://cran.r-project.org/web/packages/catR/index.html) package. See [example use in vignette]( https://pgmj.github.io/raschrvignette/RaschRvign.html#estimating-interval-level-person-scores).
