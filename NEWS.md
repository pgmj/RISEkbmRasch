## Changelog

### 0.1.30.1

- `RIitemhierarchy()` new option `numbers = FALSE` to remove numbers from the plot.

### 0.1.30.0

Minor fixes:

- `RIestThetas()` and the parallel processing version `RIestThetas2()` now use `as.data.frame()` instead of `as_tibble()` to avoid a warning message.
- The x axis of all figures showing the logit scale should now have a consistent label ("Location (logit scale)")
- Begun some work to clean up the code and make it more consistent, primarily ggplot labels using `labs()`.

Major updates:

#### `RItargeting()`

- allows choice of bin size for person location histogram, default is 30 - `RItargeting(data, bins = 30)`
- now uses `library(patchwork)` instead of `cowplot` to combine figures.
- can now output a list object with the three parts of the targeting figure, which allows for customization of themes/colors/fonts, etc. The three parts are:
  - `p1` - Person location histogram
  - `p2` - Item threshold histogram
  - `p3` - Individual item thresholds
  - If you want to use this, you should save the output to a named object, `targeting <- RItargeting(data)`, and then access the parts using `targeting$p1`, `targeting$p2`, and `targeting$p3`. You can then combine the figures, e.g. `targeting$p1 / targeting$p2 / targeting$p3 + plot_layout(heights = c(1,1,1.4))`.
- now uses the viridis H color palette by default.

#### `RIitemhierarchy()`

-  has a lot more information in the plot now! Could get messy if you have a lot of thresholds. This is documented in the caption text. It makes the plot contain a lot of key information that we recommend to include in published psychometrics. This includes:
    - Values for item threshold and average item locations.
    - The mean of the item thresholds.
    - The relative distance from the mean for each item.
- now uses the viridis H color palette by default.

#### `RIitemparams()` 

- largely reworked and has new options available.
  - it does not by default write a CSV file any more, you need to specify `output = "file"`
  - other output options include "table" (default) and "dataframe".
  - you can choose level of detail in the output, using `detail = "thresholds"` or `output = "all"`. This applies to any type of output.
  - `detail = "all"` adds information about avg/max/min values relative to the mean item location

### 0.1.20.1

- `RIscoreSE()` fix. Should now work with any number of item thresholds.

### 0.1.20.0

- `RItif()` added option to choose TIF cutoff value for which to generate caption text 
- `RIestTheta()` now accepts optional specification of range of theta values
- simulation functions added, for polytomous data only
- `RIscoreSE()` will now use WL estimation (Warm, 1989), and produces a full range of values using simulated data
  - `RIscoreSE()` can optionally output a figure showing ordinal sum score vs logit score

### 0.1.16.4

- `RIitemHierarchy)` has a new coloring scheme and wider `geom_errorbar()` to make it easier to interpret when there are overlapping confidence intervals.
- `RImissingP()` only indicates integers on x axis labels

#### Note on DIF

- The previously found bug in the `psychotools` package when mixing dichotomous and polytomous items has been [fixed in version 0.7-3](https://cran.rstudio.com/web/packages/psychotools/NEWS), so make sure to upgrade that package. This means that the "old" DIF-functions should be used primarily.
- The LR-based DIF-functions (described below) seem to produce inflated DIF sizes and should probably only be used with careful interpretation by those who fully understand the eRm package's `LRtest()` output.

### 0.1.16

- `RImissingP()` now has option `n = 10` as a default, to limit/choose how many participants to display.
- Multiple new DIF functions added, based on the eRm package's `LRtest()` function. The new functions also show standard errors/95% confidence intervals.
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
