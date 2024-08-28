## Changelog

### 0.2.4.1

- fix for `RIgetfit()` and `RIgetResidCor()` to detect when simulated response data contains less than 5 positive responses for an item (which makes `eRm::RM()` not work). This is typically only relevant for very skewed data or difficult items.

### 0.2.4

- `RIgetResidCor()` and `RIgetfit()` now replicate the sample theta distribution accurately using resampling with replacement (parametric bootstrapping based on estimated sample thetas/item thresholds).
- `RIgetResidCor()` and `RIgetfit()` will now omit simulated datasets with empty cells (zero responses in response categories that should have responses).
- `RIgetResidCor()` now automatically chooses PCM or RM depending on data (`model` option removed)
- `RIscoreSE()` has support for dichotomomous data and automatically chooses PCM or RM depending on data 
- `RItileplot()` has a new option for setting `text_color`.

### 0.2.3.1

- `RIitemfit()` should finally calculate misfit correctly so that the sorting works
- Changed `RIgetfit()` model estimation function to use `psychotools::PCModel.fit()` to speed up simulations slightly for polytomous models. Like the `eRm::PCM()` function, this also uses Conditional Maximum Likelihood, and produces identical results with `iarm::out_infit()`.

### 0.2.3

- `RIgetfit()` now retains the variable/item names from the data.
- `RIitemfit()` now uses conditional highlighting with individual cutoff values for each item.
- `RIitemfit()` outputs two new variables indicating the differences between observed infit/outfit and cutoff threshold values.
- `RIitemfit()` optionally sorts the table output based on misfit per item, using either "infit" or "outfit".
- `RIgetfitPlot()`optionally shows observed item fit in the plot. Ideally, 95% CI would be shown, but the SE output from iarm::out_infit is not reliable according to the author (Müller, 2020), and iarm::boot_fit() does not output SE, only p-values.

### 0.2.2.1

- Changed simulation based cutoff thresholds used by `RIitemfit()` to be `quantile(fitmetric, .995)` and .005 instead of .99 and .01 in previous version, to be consistent with `RIgetResidCor()` which uses the one-sided `quantile(fitmetric, .99)`.

### 0.2.2

- New `RIitemfit()` function, which replaces both `RIitemfitPCM()` and `RIitemfitRM()`
  - Only outputs conditional MSQ (ZSTD irrelevant for conditional item fit)
  - Automatically uses RM or PCM depending on data structure.
  - Optional conditional highlighting of simulation based cutoff values, and includes the cutoff intervals when using `output = "table"` (default).
- Modified `RIgetfit()` to only use conditional MSQ when running simulations.
  - Automatically uses RM or PCM depending on data structure.
- Modified `RIgetfitPlot()` accordingly.
- Removed `RIgetfitTable()` and `RIgetfitLoHi()` since this information now is included in the output from the new function `RIitemfit()`

### 0.2.1.1

- Added 1st and 99th percentiles (upper/lower limits) for simulation based item fit metrics from `RIgetfitTable()`
- `RIitemfitPCM()` and `RIgetfitLoHi()` now use 1st/99th percentile values from simulation as cutoffs.
- `RIgetfitPlot()` now uses these options (see `?ggdist::stat_dotsinterval` for details) for rendering the distribution of simulated+estimated item fit metrics

```
stat_dotsinterval(quantiles = iterations, point_interval = median_qi,
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.99)
```

### 0.2.1

- Dichotomous data now working with `RIgetfit()` and `RIgetResidCor()`
  - no integration with `RIitemfitRM()` yet.
- Renamed option `method` to `model` for `RIgetfit()` and `RIgetResidCor()` for consistency across functions.

### 0.2.0

Implemented two simulation functions to get cutoff values for item fit and 
residual correlations (Yen's Q3). For now, these only work with polytomous (PCM)
data.

As always, documentation is available by using `?function` (without the parentheses otherwise usually included).

New functions, and brief descriptions:

- `RIgetfit()` - Get simulation based cutoff values for MSQ and ZSTD.
  - `RIgetfitTable()` - Summarises simulation based cutoff values for each item.
  - `RIgetfitPlot()` - Plot (one at a time)
- `RIgetResidCor()` - Get simulation based cutoff values for Yen's Q3 residual correlations
  - Based on Christensen et al. (2017, DOI: 10.1177/0146621616677520).
  - Uses your dataset to get appropriate cutoff values for use with `RIresidcorr()`
  
Changes:

- `RIitemfitPCM()` now has two new options:
  - `simcut` Set to TRUE if you want to use simulation based cutoff values
  - `gf` The output object from `RIgetfit()` is needed when `simcut = TRUE`
  - example command: `RIitemfitPCMtest(df, simcut = TRUE, gf = getfit)`


### 0.1.50.7

- added check whether data is dichotomous for several functions to produce a more helpful error message.
- harmonized functions to make all functions use `model = "RM"` where applicable (replacing `dich = TRUE`).
- bug fix `RIloadLoc()`.
- bug fix `RIestThetas()`.

### 0.1.50.3

- minor bug fixes.
- `RIscoreSE()` relies on `iarm::person_estimates()`, which has a minor bug when setting the value of the highest MLE score, where the range is restricted to theta [-6,6]. I have temporarily added the two necessary functions from `iarm` to the `RISEkbmRasch` package to correct this.

### 0.1.50.0

Breaking change:

- `RIestThetas()` and the multicore version `RIestThetas2()` have now been renamed to `RIestThetasOLD()`
  - the new `RIestThetas()` now uses `iarm::person_estimates()` for much faster processing and easier implementation of both polytomous and dichotomous data.
  - the new `RIestThetas()` also outputs measurement error (SEM) automatically.
  - the OLD functions have been updated to use the non-shifted item threshold values.

Significant changes:

- `RItargeting()` will now show correct (unshifted) item threshold values, and person locations (thetas) are estimated using `iarm::person_estimates()` with Weighted Likelihood Estimation for less bias.
- `RItif()` bug fix for dichotomous data, now using all item locations. Also updated to use WLS theta estimation for caption text for both polytomous and dichotomous data. Option `samplePSI` still uses MLE theta.
- These functions will also show unshifted item threshold values:
  - `RIloadLoc()`
  - `RIitemparams()`
  - `RIitemHierarchy()`, which also has updated defaults to shows 84% CI to enable visual interpretation of statistically significant differences (Payton et al., 2003) between item thresholds. You can change the CI, but the caption text is not yet dynamically updated to match the setting (sorry).
- `RIscoreSE()` now uses `iarm::person_estimates()` for WLE estimation of person locations and SEM


### 0.1.40.1

- `RItileplot()` now has an option to display percentage of responses for each item, instead of number of responses, `percent = TRUE`.

### 0.1.40.0

Major changes:

- added dependency on packages `iarm` and `doParallel`.
- `RIitemfitPCM()` no longer silently removes respondents with missing data when running multiple subsamples for ZSTD estimation.
- `RIitemfitPCM()` now has a footer in the table output, indicating the sample size, also sample size used for subsampling ZSTD and number of samples.
- `RIitemfitPCM()` now uses conditional estimates for MSQ values by default, which introduces a dependency on the package `iarm`. The "old" unconditional estimation method (using `eRm`) is available using the option `output = "unconditional"` (only recommended for reproducibility for old analyses).
  - This decision is based on the paper Müller, M. (2020). Item fit statistics for Rasch analysis: Can we trust them? Journal of Statistical Distributions and Applications, 7(1), 5. https://doi.org/10.1186/s40488-020-00108-7
- `RIitemfitPCM2()`, which uses multi-core processing for ZSTD estimation, is also updated with conditional estimation as the default option.
- `RIitemfitRM()` does the same for dichotomous items, now also defaulting to conditional estimation.

Minor changes:

- started to replace `glue()` with `paste0()` to remove one library dependency (when replacement work is complete).
- also moving toward consistency in functions having an option for `output`. Most often this is related to getting a dataframe output instead of a HTML table. I will also add an option to get a `knitr::kable()`` output for all functions where relevant, to enable Quarto to directly format the table. This helps when using different output formats from Quarto (revealjs, etc).

### 0.1.34.1

- Added new default setting to `RItileplot()` to highlight text in cells with less than 10 responses with red color. Highlighting can be turned off, and the cutoff value can be changed.

### 0.1.34

- Added "proportion of variance" to the `RIpcmPCA()` function output.
- Added option to set CI for `RIitemHierarchy()`
  - confidence intervals are now 84% by default to enable visual interpretation of statistically significant differences (see Payton et al., 2003). The CI can be changed using the `sem_multiplier` option.

### 0.1.33.5

- Specified the use of `select()` to be `dplyr::select()` across all functions.

### 0.1.33.4

- Specified the use of `separate()` to be `tidyr::separate()` across all functions to avoid namespace issues (hello `lordif::separate()`). 

### 0.1.33.3

- Sorry for the multitude of minor fixes... I have now implemented a way to automatically set axis limits in figures based on the data in `RIpfit()` figure outputs, similar to what was done in `RItargeting()` recently. This will be implemented in more functions at some point.

### 0.1.33.2

- More fixes for `RIpfit()` and added an option to set cutoff values for infit ZSTD (default is +/- 1.96).

### 0.1.33.1

- Fixed the optional grouping variable for `RIpfit()` and added an option to output a vector of row numbers for respondents with deviant infit ZSTD.

### 0.1.33

- Added check of duplicate theta values in `RIscoreSE()` which gives a warning that you should extend the score_range option. Please also use the same range when estimating theta values with `RIestThetas()`. 
- Added checks for `RItargeting()` and auto-adjustment of xlim range if person/item locations are outside the (default) xlim values
- *NOTE:* these changes have not been extensively checked yet, please email me at <magnus.p.johansson@ri.se> if you encounter any issues.

### 0.1.32.1

- Added some flexibility to `RItif()` to make optional cutoff settings displayed in figure, with or without sample PSI.

### 0.1.32.0

- Fix for how `RItif()` retrieves max/min theta values of the chosen TIF cutoff
- `RItif()` added PSI values in figure and hopefully improved interpretability of caption text.
- `RItif()` now has new option for dichotomous data, `dich = TRUE`, although it seems to produce the same output as the default partial credit model.

### 0.1.31.0

- Fix for all 4 DIF-functions using the `LRtest()` function from `eRm` to make item threshold locations correct.

### 0.1.30.4

- `RItif()` draws an orange dashed horizontal line for the cutoff (optionally) chosen.

### 0.1.30.3

- Fix for `RIitemparams()` to get relative_highest_tloc values for all items even when there are different number of thresholds (added `na.rm = TRUE` to `max()` function call).

### 0.1.30.2

- Fix for `kbl()` tables to accomodate changes in Quarto 1.3+ and get desired rendering.
- Added `tbl_width` option for functions rendering tables (item fit, residual correlations, item parameters, etc)

### 0.1.30.1

- `RIitemhierarchy()` new option `numbers = FALSE` to remove numbers from the plot.

### 0.1.30.0

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
  
Minor fixes:

- `RIestThetas()` and the parallel processing version `RIestThetas2()` now use `as.data.frame()` instead of `as_tibble()` to avoid a warning message.
- The x axis of all figures showing the logit scale should now have a consistent label ("Location (logit scale)")
- Begun some work to clean up the code and make it more consistent, primarily ggplot labels using `labs()`.


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
