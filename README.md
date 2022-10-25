# RISEkbmRasch
R package for Rasch Measurement Theory based psychometric analysis. Intended for use with [Quarto](https://quarto.org) for documentation and presentation of analysis process and results.

## Installation

First, install the devtools package:
```r
install.packages('devtools')
```

Then install the package: 
```r
devtools::install_github("pgmj/RISEkbmRasch")
```

## Usage

Most functions in this package are relatively simple wrappers that create outputs such as tables and figures to make the Rasch analysis process quick and visual. There is a [companion Quarto template file](https://github.com/pgmj/RISEkbmRasch/tree/main/Quarto) that shows suggested ways to use this package.

There are two basic data structure requirements:

- you need to create a dataframe object named **itemlabels** that consists of two variables
  - the first one named **itemnr**, containing variable names exactly as they are named in the dataframe containing data (for example q1, q2, q3, etc)
  - the second one named **item**, containing either the questionnaire item or a description of it (or description of a task)
- the data you want to analyze needs to be in a dataframe with participants as rows and items as columns/variables
  - the lowest response category needs to be zero (0)
  - during data import, you will need to separate any demographic variables into vectors, for analysis of differential item functioning (DIF), and then remove them from the dataframe with item data - **the dataframe with item data has to only contain item data for the analysis functions to work**
  
If there is too much missingness in your data, some functions will struggle. In the Quarto template file there is a script for choosing how many responses a participant needs to have to be included in the analysis. You can experiment with this if you run in to trouble. Currently, the ```RIloadLoc()```function does not work with any missing data (due to the PCA function), and the workaround for now is to run this command with ```na.omit()``` around the dataframe (ie. ```RIloadLoc(na.omit(df))```.

More details and examples of use will be added. You can find two Quarto use cases with Rasch analyses in the [project subfolder Quarto](https://github.com/pgmj/RISEkbmRasch/tree/main/Quarto). Examples of output from the Quarto files are [also available](https://github.com/pgmj/RISEkbmRasch/tree/main/Quarto/output).

## Author

[Magnus Johansson](https://www.ri.se/en/person/magnus-p-johansson) is a licensed psychologist with a PhD in behavior analysis, working at [RISE Research Institutes of Sweden](https://ri.se/en).
- Twitter: [@pgmjoh](https://twitter.com/pgmjoh)
- ORCID: [0000-0003-1669-592X](https://orcid.org/0000-0003-1669-592X)

## License

This work is licensed under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/).
