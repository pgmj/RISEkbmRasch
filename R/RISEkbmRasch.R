### RISE KBM Rasch analysis package, https://github.com/pgmj/RISEkbmRasch
### Created by magnus.p.johansson@ri.se ORCID: 0000-0003-1669-592X
### The contents of this file is licensed according to 
### Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/ 

### See .qmd files at https://github.com/pgmj/pgmj for use cases
### Further documentation will be created at some point :)

### set up color palette based on RISE guidelines
RISEprimGreen <- "#009ca6"
RISEprimRed <- "#e83c63"
RISEprimYellow <- "#ffe500"
RISEprimGreenMid <- "#8dc8c7"
RISEprimRedMid <- "#f5a9ab"
RISEprimYellowMid <- "#ffee8d"
RISEprimGreenLight <- "#ebf5f0"
RISEprimRedLight <- "#fde8df"
RISEprimYellowLight <- "#fff7dd"
RISEcompPurple <- "#482d55"
RISEcompGreenDark <- "#0e4e65"
RISEgrey1 <- "#f0f0f0"
RISEgrey2 <- "#c8c8c8"
RISEgrey3 <- "#828282"
RISEgrey4 <- "#555555"

# set some colors used later
cutoff_line <- RISEprimRed
dot_color <- "black"
backg_color <- RISEprimGreenLight

# set fontsize for all tables
r.fontsize <- 15

### first we pre-set our chosen cut-off values for some commonly used indices:
msq_min <- 0.7
msq_max <- 1.3
zstd_min <- -2
zstd_max <- 2
loc_dep <- 0.2 # above average residual correlation
dif_dif <- 0.5 # logits difference between groups in average item location (DIF)

### zstd is inflated with large samples (N > 500). Reduce sample size to jz and 
### run analysis with yz random samples to get average ZSTD
jz = 300 # number to include in dataset
yz = 10 # number of random samples


#' Show items based on itemlabels file
#' 
#' Requires a dataframe with two columns, labeled "itemnr" and "item",
#' containing information on the item numbers (qN) and item content.
#' This dataframe has to be labeled "itemlabels".
#' 
#' Default behavior is to only list items that are in the dataframe.
#' Any items eliminated during analysis process will not be included.
#' 
#' If all items in the original dataset are to be shown, use option
#' "all.items = TRUE".
#' 
#' @param dfin Dataframe with item data only
#' @param all.items Set to TRUE to list all items in 'itemlabels' df
#' @return A table with items used in dataframe
#' @export
RIlistitems <- function(dfin, all.items) {
  if(missing(all.items)) {
    itemlabels %>% 
      filter(itemnr %in% names(dfin)) %>% 
    formattable(align=c("c","l"), list(
      `itemnr` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))),
      table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 75%"')
  } else {
    itemlabels %>% 
      #filter(itemnr %in% names(dfin)) %>% 
      formattable(align=c("c","l"), list(
        `itemnr` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 75%"')
  }
}

#' Show items based on itemlabels file, with coloring options
#' 
#' Requires a dataframe with two columns, labeled "itemnr" and "item",
#' containing information on the item numbers (qN) and item content.
#' This dataframe has to be labeled "itemlabels".
#' 
#' Input a vector of item rows, i.e c(1,3,5) to colorize items 1, 3 and 5.
#' Optionally choose which background color will be used. "Lightblue" is the
#' default. Text will be black, so choose a light color which gives good
#' contrast for readability.
#' 
#' @param items vector of row numbers for items to colorize background
#' @param color color of background ("lightblue" is default)
#' @return A table with items used in dataframe
#' @export
RIcolorlistitems <- function(items, color) {
  if(missing(color)) {
    formattable(itemlabels, align=c("r","l"), list(
      formattable::area(row = items) ~ color_tile("lightblue", "lightpink")),
      table.attr = 'style="font-size: 15px; font-family: Lato"')
  } else {
    formattable(itemlabels, align=c("r","l"), list(
      formattable::area(row = items) ~ color_tile(color, "lightpink")),
      table.attr = 'style="font-size: 15px; font-family: Lato"')
  }
}

#' Create a Guttman-like tileplot/heatmap
#' 
#' Sorts items (y axis) and persons (x axis) according to respective 
#' total score and displays them with a color gradient based on responses
#' 
#' @param dfin Dataframe with item data only
#' @export
RIheatmap <- function(dfin) {
  # extract vectors with person/item id arranged by order of total score
  person.order <- dfin %>% 
    mutate(persontotal = rowSums(.)) %>% 
    rownames_to_column("PersonID") %>% 
    select(PersonID, persontotal) %>% 
    arrange(persontotal) %>% 
    pull(PersonID)
  item.order <- dfin %>% 
    t() %>% as.data.frame() %>% 
    mutate(itemtotal = rowSums(.)) %>% 
    rownames_to_column("ItemID") %>% 
    select(ItemID, itemtotal) %>% 
    arrange(itemtotal) %>% 
    pull(ItemID)
  
  # use order vectors to sort item responses and make tile plot
  dfin %>% 
    #  head(20) %>% 
    rownames_to_column("PersonID") %>%
    mutate(PersonID = factor(PersonID, levels = person.order)) %>% 
    melt(id.vars = "PersonID") %>%
    rename(Item = variable) %>% 
    mutate(Item = factor(Item, levels = item.order)) %>% 
    ggplot(aes(x = PersonID, y = Item, fill = value)) +
    geom_tile() + 
    scale_fill_gradient(low = RISEprimYellowLight, high = RISEprimGreen)
}

#' Create table for demographic variables
#' 
#' Input should be a vector with a demographic variable such as gender or age.
#' @param dif.var A vector with a demographic variable
#' @param label What the variable represents (sex/age/etc)
#' @export
RIdemographics <- function(dif.var, label) {
  dif.var %>% 
    table() %>% 
    as_tibble() %>% 
    mutate('Percent' = (round((100 * n / sum(n)),1))) %>% 
    dplyr::rename(label = '.') %>% 
    kbl(booktabs = T, escape = F, table.attr = "style='width:20%;'") %>%
    # options for HTML output
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  position = "left",
                  full_width = T,
                  font_size = r.fontsize,
                  fixed_thead = T) %>% 
    column_spec(1, bold = T) %>% 
    kable_classic(html_font = "Lato") %>% 
    # latex_options are for PDF output
    kable_styling(latex_options = c("striped","scale_down"))
}

#' Create tile plot for all items, also showing the count of
#' responses in each response category for each item
#' 
#' @param dfin Dataframe with item data only
#' @export
RItileplot <- function(dfin) {
  dfin %>% 
    na.omit() %>% 
    pivot_longer(everything()) %>% 
    dplyr::count(name, value) %>% 
    mutate(name = factor(name, levels = rev(names(dfin)))) %>%
    ggplot(aes(x = value, y = name, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(expression(italic(n)), limits = c(0, NA)) +
    scale_x_continuous("Response category", expand = c(0, 0), breaks = 0:max(dfin, na.rm = T)) + # change breaks to fit number of response categories
    ggtitle("Items") +
    ylab("") +
    theme(axis.text.x = element_text(size = 8)) +
    geom_text(aes(label=n), colour = "orange") 
}

#' Create a stacked bar graph to show response distribution
#' 
#' @param dfin Dataframe with item data only
#' @export
RIbarstack <- function(dfin) {
  dfin %>% 
    na.omit() %>% 
    pivot_longer(everything()) %>% 
    dplyr::count(name, value) %>% 
    mutate(name = factor(name, levels = rev(names(dfin)))) %>%
    ggplot(aes(x = n, y = name, fill = value)) +
    geom_col() +
    scale_fill_viridis_c(expression(italic(n)), limits = c(0, NA)) +
    ggtitle("Item responses") +
    xlab("Number of responses")
}

#' Create a stacked diverging bar graph to show response distribution
#' 
#' @param dfin Dataframe with item data only
#' @export
RIbardiv <- function(dfin) {
  dfin %>% 
    na.omit() %>% 
    pivot_longer(everything()) %>% 
    rename(Item = name,
           Response = value) %>% 
    dplyr::count(Item, Response) %>% 
    group_by(Item) %>% 
    mutate(Percent = (100 * n / sum(n)) %>% round(digits = 1)) %>% 
    pivot_wider(id_cols = Item, names_from = Response, values_from = Percent) %>% 
    relocate('0', .after = Item) %>% 
    likert(horizontal = TRUE, aspect=1.5,
           main="Distribution of responses",
           auto.key=list(space="right", columns=1,
                         reverse=FALSE, padding.text=2))
}

#' Create individual bar plots for all items.
#' 
#' @param dfin Dataframe with item data only
#' @export
RIbarplot <- function(dfin) {
  for (i in 1:ncol(dfin)) {
    barplot(table(dfin[,i]), col="#8dc8c7",
            main=names(dfin[i]),
            ylab="Number of responses", 
            xlab=(itemlabels[i,2]))
  }
}


#' Create table with summarized responses across all items.
#' 
#' @param dfin Dataframe with item data only
#' @param pdf.out Set to TRUE to get PDF-compatible table (kableExtra)
#' @export
RIallresp <- function(dfin, pdf.out) {
  if(missing(pdf.out)) {
    dfin %>% 
      pivot_longer(everything()) %>% 
      dplyr::count(value) %>% 
      mutate(percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      rename('Response category' = 'value', 
             'Number of responses' = 'n',
             'Percent' = 'percent') %>%
      formattable(list(
        `Response category` = formatter("span", style = ~ style(font.weight = "bold"))),
        table.attr = 
                    'class=\"table table-striped\" style="font-size: 15px; 
                  font-family: Lato; width: 50%"')
  } else {
    dfin %>% 
      pivot_longer(everything()) %>% 
      dplyr::count(value) %>% 
      mutate(percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      rename('Response category' = 'value', 
             'Number of responses' = 'n',
             'Percent' = 'percent') %>%
      kbl(booktabs = T, escape = F, table.attr = "style='width:40%;'") %>%
      # options for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = F,
                    font_size = r.fontsize,
                    fixed_thead = T) %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  }
}

#' Running the Rasch PCM model using eRm, and 
#' conducting a PCA of residuals to get eigenvalues.
#' 
#' @param dfin Dataframe with item data only
#' @param no.table Set to TRUE to avoid output of table
#' @export
RIpcmPCA <- function(dfin, no.table) {
  if(missing(no.table)) {
    df.erm<-PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    std.resids <- item.fit$st.res
    # PCA of Rasch residuals
    pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
    # create table with top 5 eigenvalues
    pca$values %>%
      round(2) %>%
      head(5) %>% 
      as_tibble() %>% 
      rename('Eigenvalues' = 'value') %>% 
      kbl(booktabs = T, escape = F, table.attr = "style='width:25%;'") %>%
      # options for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = T,
                    font_size = r.fontsize,
                    fixed_thead = F) %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  } else {
    df.erm<-PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    std.resids <- item.fit$st.res
    # PCA of Rasch residuals
    pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
    # create vector with top 5 eigenvalues
    eigenv <- pca$values %>%
      round(2) %>%
      head(5)
  }
}

#' Running the Rasch model for dichotomous using eRm, and 
#' conducting a PCA of residuals to get eigenvalues.
#' 
#' @param dfin Dataframe with item data only
#' @param no.table Set to TRUE to avoid output of table
#' @export
RIrmPCA <- function(dfin, no.table) {
  if(missing(no.table)) {
    df.erm<-RM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.estimates <- coef(df.erm, "eta") # item coefficients
    item.fit <- eRm::itemfit(person.locations.estimate)
    # item parameter CI's
    item.confint<-confint(df.erm, "eta") # difficulty (not easiness)
    # person parameter CI's
    pp.confint<-confint(person.locations.estimate)
    std.resids <- item.fit$st.res
    # PCA of Rasch residuals
    pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
    # create table with top 5 eigenvalues
    pca$values %>%
      round(2) %>%
      head(5) %>% 
      as_tibble() %>% 
      rename('Eigenvalues' = 'value') %>% 
      kbl(booktabs = T, escape = F, table.attr = "style='width:25%;'") %>%
      # options for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = T,
                    font_size = r.fontsize,
                    fixed_thead = F) %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  } else {
    df.erm<-RM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.estimates <- coef(df.erm, "eta") # item coefficients
    item.fit <- eRm::itemfit(person.locations.estimate)
    # item parameter CI's
    item.confint<-confint(df.erm, "eta") # difficulty (not easiness)
    # person parameter CI's
    pp.confint<-confint(person.locations.estimate)
    std.resids <- item.fit$st.res
    std.resids <- item.fit$st.res
    # PCA of Rasch residuals
    pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
    # create vector with top 5 eigenvalues
    eigenv <- pca$values %>%
      round(2) %>%
      head(5)
  }
}

#' Create a figure with ICC plots for all items (not working yet)
#' 
#' @param dfin Dataframe with item data only
#' @export
RIrespcats <- function(dfin) {
  mirt.rasch <- mirt(dfin, model=1, itemtype='Rasch') # unidimensional Rasch model
  plot(mirt.rasch, type="trace") # create ICC plots for all items
}


#' Individual ICC plots.
#' @param dfin Dataframe with item data only
#' @param items A single item (e.g. "q4"), or a vector with multiple items (e.g. c("q4","q2")).
#' @export
RIitemcats <- function(dfin, items) {
  # individual plots for those two items:
  df.erm<-PCM(dfin) # run PCM, partial credit model
  plotICC(df.erm, xlim = c(-6, 6), # change the theta interval to display
          legpos = FALSE, # change legpos to TRUE if you want the legend displayed 
          ylab = "Probability", xlab = "Person location/ability",
          item.subset = items)
}
#make this escape the "hit return to see next plot"


#' Floor/ceiling effects based on raw data (ordinal scores)
#' 
#' @param dfin Dataframe with item data only
#' @return A barplot with descriptives in footnote
#' @export
RIrawdist <- function(dfin) {
  df.erm<-PCM(dfin) # run PCM model
  # get info on thresholds
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  
  # all items should have lowest category 0, making 0 the lowest total score
  rawMin <- 0 
  
  # get the number of thresholds above 0, to calculate max total raw score
  rawMax <- item_difficulty %>% 
    select(starts_with("Threshold")) %>% 
    pivot_longer(everything()) %>% 
    na.omit() %>% 
    count() %>% 
    pull()
  
  # what is the lowest score in the sample?
  rawMinX <- dfin %>%
    mutate(rowsums = rowSums(.)) %>%
    count(rowsums) %>% 
    arrange(rowsums) %>% 
    head(1) %>% 
    pull(rowsums)
  
  # if lowest participant score is higher than 0, we have no floor effect
  if (rawMinX > 0) {
    rawMinN <- 0
  } else { # if lowest participant score is 0, how many participants have scored 0?
    rawMinN <- dfin %>%
      mutate(rowsums = rowSums(.)) %>%
      count(rowsums) %>% 
      arrange(rowsums) %>% 
      head(1) %>% 
      pull(n) 
  }
  
  # what is the highest score in the sample?
  rawMaxX <- dfin %>%
    mutate(rowsums = rowSums(.)) %>%
    count(rowsums) %>% 
    arrange(desc(rowsums)) %>% 
    head(1) %>% 
    pull(rowsums)
  
  # if highest score is below max rawscore, we have no ceiling effect
  if (rawMaxX < rawMax) {
    rawMaxN <- 0
  } else {
    rawMaxN <- dfin %>%
      mutate(rowsums = rowSums(.)) %>%
      count(rowsums) %>% 
      arrange(desc(rowsums)) %>% 
      head(1) %>% 
      pull(n) 
  }
  # ceiling effect
  ceiling_eff<-round(rawMaxN/nrow(dfin)*100,2)
  # floor effect
  floor_eff<-round(rawMinN/nrow(dfin)*100,2)
  
  # create barplot to show sum score distribution
  dfin %>% 
    mutate('Raw sum score' = rowSums(.)) %>% 
    pull() %>% 
    table() %>%   
    barplot(main = "Distribution of summed ordinal raw scores", 
            ylab = "Number of participants",
            sub = paste0("Min score: ", floor_eff, "% , max score: ", ceiling_eff, "%."),
            xlim = c(0, rawMax),
            space = 0,
            col = RISEprimGreen)
            
}


#' Create table with Rasch PCM model item fit values for each item.
#' 
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce 
#' sample size to jz and run analysis using yz random samples to get average ZSTD
#' If you are using Quarto/Rmarkdown, "cache: yes" will be a useful chunk option to 
#' speed things up. 50 samples seems to give stable output, but 10 is probably
#' sufficient for a quick look at the approximate ZSTD statistics. It is recommended
#' to use sample size 250-500, based on Hagell & Westergren, 2016.
#' 
#' @param dfin Dataframe with item data only
#' @param jz Desired sample size in multisampling
#' @param yz Desired number of samples (recommended range 10-50)
#' @export
RIitemfitPCM <- function(dfin, jz, yz) {
  if(missing(jz)) {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, item.fit$i.outfitZ, item.fit$i.infitZ))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    
    # create table that highlights cutoff values in red
    item.fit.table %>% 
      mutate(across(where(is.numeric), round, 3)) %>% 
      mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                               ifelse(OutfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                             ifelse(InfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                             ifelse(OutfitMSQ > msq_max, "red", "black")))) %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                           ifelse(InfitMSQ > msq_max, "red", "black")))) %>%
      kbl(booktabs = T, escape = F) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = F,
                    font_size = r.fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      #  column_spec(c(2:3), color = "red") %>% 
      #  row_spec(3:5, bold = T, color = "white", background = "lightblue") %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  } else {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    
    # ZSTD multisample
    outfitZ<-c()
    infitZ<-c()
    for (i in 1:yz) {
      df.new <- dfin[sample(1:nrow(dfin), jz), ]
      df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ<-cbind(outfitZ,item.fit.z$i.outfitZ)
      infitZ<-cbind(infitZ,item.fit.z$i.infitZ)
    }
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, rowMeans(outfitZ), rowMeans(infitZ)))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    
    # create table that highlights cutoff values in red
    item.fit.table %>% 
      mutate(across(where(is.numeric), round, 3)) %>% 
      mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                               ifelse(OutfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                             ifelse(InfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                             ifelse(OutfitMSQ > msq_max, "red", "black")))) %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                           ifelse(InfitMSQ > msq_max, "red", "black")))) %>%
      kbl(booktabs = T, escape = F) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = F,
                    font_size = r.fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      #  column_spec(c(2:3), color = "red") %>% 
      #  row_spec(3:5, bold = T, color = "white", background = "lightblue") %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  }
}

#' Create table with Rasch dichotomous model item fit values for each item.
#' 
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce 
#' sample size to jz and run analysis using yz random samples to get average ZSTD
#' If you are using Quarto/Rmarkdown, "cache: yes" will be a useful chunk option to 
#' speed things up. 50 samples seems to give stable output, but 10 is probably
#' sufficient for a quick look at the approximate ZSTD statistics. It is recommended
#' to use sample size 250-500, based on Hagell & Westergren, 2016.
#' 
#' @param dfin Dataframe with item data only
#' @param jz Desired sample size in multisampling
#' @param yz Desired number of samples (recommended range 10-50)
#' @export
RIitemfitRM <- function(dfin, jz, yz) {
  if(missing(jz)) {
    df.erm<-RM(dfin) # run Rasch model
    # get estimates
    item.estimates <- coef(df.erm, "eta") # item coefficients
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, item.fit$i.outfitZ, item.fit$i.infitZ))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    
    # create table that highlights cutoff values in red
    item.fit.table %>% 
      mutate(across(where(is.numeric), round, 3)) %>% 
      mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                               ifelse(OutfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                             ifelse(InfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                             ifelse(OutfitMSQ > msq_max, "red", "black")))) %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                           ifelse(InfitMSQ > msq_max, "red", "black")))) %>%
      kbl(booktabs = T, escape = F) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = F,
                    font_size = r.fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      #  column_spec(c(2:3), color = "red") %>% 
      #  row_spec(3:5, bold = T, color = "white", background = "lightblue") %>% 
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  } else {
    df.erm<-RM(dfin) # run Rasch model
    # get estimates
    item.estimates <- coef(df.erm, "eta") # item coefficients
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    
    # ZSTD multisample
    outfitZ<-c()
    infitZ<-c()
    for (i in 1:yz) {
      df.new <- dfin[sample(1:nrow(dfin), jz), ]
      df.new <- na.omit(df.new)
      df.z <- RM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ<-cbind(outfitZ,item.fit.z$i.outfitZ)
      infitZ<-cbind(infitZ,item.fit.z$i.infitZ)
    }
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, rowMeans(outfitZ), rowMeans(infitZ)))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    
    # create table that highlights cutoff values in red
    item.fit.table %>% 
      mutate(across(where(is.numeric), round, 3)) %>% 
      mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                               ifelse(OutfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                             ifelse(InfitZSTD > zstd_max, "red", "black")))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                             ifelse(OutfitMSQ > msq_max, "red", "black")))) %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                           ifelse(InfitMSQ > msq_max, "red", "black")))) %>%
      kbl(booktabs = T, escape = F) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"), 
                    position = "left",
                    full_width = F,
                    font_size = r.fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>% 
      kable_classic(html_font = "Lato") %>% 
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
  }
}


#' Correlation matrix of Rasch residuals
#' 
#' Mandatory option to set relative cutoff-value over 
#' the average of all item residual correlations (usually 0.2-0.3).
#' 
#' @param dfin Dataframe with item data only
#' @param cutoff Relative value above the average of all item residual correlations
#' @export
RIresidcorr <- function(dfin, cutoff) {
  
  sink(nullfile()) # suppress output from the rows below
  
  mirt.rasch <- mirt(dfin, model=1, itemtype='Rasch') # unidimensional Rasch model
  resid=residuals(mirt.rasch, type="Q3", digits=2) # get residuals
  
  sink() # disable suppress output
  
  diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
  dyn.cutoff<-mean(resid, na.rm=T) + cutoff # create variable indicating dynamic cutoff above average correlation
  resid<-as.data.frame(resid)
  
  # table
  resid<-as.data.frame(resid)
  for (i in 1:ncol(resid)) {
    resid[,i]<-round(resid[,i],2)
  }
  resid[upper.tri(resid)] <- "" # remove values in upper right triangle to clean up table
  diag(resid) <- "" # same for diagonal
  
  resid %>% 
    #mutate(across(where(is.numeric), round, 2)) %>%
    mutate(across(everything(), ~ cell_spec(.x, color = case_when(.x >= dyn.cutoff ~ "red", TRUE ~ "black")))) %>%  
    kbl(booktabs = T, escape = F, 
        table.attr = "style='width:50%;'") %>%
    # bootstrap options are for HTML output
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  position = "left",
                  full_width = F,
                  font_size = r.fontsize,
                  fixed_thead = T) %>% # when there is a long list in the table
    column_spec(1, bold = T) %>% 
    kable_classic(html_font = "Lato") %>% 
    # latex_options are for PDF output
    kable_styling(latex_options = c("striped","scale_down")) %>% 
    footnote(general = paste0("Relative cut-off value (highlighted in red) is ", round(dyn.cutoff,3), ", which is ", cutoff, " above the average correlation."))
  
}


#' Targeting, Wright map derivative.
#' 
#' Outputs a figure consisting of three figures with the 
#' same scale on top of each other. 
#' At the top is a histogram of Person Locations, with a dotted line and 
#' gray field indicating mean/SD. In the middle is a similar histogram with 
#' Item Thresholds. At the bottom is a figure showing the individual 
#' item thresholds as dots.
#' 
#' @param dfin Dataframe with item data only
#' @param dich Set to TRUE if your data is dichotomous
#' @export
RItargeting <- function(dfin, dich) {
  if(missing(dich)) {
  df.erm<-PCM(dfin) # run PCM model
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  item.se <- item.estimates$se.thresh
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  
  item.locations<-item_difficulty[,2:ncol(item_difficulty)]
  names(item.locations) <- paste0("T", c(1:ncol(item.locations))) #re-number items
  itemloc.long <- item.locations %>%
    rownames_to_column() %>% 
    dplyr::rename(names = 'rowname') %>%
    mutate(names = factor(names, levels = rev(names(dfin)))) %>%
    pivot_longer(cols=starts_with('T'), 
                 names_to ='thresholds', 
                 values_to = 'par_values' )
  ### create df for ggplot histograms
  # person locations
  thetas<-as.data.frame(person.locations.estimate$theta.table)
  pthetas<-thetas$`Person Parameter`
  # item locations
  thresholds<-c()
  for (i in 2:ncol(item_difficulty)) {
    thresholds<-c(thresholds,item_difficulty[,i])
  }
  ### items and persons in the same variable
  #create data frame with 0 rows and 3 columns
  df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
  #provide column names
  colnames(df.locations) <- c('type', 'locations')
  # change type of data
  df.locations$type<-as.character(df.locations$type)
  df.locations$locations<-as.numeric(df.locations$locations)
  # insert labels in accurate amounts (N+items)
  nper<-nrow(dfin)
  nperp<-nper+1
  nthr<-length(thresholds)+nper
  df.locations[1:nper,1]<-paste0("Persons")
  df.locations[nperp:nthr,1]<-paste0("Item thresholds")
  # insert data from vectors with thetas and thresholds
  df.locations$locations<-c(pthetas,thresholds)
  # change type to class factor
  df.locations$type<-as.factor(df.locations$type)
  
  # get mean/SD for item/person locations
  pi.locations <- data.frame(matrix(ncol = 3, nrow = 3))
  
  item.mean <- round(mean(item_difficulty$Location),2)
  item.sd <- round(sd(item_difficulty$Location),2)
  item.thresh.sd <- item_difficulty %>% 
    select(starts_with("Threshold")) %>% 
    pivot_longer(everything()) %>% 
    pull() %>% 
    na.omit() %>% sd() %>% round(2)
  person.mean <- round(mean(pthetas),2)
  person.sd <- round(sd(pthetas),2)
  #provide column names
  colnames(pi.locations) <- c('','Mean', 'SD')
  pi.locations[1,1] <- "Items"
  pi.locations[1,2] <- round(mean(item_difficulty$Location),2)
  pi.locations[1,3] <- round(sd(item_difficulty$Location),2)
  pi.locations[2,1] <- "Item thresholds"
  pi.locations[2,2] <- round(mean(item_difficulty$Location),2)
  pi.locations[2,3] <- item.thresh.sd
  pi.locations[3,1] <- "Persons"
  pi.locations[3,2] <- round(mean(pthetas),2)
  pi.locations[3,3] <- round(sd(pthetas),2)
  
  # Person location histogram
  p2<-ggplot() + 
    geom_histogram(data=subset(df.locations, type=="Persons"), 
                   aes(locations, fill="Persons", y= ..count..)) +
    xlab('') +
    ylab('Persons') +
    scale_x_continuous(limits = c(-5,6), breaks = scales::pretty_breaks(n = 10)) + 
    geom_vline(xintercept = person.mean, color = RISEcompGreenDark, linetype = 2) +
    annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean-person.sd), xmax = (person.mean+person.sd), alpha = .2) +
    geom_text(hjust = 1.1, vjust = 1) +
    theme_bw() +
    theme(legend.position = 'none',
          text=element_text(family = "sans"))
  
  # Item Threshold location histogram
  p3 <- ggplot() +
    geom_histogram(data=subset(df.locations, type=="Item thresholds"), 
                   aes(locations, y= ..count..)) + 
    xlab('') +
    ylab('Thresholds') +
    scale_x_continuous(limits = c(-5,6), breaks = scales::pretty_breaks(n = 10)) + 
    scale_y_reverse() +
    geom_vline(xintercept = item.mean, color = RISEprimRed, linetype = 2) +
    annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean-item.thresh.sd), xmax = (item.mean+item.thresh.sd), alpha = .2) +
    geom_text(hjust = 1.1, vjust = 1) +
    theme_bw() +
    theme(legend.position = 'none')
  
  # make plot with each items thresholds shown as dots
  p1=ggplot(itemloc.long, aes(x = names, y = par_values, label = thresholds, color = names)) +
    geom_point() + 
    geom_text(hjust = 1.1, vjust = 1) + 
    ylab('Location (logit scale)') + 
    xlab('Items') + 
    scale_y_continuous(limits = c(-5,6), breaks = scales::pretty_breaks(n = 10)) + 
    theme_bw() + 
    theme(legend.position = 'none') + 
    coord_flip() +
    labs(caption = paste0("Person location average: ", pi.locations[3,2], " (SD ", pi.locations[3,3],"), Item threshold location average: ",
                          pi.locations[2,2], " (SD ", pi.locations[2,3], ").")) +
    theme(plot.caption = element_text(hjust = 0, face = "italic"))
  
  # combine plots together to create Wright map, and let the individual item threshold plot have some more space
  plot_grid(p2,p3,p1, labels=NULL, nrow = 3, align ="hv", rel_heights = c(1,1,1.4))
  } else {
    df.erm<-RM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.estimates <- coef(df.erm, "eta") # item coefficients
    item.fit <- eRm::itemfit(person.locations.estimate)
    
    item.locations<-as.data.frame(item.estimates)
    #names(item.locations) <- paste0("T", c(1:ncol(item.locations))) #re-number items
    itemloc.long <- item.locations %>%
      rownames_to_column() %>% 
      dplyr::rename(names = 'rowname') 
    
    ### create df for ggplot histograms
    # person locations
    thetas<-as.data.frame(person.locations.estimate$theta.table)
    pthetas<-thetas$`Person Parameter`
    # item locations
    thresholds<-itemloc.long$item.estimates
    ### items and persons in the same variable
    #create data frame with 0 rows and 3 columns
    df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
    #provide column names
    colnames(df.locations) <- c('type', 'locations')
    # change type of data
    df.locations$type<-as.character(df.locations$type)
    df.locations$locations<-as.numeric(df.locations$locations)
    # insert labels in accurate amounts (N+items)
    nper<-nrow(dfin)
    nperp<-nper+1
    nthr<-length(thresholds)+nper
    df.locations[1:nper,1]<-paste0("Persons")
    df.locations[nperp:nthr,1]<-paste0("Item thresholds")
    # insert data from vectors with thetas and thresholds
    df.locations$locations<-c(pthetas,thresholds)
    # change type to class factor
    df.locations$type<-as.factor(df.locations$type)
    
    # get mean/SD for item/person locations
    pi.locations <- data.frame(matrix(ncol = 3, nrow = 3))
    item_difficulty <- as.data.frame(item.estimates) %>% 
      rownames_to_column() %>% 
      dplyr::rename(Item = 'rowname', Location = 'item.estimates')
    
    #
    item.mean <- round(mean(item_difficulty$Location),2)
    item.sd <- round(sd(item_difficulty$Location),2)
    person.mean <- round(mean(pthetas),2)
    person.sd <- round(sd(pthetas),2)
    #provide column names
    colnames(pi.locations) <- c('','Mean', 'SD')
    pi.locations[1,1] <- "Items"
    pi.locations[1,2] <- round(mean(item_difficulty$Location),2)
    pi.locations[1,3] <- round(sd(item_difficulty$Location),2)
    pi.locations[2,1] <- "Persons"
    pi.locations[2,2] <- round(mean(pthetas),2)
    pi.locations[2,3] <- round(sd(pthetas),2)
  
    # make plot with each items thresholds shown as dots
    p1=ggplot(itemloc.long, aes(x = names, y = item.estimates, label = names, color = names)) +
      geom_point() + 
      geom_text(hjust = 1.1, vjust = 1) + 
      ylab('Location (logit scale)') + 
      xlab('Items') + 
      scale_y_continuous(limits = c(-5,6), breaks = scales::pretty_breaks(n = 10)) + 
      theme_bw() + 
      theme(legend.position = 'none') + 
      coord_flip() +
      labs(caption = paste0("Person location average: ", pi.locations[2,2], " (SD ", pi.locations[2,3],"), Item location average: ",
                            pi.locations[1,2], " (SD ", pi.locations[1,3], ").")) +
      theme(plot.caption = element_text(hjust = 0, face = "italic"))
    
    # Person location histogram
    p2<-ggplot() + 
      geom_histogram(data=subset(df.locations, type=="Persons"), 
                     aes(locations, fill="Persons", y= ..count..)) +
      xlab('') +
      ylab('Persons') +
      scale_x_continuous(limits = c(-5,6), breaks = scales::pretty_breaks(n = 10)) + 
      geom_vline(xintercept = person.mean, color = RISEcompGreenDark, linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean-person.sd), xmax = (person.mean+person.sd), alpha = .2) +
      geom_text(hjust = 1.1, vjust = 1) +
      theme_bw() +
      theme(legend.position = 'none',
            text=element_text(family = "sans"))
    
    # Item Threshold location histogram
    p3 <- ggplot() +
      geom_histogram(data=subset(df.locations, type=="Item thresholds"), 
                     aes(locations, y= ..count..)) + 
      xlab('') +
      ylab('Items aggregated') +
      scale_x_continuous(limits = c(-5,6), breaks = scales::pretty_breaks(n = 10)) + 
      scale_y_reverse() +
      geom_vline(xintercept = item.mean, color = RISEprimRed, linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean-item.sd), xmax = (item.mean+item.sd), alpha = .2) +
      geom_text(hjust = 1.1, vjust = 1) +
      theme_bw() +
      theme(legend.position = 'none')
    
    # combine plots together to create Wright map, and let the individual item threshold plot have some more space
    plot_grid(p2,p3,p1, labels=NULL, nrow = 3, align ="hv", rel_heights = c(1,1,1.4))
    
  }
}


#' Reliability, overall test information
#' 
#' Note: the eRm function below will provide individual item information curves
#' plotINFO(df.erm, type = "item", legpos = "topleft")
#' 
#' @param dfin Dataframe with item data only
#' @param lo Lower limit of x axis (default = -6)
#' @param hi Upper limit of x axis (default = 6)
#' @export
RItif <- function(dfin, lo, hi) {
  if(missing(lo)) {
  df.erm <- PCM(dfin)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  item.se <- item.estimates$se.thresh
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  # person locations
  thetas<-as.data.frame(person.locations.estimate$theta.table)
  pthetas<-thetas$`Person Parameter`
  # item locations
  thresholds<-c()
  for (i in 2:ncol(item_difficulty)) {
    thresholds<-c(thresholds,item_difficulty[,i])
  }
  #create data frame with 0 rows and 3 columns
  df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
  #provide column names
  colnames(df.locations) <- c('type', 'locations')
  # change type of data
  df.locations$type<-as.character(df.locations$type)
  df.locations$locations<-as.numeric(df.locations$locations)
  # insert labels in accurate amounts (N+items)
  nper<-nrow(dfin)
  nperp<-nper+1
  nthr<-length(thresholds)+nper
  df.locations[1:nper,1]<-paste0("Persons")
  df.locations[nperp:nthr,1]<-paste0("Item thresholds")
  # insert data from vectors with thetas and thresholds
  df.locations$locations<-c(pthetas,thresholds)
  # change type to class factor
  df.locations$type<-as.factor(df.locations$type)
  
  
  # we need to make a new dataframe for the test information plot/curve
  psimatrix <- data.frame(matrix(ncol = 2, nrow = 1001)) 
  names(psimatrix) <- c("psY","psX")
  # this gets 1001 "dots" for the scale information variable y
  psimatrix$psY <- test_info(df.erm, seq(-6, 6, length.out = 1001L)) 
  # this is the x variable in the TIF figure
  psimatrix$psX <- seq(-6, 6, length.out = 1001L)
  
  # check if TIF goes above 3.3
  peak.tif <- psimatrix %>% slice(which.max(psY)) %>% select(psY) %>% pull()
  
  if (peak.tif > 3.32) {
    # now find where the cutoff points are for 3.33 on the theta (x) variable 
    # this provides the highest and lowest value into two variables
    psep_min <- psimatrix %>% filter(psX < 0) %>% slice(which.min(abs(psY - 3.33))) %>% select(psX) %>% pull()
    psep_max <- psimatrix %>% filter(psX > 0) %>%  slice(which.min(abs(psY - 3.33))) %>% select(psX) %>% pull()
    # calculate how many participants cross the cutoffs
    nCeilingRel<-length(which(pthetas > psep_max))
    nFloorRel<-length(which(pthetas < psep_min))
    nWithinRel<-(length(pthetas)-(nCeilingRel+nFloorRel))
    # Retrieve the lowest and highest item thresholds into vector variables
    min_thresh <- df.locations %>% 
      filter(type == "Item thresholds") %>% 
      arrange(locations) %>% 
      slice(1) %>% 
      pull()
    max_thresh <- df.locations %>% 
      filter(type == "Item thresholds") %>% 
      arrange(desc(locations)) %>% 
      slice(1) %>% 
      pull()
    
    # calculate how many participants cross the cutoffs
    nCeilingThresh<-length(which(pthetas > max_thresh))
    nFloorThresh<-length(which(pthetas < min_thresh))
    #PSI<-SepRel(person.locations.estimate)
    psep_caption <- paste0("Test Information 3.33 (PSI 0.7) is reached between ", psep_min, " and ", psep_max, " logits, where ", 
                           round(nWithinRel/length(pthetas)*100,1), "% of the participants are located. \n",
                           round(nCeilingRel/length(pthetas)*100,1), "% of participants have locations above the upper cutoff, and ",
                           round(nFloorRel/length(pthetas)*100,1), "% are below the lower cutoff. \n",
                           round(nCeilingThresh/length(pthetas)*100,1), "% have person locations above the highest item threshold (",
                           round(max_thresh,2), ") and ", round(nFloorThresh/length(pthetas)*100,1), "% are below the lowest item threshold (",
                           round(min_thresh,2), ").")
  } else {
    psep_min = 0
    psep_max = 0
    psep_caption <- paste0("Test information is not above 3.33 (PSI 0.7) at any part of the scale.")
  }
  
  ggplot(psimatrix) + 
    geom_point(aes(x=psX, y=psY), size = 0.1, color = dot_color) +
    geom_hline(yintercept = 3.33, color = cutoff_line, linetype = 2, size = 0.5) +
    geom_hline(yintercept = 5, color = cutoff_line, linetype = 2, size = 0.7) + 
    scale_y_continuous(breaks=seq(0, 8, by = 1)) +
    scale_x_continuous(breaks=seq(-6, 6, by = 1)) +
    labs(x = "Logits", y = "Test information") +
    labs(caption = paste0(psep_caption)) +
    theme(plot.caption = element_text(hjust = 0, face = "italic")) +
    theme(
      panel.background = element_rect(fill = backg_color,
                                      colour = backg_color,
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  } else {
    df.erm <- PCM(dfin)
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # person locations
    thetas<-as.data.frame(person.locations.estimate$theta.table)
    pthetas<-thetas$`Person Parameter`
    # item locations
    thresholds<-c()
    for (i in 2:ncol(item_difficulty)) {
      thresholds<-c(thresholds,item_difficulty[,i])
    }
    #create data frame with 0 rows and 3 columns
    df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
    #provide column names
    colnames(df.locations) <- c('type', 'locations')
    # change type of data
    df.locations$type<-as.character(df.locations$type)
    df.locations$locations<-as.numeric(df.locations$locations)
    # insert labels in accurate amounts (N+items)
    nper<-nrow(dfin)
    nperp<-nper+1
    nthr<-length(thresholds)+nper
    df.locations[1:nper,1]<-paste0("Persons")
    df.locations[nperp:nthr,1]<-paste0("Item thresholds")
    # insert data from vectors with thetas and thresholds
    df.locations$locations<-c(pthetas,thresholds)
    # change type to class factor
    df.locations$type<-as.factor(df.locations$type)
    
    
    # we need to make a new dataframe for the test information plot/curve
    psimatrix <- data.frame(matrix(ncol = 2, nrow = 1001)) 
    names(psimatrix) <- c("psY","psX")
    # this gets 1001 "dots" for the scale information variable y
    psimatrix$psY <- test_info(df.erm, seq(lo, hi, length.out = 1001L)) 
    # this is the x variable in the TIF figure
    psimatrix$psX <- seq(lo, hi, length.out = 1001L)
    
    # check if TIF goes above 3.3
    peak.tif <- psimatrix %>% slice(which.max(psY)) %>% select(psY) %>% pull()
    
    if (peak.tif > 3.32) {
      # now find where the cutoff points are for 3.33 on the theta (x) variable 
      # this provides the highest and lowest value into two variables
      psep_min <- psimatrix %>% filter(psX < 0) %>% slice(which.min(abs(psY - 3.33))) %>% select(psX) %>% pull()
      psep_max <- psimatrix %>% filter(psX > 0) %>%  slice(which.min(abs(psY - 3.33))) %>% select(psX) %>% pull()
      # calculate how many participants cross the cutoffs
      nCeilingRel<-length(which(pthetas > psep_max))
      nFloorRel<-length(which(pthetas < psep_min))
      nWithinRel<-(length(pthetas)-(nCeilingRel+nFloorRel))
      # Retrieve the lowest and highest item thresholds into vector variables
      min_thresh <- df.locations %>% 
        filter(type == "Item thresholds") %>% 
        arrange(locations) %>% 
        slice(1) %>% 
        pull()
      max_thresh <- df.locations %>% 
        filter(type == "Item thresholds") %>% 
        arrange(desc(locations)) %>% 
        slice(1) %>% 
        pull()
      
      # calculate how many participants cross the cutoffs
      nCeilingThresh<-length(which(pthetas > max_thresh))
      nFloorThresh<-length(which(pthetas < min_thresh))
      #PSI<-SepRel(person.locations.estimate)
      psep_caption <- paste0("Test Information 3.33 (PSI 0.7) is reached between ", psep_min, " and ", psep_max, " logits, where ", 
                             round(nWithinRel/length(pthetas)*100,1), "% of the participants are located. \n",
                             round(nCeilingRel/length(pthetas)*100,1), "% of participants have locations above the upper cutoff, and ",
                             round(nFloorRel/length(pthetas)*100,1), "% are below the lower cutoff. \n",
                             round(nCeilingThresh/length(pthetas)*100,1), "% have person locations above the highest item threshold (",
                             round(max_thresh,2), ") and ", round(nFloorThresh/length(pthetas)*100,1), "% are below the lowest item threshold (",
                             round(min_thresh,2), ").")
    } else {
      psep_min = 0
      psep_max = 0
      psep_caption <- paste0("Test information is not above 3.33 (PSI 0.7) at any part of the scale.")
    }
    
    ggplot(psimatrix) + 
      geom_point(aes(x=psX, y=psY), size = 0.1, color = dot_color) +
      geom_hline(yintercept = 3.33, color = cutoff_line, linetype = 2, size = 0.5) +
      geom_hline(yintercept = 5, color = cutoff_line, linetype = 2, size = 0.7) + 
      scale_y_continuous(breaks=seq(0, 8, by = 1)) +
      scale_x_continuous(breaks=seq(lo, hi, by = 1)) +
      labs(x = "Logits", y = "Test information") +
      labs(caption = paste0(psep_caption)) +
      theme(plot.caption = element_text(hjust = 0, face = "italic")) +
      theme(
        panel.background = element_rect(fill = backg_color,
                                        colour = backg_color,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}


#' Person fit
#' 
#' @param dfin Dataframe with item data only
#' @export
RIpfit <- function(dfin) {
  df.erm <- PCM(dfin)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  item.se <- item.estimates$se.thresh
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  
  person.fit <- eRm::personfit(person.locations.estimate)
  thetas2<-as.data.frame(person.locations.estimate$theta.table)
  
  nPfit <- length(person.fit$p.infitZ)
  nCeilingPfit<-length(which(person.fit$p.infitZ > 2))
  nFloorPfit<-length(which(person.fit$p.infitZ < -2))
  nPgoodfit<-(nPfit-(nCeilingPfit+nFloorPfit))
  
  hist(person.fit$p.infitZ, col = RISEprimGreen, xlim = c(-4,6), xlab = "Person infit ZSTD", main = "Histogram of Person infit ZSTD")
  
  # check whether there are excluded observations, and if found, adjust thetas2 df
  if (length(person.fit$excl_obs_num) > 0L) {
    thetas2[person.fit$excl_obs_num,] <- NA
    thetas2 <- na.omit(thetas2)
  }
  df.pfit <- data.frame(matrix(ncol = 2, nrow = nrow(thetas2)))
  #provide column names
  colnames(df.pfit) <- c('Person locations', 'Person infit ZSTD')
  df.pfit$`Person locations` <- thetas2$`Person Parameter`
  df.pfit$`Person infit ZSTD` <- person.fit$p.infitZ
  
  # figure
  df.pfit %>% 
    ggplot(aes(x=`Person infit ZSTD`, y=`Person locations`, label="")) +
    geom_point(size = 1, color = dot_color) +
    geom_vline(xintercept = -2, color = cutoff_line, linetype = 2, size = 0.7) +
    geom_vline(xintercept = 2, color = cutoff_line, linetype = 2, size = 0.7) + 
    scale_y_continuous(breaks=seq(-5, 5, by = 1)) +
    scale_x_continuous(breaks=seq(-5, 7, by = 1)) +
    labs(caption = paste0(round(nCeilingPfit/nPfit*100,1), "% of participants have person infit ZSTD below -2.0, and ",
                          round(nFloorPfit/nPfit*100,1), "% are above 2.0. \nThus, ", round(nPgoodfit/nPfit*100,1), "% are within +/- 2 infit ZSTD.")) +
    theme(plot.caption = element_text(hjust = 0, face = "italic")) +
    theme(
      panel.background = element_rect(fill = backg_color,
                                      colour = backg_color,
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
}


#' Item parameters summary
#' 
#' @param dfin Dataframe with item data only
#' @export
RIitemparams <- function(dfin) {
  df.erm <- PCM(dfin)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  
  item_difficulty %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    relocate(Location, .after = last_col()) %>% 
    mutate(Location = cell_spec(Location, bold = T, align = "left")) %>% 
    dplyr::rename('Item location' = Location) %>% 
    kbl(booktabs = T, escape = F) %>%
    # bootstrap options are for HTML output
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  position = "left",
                  full_width = F,
                  font_size = r.fontsize,
                  fixed_thead = T) %>% # when there is a long list in the table
    #  column_spec(c(2:3), color = "red") %>% 
    #  row_spec(3:5, bold = T, color = "white", background = "lightblue") %>% 
    column_spec(1, bold = T) %>% 
    kable_classic(html_font = "Lato") %>% 
    # latex_options are for PDF output
    kable_styling(latex_options = c("striped","scale_down"))
}


##### Construct alley plots

#' Plot with infit ZSTD and item location
#' ZSTD is sample size sensitive, see "RIitemfitPCM"
#' for options
#' 
#' @param dfin Dataframe with item data only
#' @param jz Desired sample size in multisampling
#' @param yz Desired number of samples (recommended range 10-50)
#' @export
RIinfitLoc <- function(dfin, jz, yz) {
  if(missing(jz)) {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, 
                                        item.fit$i.outfitZ, item.fit$i.infitZ))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    item.fit.table$Location <- item_difficulty$Location
    
    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))
    
    item.fit.table %>% 
      rownames_to_column() %>% 
      ggplot(aes(x=InfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = dot_color) +
      geom_vline(xintercept = -2, color = cutoff_line, linetype = 2) +
      geom_vline(xintercept = 2, color = cutoff_line, linetype = 2) + 
      scale_y_continuous(limits = ylims, breaks = ybreaks, 
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) + 
      theme(
        panel.background = element_rect(fill = backg_color,
                                        colour = backg_color,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  } else {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    
    # ZSTD multisample
    outfitZ<-c()
    infitZ<-c()
    for (i in 1:yz) {
      df.new <- dfin[sample(1:nrow(dfin), jz), ]
      df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ<-cbind(outfitZ,item.fit.z$i.outfitZ)
      infitZ<-cbind(infitZ,item.fit.z$i.infitZ)
    }
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, rowMeans(outfitZ), rowMeans(infitZ)))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    
    item.fit.table$Location <- item_difficulty$Location
    
    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))
    
    item.fit.table %>% 
      rownames_to_column() %>% 
      ggplot(aes(x=InfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = dot_color) +
      geom_vline(xintercept = -2, color = cutoff_line, linetype = 2) +
      geom_vline(xintercept = 2, color = cutoff_line, linetype = 2) + 
      scale_y_continuous(limits = ylims, breaks = ybreaks, 
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) + 
      theme(
        panel.background = element_rect(fill = backg_color,
                                        colour = backg_color,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}


#' Plot with outfit ZSTD and item location
#' ZSTD is sample size sensitive, see "RIitemfitPCM"
#' 
#' @param dfin Dataframe with item data only
#' @param jz Desired sample size in multisampling
#' @param yz Desired number of samples (recommended range 10-50)
#' @export
RIoutfitLoc <- function(dfin, jz, yz) {
  if(missing(jz)) {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, 
                                        item.fit$i.outfitZ, item.fit$i.infitZ))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    item.fit.table$Location <- item_difficulty$Location
    
    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))
    
    item.fit.table %>% 
      rownames_to_column() %>% 
      ggplot(aes(x=OutfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = dot_color) +
      geom_vline(xintercept = -2, color = cutoff_line, linetype = 2) +
      geom_vline(xintercept = 2, color = cutoff_line, linetype = 2) + 
      scale_y_continuous(limits = ylims, breaks = ybreaks, 
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) + 
      theme(
        panel.background = element_rect(fill = backg_color,
                                        colour = backg_color,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  } else {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    
    # ZSTD multisample
    outfitZ<-c()
    infitZ<-c()
    for (i in 1:yz) {
      df.new <- dfin[sample(1:nrow(dfin), jz), ]
      df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ<-cbind(outfitZ,item.fit.z$i.outfitZ)
      infitZ<-cbind(infitZ,item.fit.z$i.infitZ)
    }
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, rowMeans(outfitZ), rowMeans(infitZ)))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    
    item.fit.table$Location <- item_difficulty$Location
    
    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))
    
    item.fit.table %>% 
      rownames_to_column() %>% 
      ggplot(aes(x=OutfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = dot_color) +
      geom_vline(xintercept = -2, color = cutoff_line, linetype = 2) +
      geom_vline(xintercept = 2, color = cutoff_line, linetype = 2) + 
      scale_y_continuous(limits = ylims, breaks = ybreaks, 
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) + 
      theme(
        panel.background = element_rect(fill = backg_color,
                                        colour = backg_color,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}


#' Plot with first residual contrast loadings vs item locations 
#' 
#' @param dfin Dataframe with item data only
#' @export
RIloadLoc <- function(dfin) {
  df.erm<-PCM(dfin) # run PCM model
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  item.se <- item.estimates$se.thresh
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  std.resids <- item.fit$st.res
  
  pca2<-prcomp(std.resids)
  pcaloadings <- as.data.frame(pca2$rotation)
  pcaloadings$Location <- item_difficulty$Location
  
  # find limits of contrast 1 loadings and location
  xlims <- c(floor(min(pcaloadings$PC1)),ceiling(max(pcaloadings$PC1)))
  ylims <- c(floor(min(pcaloadings$Location)),ceiling(max(pcaloadings$Location)))
  # how many steps between min/max
  xbreaks <- seq(floor(min(pcaloadings$PC1)),ceiling(max(pcaloadings$PC1)), 1)
  ybreaks <- seq(floor(min(pcaloadings$Location)),ceiling(max(pcaloadings$Location)), 0.5)
  xdiff <- diff(c(floor(min(pcaloadings$PC1)),ceiling(max(pcaloadings$PC1))))
  ydiff <- diff(c(floor(min(pcaloadings$Location)),ceiling(max(pcaloadings$Location))))
  
  pcaloadings %>% 
    rownames_to_column() %>% 
    ggplot(aes(x=PC1, y=Location, label = rowname)) +
    geom_point(size = 3, color = dot_color) +
    xlab("Loadings on first residual contrast") +
    ylab("Item location") +
    geom_vline(xintercept = 0, color = cutoff_line, linetype = 2) +
    geom_hline(yintercept = 0, color = cutoff_line, linetype = 2) + 
    scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, by = 0.25)) +
    scale_y_continuous(limits = ylims, breaks = ybreaks) +
    geom_text(hjust=1.4, vjust=0.6) +
    theme(
      panel.background = element_rect(fill = backg_color,
                                      colour = backg_color,
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
}


#' DIF PCM analysis - requires having set up dif.variables previously
#' 
#' Makes use of the psychotree package, which also allows for interactions
#' between DIF variables, which is not implemented in this function (yet).
#' 
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#' 
#' sample usage: RIdifTable(df, dif.age)
#' 
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifTable <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-pctree(difdata ~ dif.var, data = df.tree)
  
  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    plot(pctree.out)
    
    itempar(pctree.out) %>% # identify the nodes to compare (see plot above)
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      mutate('Mean location' = rowMeans(.), StDev = rowSds(as.matrix(.))) %>%
      rowwise() %>% 
      mutate(MaxDiff = (max(c_across(c(1:(ncol(.)-2))))) - min(c_across(c(1:(ncol(.)-2))))) %>% 
      ungroup() %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>% 
      relocate(MaxDiff, .after = last_col()) %>% 
      formattable(list(
        'MaxDiff' = 
          formatter("span", style = ~ style(color = ifelse(MaxDiff < -0.5, "red",
                                                           ifelse(MaxDiff > 0.5, "red",  "black"))))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"')

  } else {
    print("No significant DIF found.")
  }
}


#' Create a DIF line graph, showing groups' PCM item locations
#' 
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigure <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-pctree(difdata ~ dif.var, data = df.tree)
  
  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
  # create dataframe for ggplot
  pctree.par <- itempar(pctree.out) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  pctree.par$Item<-names(dfin)
  pctree.par$item <- NULL
  rownames(pctree.par)<-NULL
  pctree.par <- melt(pctree.par, id.vars = "Item")
  names(pctree.par)<-c("Item", "Group", "Logits")
  # make plot
  ggplot(pctree.par, aes(x=Item, y=Logits, color=Group, group = Group)) +
    geom_line(size = 1.5) +
    geom_point(size = 2, color = "black")
  } else {
    print("No significant DIF found.")
  }
}

#' DIF analysis dichotomous - requires having set up dif.variables
#' 
#' Makes use of the psychotree package, which also allows for interactions
#' between DIF variables, which is not implemented in this function (yet).
#' 
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#' 
#' sample usage: RIdifTable(df, dif.age)
#' 
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifTableRM <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-raschtree(difdata ~ dif.var, data = df.tree)
  
  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    plot(pctree.out)
    
    itempar(pctree.out) %>% # identify the nodes to compare (see plot above)
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      #dplyr::rename('Age 18-29' = '2', # rename numerical node names to interpretable text
      #              'Age 30+' = '3') %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>% 
      rowwise() %>% 
      mutate(MaxDiff = (max(c_across(c(2:ncol(.))))) - min(c_across(c(2:ncol(.))))) %>% 
      ungroup() %>% 
      mutate('Mean location' = rowMeans(.[2:ncol(.)]), StDev = rowSds(as.matrix(.[2:ncol(.)]))) %>%
      #mutate(Medel = rowMeans(.[2:3]), Stdev = rowSds(as.matrix(.[2:3]))) %>%
      #mutate(Difference = .data[['Age 18-29']] - .data[['Age 30+']]) %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      #arrange(desc(Skillnad)) %>%
      relocate(MaxDiff, .after = last_col()) %>%
      formattable(list(
        'MaxDiff' = 
          formatter("span", style = ~ style(color = ifelse(MaxDiff < -0.5, "red",
                                                           ifelse(MaxDiff > 0.5, "red",  "black"))))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"')
    
  } else {
    print("No significant DIF found.")
  }
}


#' Create a DIF line graph, showing groups' RM item locations
#' 
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigureRM <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-raschtree(difdata ~ dif.var, data = df.tree)
  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    # make a line graph to visualize differences
    pctree.par<- itempar(pctree.out) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    pctree.par$Item<-names(dfin)
    pctree.par$item <- NULL
    rownames(pctree.par)<-NULL
    #names(pctree.par)<-c("Åk 9","Gy 2","Item")
    pctree.par <- melt(pctree.par, id.vars = "Item")
    names(pctree.par)<-c("Item", "Group", "Logits")
    #pctree.par$Item<-str_remove_all(pctree.par$Item, "[difdata]") # remove IF from item labels
    ggplot(pctree.par, aes(x=Item, y=Logits, color=Group, group = Group)) +
      geom_line(size = 1.5) +
      geom_point(size = 2, color = "black")
  } else {
    print("No significant DIF found.")
  }
}
