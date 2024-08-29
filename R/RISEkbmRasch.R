### RISE KBM Rasch analysis package, https://github.com/pgmj/RISEkbmRasch
### Created by magnus.p.johansson@ri.se ORCID: 0000-0003-1669-592X
### The contents of this file is licensed according to
### Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/

### See https://pgmj.github.io/raschrvignette/RaschRvign.html for vignette.

#' A simple ggplot theme for RISE formatting
#'
#' Use is optional :)
#'
#' See ?element_text for more details on available settings.
#'
#' Please note that using this theme does not update the session defaults for
#' geom_text and geom_text_repel. You can add the relevant line(s) manually:
#'
#'     update_geom_defaults("text", list(family = fontfamily)) +
#'     update_geom_defaults("text_repel", list(family = fontfamily))
#'
#' @param fontfamily Font family for all plot text
#' @param axissize Font size for axis labels
#' @param titlesize Font size for plot title
#' @param margins Distance of axis labels to plot
#' @param axisface Set to "bold" if you want bold axis labels
#' @return Add + theme_rise() to your ggplot or RIfunction that outputs a ggplot
#' @export
theme_rise <- function(fontfamily = "Lato", axissize = 13, titlesize = 15,
                       margins = 12, axisface = "plain", panelDist = 0.6, ...) {
  theme_minimal() +
  theme(
    text = element_text(family = fontfamily),
    axis.title.x = element_text(
      margin = margin(t = margins),
      size = axissize
    ),
    axis.title.y = element_text(
      margin = margin(r = margins),
      size = axissize
    ),
    plot.title = element_text(
      face = "bold",
      size = titlesize
    ),
    axis.title = element_text(
      face = axisface
    ),
    plot.caption = element_text(
      hjust = 0,
      face = "italic"
    ),
    legend.text = element_text(family = fontfamily),
    legend.background = element_rect(color = "lightgrey"),
    strip.background = element_rect(color = "lightgrey"),
    panel.spacing = unit(panelDist, "cm", data = NULL),
    panel.border = element_rect(color = "grey", fill = NA),
    ...
  )
}

#' A kableExtra function to simplify table creation
#'
#' @param data Dataframe/tibble to create table from
#' @param tbl_width Width of table (0-100)
#' @param fontsize Font size
#' @param fontfamily Font family
#' @param ... Passes options to kbl()
#' @export
kbl_rise <- function(data, tbl_width = 65, fontsize = 14, fontfamily = "Arial",
                     options = c("striped", "hover"), ...) {
  kbl(data, booktabs = T, escape = F,
      table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
    kable_styling(
      bootstrap_options = options,
      position = "left",
      full_width = T,
      font_size = fontsize,
      fixed_thead = T,
      latex_options = c("striped", "scale_down"),
      ...
    ) %>%
    row_spec(0, bold = T) %>%
    kable_classic(html_font = fontfamily)
}

#' Creates a figure with item missing data descriptives for items
#'
#' Sample use: `RImissing(df, itemStart = "PSS")`
#'
#' If `itemStart` is missing, the whole dataframe will be used.
#'
#' @param data Dataframe/tibble to create table from
#' @param itemStart What your variable names start with, in quotes
#' @export
RImissing <- function(data, itemStart) {

  if (missing(itemStart)) {
    data %>%
      t() %>%
      as.data.frame() %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      arrange(desc(Missing)) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Percentage = Missing / nrow(data) * 100) %>%
      mutate(Item = factor(Item, levels = rev(Item))) %>%
      ggplot(aes(x = Item, y = Percentage)) +
      geom_col(fill = "#009ca6") +
      geom_text(aes(label = paste0(round(Percentage,1),"%")),
                hjust = 1.5, vjust = 0.5,
                color = "white"
      ) +
      coord_flip() +
      ggtitle("Missing data per item") +
      xlab("Items") +
      ylab("Percentage of responses missing") +
      theme_minimal()

  } else {

  data %>%
    dplyr::select(starts_with({{ itemStart }})) %>%
    t() %>%
    as.data.frame() %>%
    mutate(Missing = rowSums(is.na(.))) %>%
    dplyr::select(Missing) %>%
    arrange(desc(Missing)) %>%
    rownames_to_column(var = "Item") %>%
    mutate(Percentage = Missing / nrow(data) * 100) %>%
    mutate(Item = factor(Item, levels = rev(Item))) %>%
    ggplot(aes(x = Item, y = Percentage)) +
    geom_col(fill = "#009ca6") +
    geom_text(aes(label = paste0(round(Percentage,1),"%")),
      hjust = 1.5, vjust = 0.5,
      color = "white"
    ) +
    coord_flip() +
    ggtitle("Missing data per item") +
    xlab("Items") +
    ylab("Percentage of responses missing") +
    theme_minimal()
  }
}

#' Creates a figure with item missing data descriptives for participants
#'
#' Sample use: `RImissingP(df, itemStart = "PSS")`
#'
#' If `itemStart` is missing, the whole dataframe will be used.
#'
#' @param data Dataframe/tibble to create table from
#' @param itemStart What your variable names start with, in quotes
#' @param output Optional dataframe with participants with missing data
#' @param n For large samples, show n participants with most missing data
#' @export
RImissingP <- function(data, itemStart, output, n = 10) {

  if (missing(itemStart)) {
    order <- data %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      rownames_to_column(var = "Participant") %>%
      na.omit() %>%
      dplyr::filter(Missing > 0) %>%
      arrange(desc(Missing)) %>%
      pull(Participant)

    data %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      rownames_to_column(var = "Participant") %>%
      na.omit() %>%
      dplyr::filter(Missing > 0) %>%
      arrange(desc(Missing)) %>%
      head(n) %>%
      mutate(Participant = as.factor(Participant)) %>%
      mutate(Participant = fct_relevel(Participant, rev(order))) %>%
      {

        ggplot(.,aes(x = Participant, y = Missing)) +
          geom_col(fill = "#009ca6") +
          geom_text(aes(label = paste0(round(Missing*100/ncol(data),1),"%")),
                    hjust = 1.1, vjust = 0.5,
                    color = "white"
          ) +
          scale_y_continuous(breaks = seq(from = 0,
                                          to = max(.$Missing),
                                          by = 1),
                             labels = scales::number_format(accuracy = 1),
                             minor_breaks = NULL) +
          coord_flip() +
          labs(title = "Missing data per participant",
               x = "Participant rownumber",
               y = "Number of responses missing",
               caption = paste0("Note. Total number of items is ", ncol(data),".")) +
          theme_minimal()
      }

  } else {

    order <- data %>%
      dplyr::select(starts_with({{ itemStart }})) %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      rownames_to_column(var = "Participant") %>%
      na.omit() %>%
      dplyr::filter(Missing > 0) %>%
      arrange(desc(Missing)) %>%
      pull(Participant)

    data %>%
      dplyr::select(starts_with({{ itemStart }})) %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      rownames_to_column(var = "Participant") %>%
      na.omit() %>%
      dplyr::filter(Missing > 0) %>%
      arrange(desc(Missing)) %>%
      head(n) %>%
      mutate(Participant = as.factor(Participant)) %>%
      mutate(Participant = fct_relevel(Participant, rev(order))) %>%
      {

        ggplot(.,aes(x = Participant, y = Missing)) +
          geom_col(fill = "#009ca6") +
          geom_text(aes(label = paste0(round(Missing*100/ncol(data),1),"%")),
                    hjust = 1.1, vjust = 0.5,
                    color = "white"
          ) +
          scale_y_continuous(breaks = seq(from = 0,
                                          to = max(.$Missing),
                                          by = 1),
                             labels = scales::number_format(accuracy = 1),
                             minor_breaks = NULL) +
          coord_flip() +
          labs(title = "Missing data per participant",
               x = "Participant rownumber",
               y = "Number of responses missing",
               caption = paste0("Note. Total number of items is ", ncol(data),".")) +
          theme_minimal()
      }
  }
}


#' Show items based on itemlabels file
#'
#' Requires a dataframe with two columns, labeled "itemnr" and "item",
#' containing information on the item numbers/labels and item content/description.
#' This dataframe has to be labeled `itemlabels`.
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
RIlistitems <- function(dfin, all.items = FALSE, ...) {
  if (all.items == FALSE) {

    itemlabels %>%
      dplyr::filter(itemnr %in% names(dfin)) %>%
      kbl_rise(...)

  } else {
    itemlabels %>%
      kbl_rise(...)
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
  if (missing(color)) {
    formattable(itemlabels,
      align = c("r", "l"), list(
        formattable::area(row = items) ~ color_tile("lightblue", "lightpink")
      ),
      table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"'
    )
  } else {
    formattable(itemlabels,
      align = c("r", "l"), list(
        formattable::area(row = items) ~ color_tile(color, "lightpink")
      ),
      table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"'
    )
  }
}

#' Create a table with the items used in a dataframe
#'
#' Depends on the `itemlabels` object, see package README.
#'
#' Intended for use with Quarto chunk option `column: margin`
#'
#' @param dfin Dataframe with item data only
#' @param fontsize Defaults to 11, optimize if desired
#' @export
RIlistItemsMargin <- function(dfin, fontsize = 11) {
  itemlabels %>%
    dplyr::filter(itemnr %in% names(dfin)) %>%
    formattable(align = c(
      "c",
      "l"
    ), list(itemnr = formatter("span", style = ~ style(
      color = "grey",
      font.weight = "bold"
    ))), table.attr = glue::glue("class=\"table table-striped\" style=\"font-size: {fontsize}px; font-family: Lato\""))
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
    mutate(persontotal = rowSums(., na.rm = TRUE)) %>%
    rownames_to_column("PersonID") %>%
    dplyr::select(PersonID, persontotal) %>%
    arrange(persontotal) %>%
    pull(PersonID)
  item.order <- dfin %>%
    t() %>%
    as.data.frame() %>%
    mutate(itemtotal = rowSums(., na.rm = TRUE)) %>%
    rownames_to_column("ItemID") %>%
    dplyr::select(ItemID, itemtotal) %>%
    arrange(itemtotal) %>%
    pull(ItemID)

  # use order vectors to sort item responses and make tile plot
  dfin %>%
    rownames_to_column("PersonID") %>%
    mutate(PersonID = factor(PersonID, levels = person.order)) %>%
    pivot_longer(where(is.numeric)) %>%
    dplyr::rename(Item = name) %>%
    mutate(Item = factor(Item, levels = item.order)) %>%
    ggplot(aes(x = PersonID, y = Item, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "#fff7dd", high = "#009ca6") +
    # scale_x_discrete(guide = guide_axis(n.dodge = 2))
    theme(axis.text.x = element_text(angle = 90))
}

#' Create table for demographic variables
#'
#' Input should be a vector with a demographic variable such as gender or age,
#' and the desired label, enclosed in double quotes.
#'
#' Sample use: RIdemographics(dif.gender, "Gender", width = 40)
#'
#' @param dif.var A vector with a demographic variable
#' @param diflabel What the variable represents (sex/age/etc), in quotes
#' @param ... Options for table, see `kbl_rise()`
#' @export
RIdemographics <- function(dif.var, diflabel, ...) {
  dif.var %>%
    table() %>%
    as_tibble() %>%
    mutate("Percent" = (round((100 * n / sum(n)), 1))) %>%
    dplyr::rename(!!quo_name(diflabel) := ".") %>%
    kbl_rise(...)
}

#' Create tile plot for all items, also showing the count of
#' responses in each response category for each item
#'
#' @param data Dataframe with item data only
#' @param cutoff Conditional highlighting of text in cells with n below cutoff
#' @param highlight Defaults to TRUE. Set to FALSE to disable text highlighting
#' @param percent Set to TRUE to replace n with percentage of item responses
#' @export
RItileplot <- function(data, cutoff = 10, highlight = TRUE, percent = FALSE, text_color = "orange") {

  tileplot <-
    data %>%
    pivot_longer(everything()) %>%
    dplyr::count(name, value) %>%
    mutate(name = factor(name, levels = rev(names(data)))) %>%
    group_by(name) %>%
    mutate(percentage = round(n/sum(n)*100,1)) %>%
    ungroup() %>%

    ggplot(aes(x = value, y = name, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(expression(italic(n)), limits = c(0, NA)) +
    scale_x_continuous("Response category", expand = c(0, 0), breaks = 0:max(data, na.rm = T)) + # change breaks to fit number of response categories
    labs(y = "Items") +
    theme(axis.text.x = element_text(size = 8))

  if(highlight == TRUE & percent == FALSE) {
    tileplot +
      geom_text(aes(label = n,
                    color = ifelse(n < cutoff,"red",text_color))) +
      guides(color = "none") +
      scale_color_identity()

  } else if(highlight == FALSE & percent == FALSE) {

    tileplot +
      geom_text(aes(label = n), color = text_color)

  } else if(highlight == TRUE & percent == TRUE) {

    tileplot +
      geom_text(aes(label = paste0(percentage,"%"),
                    color = ifelse(n < cutoff,"red",text_color))) +
      guides(color = "none") +
      scale_color_identity()

  } else if(highlight == FALSE & percent == TRUE) {

    tileplot +
      geom_text(aes(label = paste0(percentage,"%")), color = text_color)

  }
}

#' Create a stacked bar graph to show response distribution
#'
#' @param dfin Dataframe with item data only
#' @param omit.na Remove respondents with missing data (or not)
#' @export
RIbarstack <- function(dfin, omit.na = T) {
  if (omit.na) {
    dfin %>%
      na.omit() %>%
      pivot_longer(everything()) %>%
      dplyr::count(name, value) %>%
      mutate(Item = factor(name, levels = rev(names(dfin))),
             value = factor(value)) %>%
      mutate(value = forcats::fct_rev(value)) %>%
      ggplot(aes(x = n, y = Item, fill = value)) +
      geom_col() +
      scale_fill_viridis_d(direction = -1) +
      labs(title = "Item responses",
           x = "Number of responses",
           fill = "Category")
  } else {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(name, value) %>%
      mutate(Item = factor(name, levels = rev(names(dfin))),
             value = factor(value)) %>%
      mutate(value = forcats::fct_rev(value)) %>%
      ggplot(aes(x = n, y = Item, fill = value)) +
      geom_col() +
      scale_fill_viridis_d(direction = -1) +
      labs(title = "Item responses",
           x = "Number of responses",
           fill = "Category")
  }
}

#' Create a stacked diverging bar graph to show response distribution
#'
#' This function automatically removes respondents with missing data.
#'
#' @param dfin Dataframe with item data only
#' @export
RIbardiv <- function(dfin) {
  dfin %>%
    na.omit() %>%
    pivot_longer(everything()) %>%
    dplyr::rename(
      Item = name,
      Response = value
    ) %>%
    dplyr::count(Item, Response) %>%
    group_by(Item) %>%
    mutate(Percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    pivot_wider(id_cols = Item, names_from = Response, values_from = Percent) %>%
    dplyr::relocate("0", .after = Item) %>%
    likert(
      horizontal = TRUE, aspect = 1.5,
      main = "Distribution of responses",
      auto.key = list(
        space = "right", columns = 1,
        reverse = FALSE, padding.text = 2
      )
    )
}

#' Create individual bar plots for all items.
#'
#' @param dfin Dataframe with item data only
#' @export
RIbarplot <- function(dfin) {
  for (i in 1:ncol(dfin)) {
    barplot(table(dfin[, i]),
      col = "#8dc8c7",
      ylab = "Number of responses",
      xlab = "Response category"
    )
    # set item description as subtitle
    mtext(text = itemlabels %>%
                    dplyr::filter(itemnr %in% names(dfin))
                  %>% .[i, 2],
    side = 3,
    line = 0.4)
    # add itemnr as title
    mtext(text = itemlabels %>%
            dplyr::filter(itemnr %in% names(dfin)) %>%
            .[i, 1],
          side = 3,
          line = 1.5,
          font = 2)
  }
}


#' Create table with summarized responses across all items.
#'
#' @param dfin Dataframe with item data only
#' @param pdf.out Set to TRUE to get PDF-compatible table (kableExtra)
#' @param fontsize Set font size for PDF-compatible table
#' @export
RIallresp <- function(dfin, pdf.out, fontsize = 15) {
  if (missing(pdf.out)) {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(value) %>%
      mutate(percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      dplyr::rename(
        "Response category" = "value",
        "Number of responses" = "n",
        "Percent" = "percent"
      ) %>%
      formattable(
        list(
          `Response category` = formatter("span", style = ~ style(font.weight = "bold"))
        ),
        table.attr =
          'class=\"table table-striped\" style="font-size: 15px;
                  font-family: Lato; width: 50%"'
      )
  } else {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(value) %>%
      mutate(percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      dplyr::rename(
        "Response category" = "value",
        "Number of responses" = "n",
        "Percent" = "percent"
      ) %>%
      kbl(booktabs = T, escape = F, table.attr = "data-quarto-disable-processing='true' style='width:40%;'") %>%
      # options for HTML output
      kable_styling(
        bootstrap_options = c("striped", "hover"),
        position = "left",
        full_width = F,
        font_size = fontsize,
        fixed_thead = T
      ) %>%
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped", "scale_down"))
  }
}

#' Fits the Rasch PCM model using eRm, and
#' conducts a PCA of residuals to get eigenvalues using `psych::pca()`
#' and reports the top 5 values. Proportion of explained variance is calculated
#' using `stats::prcomp()`.
#'
#' Note from `?psych::pca`:
#' The eigen vectors are rescaled by the sqrt of the eigen values to produce
#' the component loadings more typical in factor analysis.
#'
#' @param dfin Dataframe with item data only
#' @param output Defaults to "table", optional "dataframe"
#' @param fontsize Set font size for table
#' @export
RIpcmPCA <- function(dfin, output = "table", fontsize = 15) {

  df.erm <- PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  std.resids <- item.fit$st.res

  if (ncol(dfin) > 5) {
    n_factors = 5
  } else {
    n_factors = ncol(dfin)
  }
  # PCA of Rasch residuals
  pca <- pca(std.resids, nfactors = n_factors, rotate = "oblimin")
  # get proportion of explained variance
  pca2 <- prcomp(std.resids, rank. = n_factors)
  PoV <- pca2$sdev^2/sum(pca2$sdev^2)
  # create table with top (max) 5 eigenvalues
  table <- pca$values %>%
    round(2) %>%
    head(n_factors) %>%
    as.data.frame(nm = "Eigenvalues") %>%
    add_column("Proportion of variance" = paste0(round(100*PoV[1:n_factors],1),"%"))

  if (output == "table") {
    return(kbl_rise(table))
  }

  if (output == "quarto") {
    return(knitr::kable(table))
  }

  if (output == "dataframe") {
    return(table)
  }
}

#' Fits the Rasch model for dichotomous data using eRm, and
#' conducts a PCA of residuals to get eigenvalues.
#'
#' @param dfin Dataframe with item data only
#' @param no.table Set to TRUE to avoid output of table
#' @param fontsize Set font size
#' @export
RIrmPCA <- function(dfin, no.table, fontsize = 15) {
  if (missing(no.table)) {
    df.erm <- RM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.estimates <- coef(df.erm, "eta") # item coefficients
    item.fit <- eRm::itemfit(person.locations.estimate)
    # item parameter CI's
    item.confint <- confint(df.erm, "eta") # difficulty (not easiness)
    # person parameter CI's
    pp.confint <- confint(person.locations.estimate)
    std.resids <- item.fit$st.res
    # PCA of Rasch residuals
    pca <- pca(std.resids, nfactors = ncol(dfin), rotate = "oblimin")
    # create table with top 5 eigenvalues
    pca$values %>%
      round(2) %>%
      head(5) %>%
      as_tibble() %>%
      dplyr::rename("Eigenvalues" = "value") %>%
      kbl(booktabs = T, escape = F, table.attr = "data-quarto-disable-processing='true' style='width:25%;'") %>%
      # options for HTML output
      kable_styling(
        bootstrap_options = c("striped", "hover"),
        position = "left",
        full_width = T,
        font_size = fontsize,
        fixed_thead = F
      ) %>%
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped", "scale_down"))
  } else {
    df.erm <- RM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.estimates <- coef(df.erm, "eta") # item coefficients
    item.fit <- eRm::itemfit(person.locations.estimate)
    # item parameter CI's
    item.confint <- confint(df.erm, "eta") # difficulty (not easiness)
    # person parameter CI's
    pp.confint <- confint(person.locations.estimate)
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

#' Individual ICC plots.
#' @param dfin Dataframe with item data only
#' @param items A single item (e.g. "q4"), or a vector with multiple items (e.g. c("q4","q2"))
#' @param xlims Start/end point for x-axis
#' @export
RIitemCats <- function(dfin, items = "all", xlims = c(-6,6), legend = FALSE) {
  # individual plots for those two items:
  df.erm <- PCM(dfin) # run PCM, partial credit model
  plotICC(df.erm,
    xlim = xlims, # change the x axis theta interval to display
    legpos = legend, # change legpos to TRUE if you want the legend displayed
    ylab = "Probability",
    xlab = "Person location (logit scale)",
    item.subset = items,
    ask = FALSE
  )
}

#' Floor/ceiling effects based on raw data (ordinal scores). Needs at least one
#' data point in each response category to produce correct footnote text.
#'
#' @param dfin Dataframe with item data only
#' @return A barplot with descriptives in footnote
#' @export
RIrawdist <- function(dfin) {
  df.erm <- PCM(dfin) # run PCM model
  # get info on thresholds
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty <- as.data.frame(item_difficulty)

  # all items should have lowest category 0, making 0 the lowest total score
  rawMin <- 0

  # get the number of thresholds above 0, to calculate max total raw score
  rawMax <- item_difficulty %>%
    dplyr::select(starts_with("Threshold")) %>%
    pivot_longer(everything()) %>%
    na.omit() %>%
    dplyr::count() %>%
    pull()

  # what is the lowest score in the sample?
  rawMinX <- dfin %>%
    mutate(rowsums = rowSums(.,na.rm = T)) %>%
    dplyr::count(rowsums) %>%
    arrange(rowsums) %>%
    head(1) %>%
    pull(rowsums)

  # if lowest participant score is higher than 0, we have no floor effect
  if (rawMinX > 0) {
    rawMinN <- 0
  } else { # if lowest participant score is 0, how many participants have scored 0?
    rawMinN <- dfin %>%
      mutate(rowsums = rowSums(.,na.rm = T)) %>%
      dplyr::count(rowsums) %>%
      arrange(rowsums) %>%
      head(1) %>%
      pull(n)
  }

  # what is the highest score in the sample?
  rawMaxX <- dfin %>%
    mutate(rowsums = rowSums(.,na.rm = T)) %>%
    dplyr::count(rowsums) %>%
    arrange(desc(rowsums)) %>%
    head(1) %>%
    pull(rowsums)

  # if highest score is below max rawscore, we have no ceiling effect
  if (rawMaxX < rawMax) {
    rawMaxN <- 0
  } else {
    rawMaxN <- dfin %>%
      mutate(rowsums = rowSums(.,na.rm = T)) %>%
      dplyr::count(rowsums) %>%
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
    mutate('Raw sum score' = rowSums(.,na.rm = T)) %>%
    pull() %>%
    table() %>%
    barplot(main = "Distribution of summed ordinal raw scores",
            ylab = "Number of participants",
            sub = paste0("Min score: ", floor_eff, "% , max score: ", ceiling_eff, "%."),
            xlim = c(0, rawMax),
            space = 0,
            col = "#009ca6")

}


#' Create table with Rasch PCM model item fit values for each item.
#'
#' Defaults to using conditional estimates for MSQ values (Müller, 2020)
#' estimated using the `iarm` package. Use `method = "unconditional"` for the
#' "old" unconditional MSQ values (using `eRm`).
#'
#' Since version 0.2.0 (2024-08-15), it is highly recommended to replace
#' rule-of-thumb cutoff values with simulation based cutoffs. See details in
#' `?RIgetfit()` for an easy way to get and set appropriate cutoff values.
#'
#' ZSTD is inflated with large samples (N > 500). There is an optional function
#' to use a reduced sample size and run analysis using multiple random samples
#' to get the average ZSTD for each item over all runs.
#'
#' If you are using Quarto, the YAML execute setting "cache: yes" will be a
#' useful chunk option to speed things up if you render often. 30-50 samples
#' seems to produce stable output, but 4-8 is probably sufficient for a quick
#' look at the approximate ZSTD statistics.
#' It is recommended to use sample size 200-500, based on
#' Hagell & Westergren (2016) & Müller (2020).
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 200-500)
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param fontsize Set fontsize for table
#' @param fontfamily Set font family for table
#' @param output Defaults to output a table. Optional "dataframe" or "quarto"
#' @param tbl_width Set table width in percent
#' @param method Defaults to "conditional". Optional "unconditional"
#' @param simcut Set to TRUE if you want to use simulation based cutoff values
#' @param gf The output object from `RIgetfit()` is needed when `simcut = TRUE`
#' @export
RIitemfitPCM <- function(dfin, samplesize, nsamples, zstd_min = -1.96, zstd_max = 1.96,
                         msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                         output = "table", tbl_width = 65, method = "conditional",
                         simcut = FALSE, gf) {

  if (missing(samplesize)) {
    df.erm <- PCM(dfin) # run PCM model

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            item.fit$i.outfitZ,
                                            item.fit$i.infitZ)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = item.fit$i.outfitZ,
                                   InfitZSTD = item.fit$i.infitZ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (simcut == TRUE) {
      if(missing(gf)) {
        stop("When `simcut = TRUE` you need to specify a `gf` object, output from `RIgetfit()`")
      }
      iterations <- length(gf) - 3

      lo_hi <- bind_rows(gf[1:(length(gf)-3)]) %>%
        summarise(max_infit_msq = quantile(infit_msq, .99),
                  min_infit_msq = quantile(infit_msq, .01),
                  max_outfit_msq = quantile(outfit_msq, .99),
                  min_outfit_msq = quantile(outfit_msq, .01),
                  max_infit_zstd = quantile(infit_zstd, .99),
                  min_infit_zstd = quantile(infit_zstd, .01),
                  max_outfit_zstd = quantile(outfit_zstd, .99),
                  min_outfit_zstd = quantile(outfit_zstd, .01),
        )

      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < lo_hi$min_outfit_zstd, "red",
                                                                 ifelse(OutfitZSTD > lo_hi$max_outfit_zstd, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < lo_hi$min_infit_zstd, "red",
                                                               ifelse(InfitZSTD > lo_hi$max_infit_zstd, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < lo_hi$min_outfit_msq, "red",
                                                               ifelse(OutfitMSQ > lo_hi$max_outfit_msq, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < lo_hi$min_infit_msq, "red",
                                                             ifelse(InfitMSQ > lo_hi$max_infit_msq, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))
    } else {

      if (output == "table") {
        # create table that highlights cutoff values in red
        item.fit.table %>%
          mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                   ifelse(OutfitZSTD > zstd_max, "red", "black")
          ))) %>%
          mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                                 ifelse(InfitZSTD > zstd_max, "red", "black")
          ))) %>%
          mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                                 ifelse(OutfitMSQ > msq_max, "red", "black")
          ))) %>%
          mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                               ifelse(InfitMSQ > msq_max, "red", "black")
          ))) %>%
          kbl(booktabs = T, escape = F,
              table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
          # bootstrap options are for HTML output
          kable_styling(
            bootstrap_options = c("striped", "hover"),
            position = "left",
            full_width = F,
            font_size = fontsize,
            fixed_thead = T
          ) %>%
          column_spec(1, bold = T) %>%
          row_spec(0, bold = T) %>%
          kable_classic(html_font = fontfamily) %>%
          # latex_options are for PDF output
          kable_styling(latex_options = c("striped", "scale_down")) %>%
          footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))

      } else if (output == "dataframe") {
        return(item.fit.table)
      } else if (output == "quarto") {
        knitr::kable(item.fit.table)
      }
    }

  } else {
    df.erm <- PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)

    # ZSTD multisample
    outfitZ <- c()
    infitZ <- c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      #df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ <- cbind(outfitZ, item.fit.z$i.outfitZ)
      infitZ <- cbind(infitZ, item.fit.z$i.infitZ)
    }

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            rowMeans(outfitZ),
                                            rowMeans(infitZ)
      )) %>%
        round(3) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = rowMeans(outfitZ),
                                   InfitZSTD = rowMeans(infitZ)
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (output == "table") {
      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                  "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))

    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }

  }
}


#' Create table with Rasch PCM model item fit values for each item.
#'
#' Special version of `RIitemfitPCM()` that utilizes multiple CPU cores to improve
#' performance. Requires `library(doParallel)`. To find how many cores you
#' have on your computer, use `parallel::detectCores()`, but remember to keep
#' some cores free.
#'
#' See documentation for `RIitemfitPCM()` for more complete information.
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 200-500)
#' @param nsamples Desired number of samples (recommended range 8-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param cpu Number of CPU cores to utilize (default = 4)
#' @param fontsize Set fontsize for table
#' @param fontfamily Set font family for table
#' @param output Defaults to output a table. Optional "dataframe" or "quarto"
#' @param tbl_width Set table width in percent
#' @param method Defaults to "conditional". Optional "unconditional"
#' @export
RIitemfitPCM2 <- function(dfin, samplesize = 200, nsamples = 8, cpu = 4,
                          zstd_min = -1.96, zstd_max = 1.96, msq_min = 0.7,
                          msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                          output = "table", tbl_width = 65,
                          method = "conditional") {
  require(doParallel)
  registerDoParallel(cores = cpu)
  df.erm <- PCM(dfin) # run PCM model

  # ZSTD multisample
  outfitZ <- c()
  infitZ <- c()

  infitZ <- foreach(icount(nsamples), .combine = cbind) %dopar% {
    library(eRm)
    df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
    df.new <- na.omit(df.new)
    df.z <- PCM(df.new)
    ple <- person.parameter(df.z)
    item.fit.z <- eRm::itemfit(ple)
    item.fit.z$i.infitZ
  }

  outfitZ <- foreach(icount(nsamples), .combine = cbind) %dopar% {
    library(eRm)
    df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
    df.new <- na.omit(df.new)
    df.z <- PCM(df.new)
    ple <- person.parameter(df.z)
    item.fit.z <- eRm::itemfit(ple)
    item.fit.z$i.outfitZ
  }

  if (method == "unconditional") {
    # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)

    # collect data to df
    item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                          item.fit$i.infitMSQ,
                                          rowMeans(outfitZ),
                                          rowMeans(infitZ)
    )) %>%
      round(3) %>%
      rownames_to_column("Item")

    colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

  } else if (method == "conditional") {
    # get conditional MSQ
    cfit <- iarm::out_infit(df.erm)
    # get unconditional MSQ
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)

    item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                 InfitMSQ = cfit$Infit,
                                 OutfitZSTD = rowMeans(outfitZ),
                                 InfitZSTD = rowMeans(infitZ)
    ) %>%
      round(3) %>%
      rownames_to_column("Item")
  }

  if (output == "table") {
    # create table that highlights cutoff values in red
    item.fit.table %>%
      mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                               ifelse(OutfitZSTD > zstd_max, "red", "black")
      ))) %>%
      mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                             ifelse(InfitZSTD > zstd_max, "red", "black")
      ))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                             ifelse(OutfitMSQ > msq_max, "red", "black")
      ))) %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                           ifelse(InfitMSQ > msq_max, "red", "black")
      ))) %>%
      kbl(booktabs = T, escape = F,
          table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(
        bootstrap_options = c("striped", "hover"),
        position = "left",
        full_width = F,
        font_size = fontsize,
        fixed_thead = T
      ) %>%
      column_spec(1, bold = T) %>%
      row_spec(0, bold = T) %>%
      kable_classic(html_font = fontfamily) %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped", "scale_down")) %>%
      footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))

  } else if (output == "dataframe") {
    return(item.fit.table)
  } else if (output == "quarto") {
    knitr::kable(item.fit.table)
  }
}

#' Create table with Rasch dichotomous model item fit values for each item.
#'
#' Defaults to using conditional estimates for MSQ values (Müller, 2020)
#' estimated using the `iarm` package. Use `method = "unconditional"` for the
#' "old" unconditional MSQ values (using `eRm`).
#'
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce
#' sample size and run analysis using multiple random samples to get average ZSTD
#' If you are using Quarto/Rmarkdown, "cache: yes" will be a useful chunk option to
#' speed things up. 50 samples seems to give stable output, but 4-8 is probably
#' sufficient for a quick look at the approximate ZSTD statistics. It is recommended
#' to use sample size 200-500, based on Hagell & Westergren, 2016.
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 250-500)
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param fontsize Set font size for table
#' @param fontfamily Set font family for table
#' @param output Defaults to output a table. Optional "dataframe" or "quarto"
#' @param tbl_width Set table width in percent
#' @param method Defaults to "conditional". Optional "unconditional"
#' @export
RIitemfitRM <- function(dfin, samplesize, nsamples, zstd_min = -1.96, zstd_max = 1.96,
                            msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                            output = "table", tbl_width = 65,
                            method = "conditional") {
  if(missing(samplesize)) {
    df.erm <- RM(dfin) # run Rasch model

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            item.fit$i.outfitZ,
                                            item.fit$i.infitZ)) %>%
        round(3) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = item.fit$i.outfitZ,
                                   InfitZSTD = item.fit$i.infitZ
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (output == "table") {

      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(bootstrap_options = c("striped", "hover"),
                      position = "left",
                      full_width = F,
                      font_size = fontsize,
                      fixed_thead = T) %>% # when there is a long list in the table
        column_spec(1, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped","scale_down")) %>%
        footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))

    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }

  } else { # for multisampling
    df.erm <- RM(dfin) # run Rasch model

    # ZSTD multisample
    outfitZ <- c()
    infitZ <- c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      df.new <- na.omit(df.new)
      df.z <- RM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ <- cbind(outfitZ, item.fit.z$i.outfitZ)
      infitZ <- cbind(infitZ, item.fit.z$i.infitZ)
    }

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            rowMeans(outfitZ),
                                            rowMeans(infitZ)
      )) %>%
        round(3) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = rowMeans(outfitZ),
                                   InfitZSTD = rowMeans(infitZ)
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (output == "table") {
      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                  "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))

    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
  }
}


#' Correlation matrix of Rasch residuals
#'
#' Mandatory option to set relative cutoff-value over the average of all
#' item residual correlations. It is strongly recommended to use the function
#' `RIgetResidCor()` to retrieve an appropriate cutoff value for your data.
#'
#' Christensen et al. (2017, p.181) write:
#' "under the null hypothesis, the average correlation of residuals is negative"
#'
#' Note that negative values are currently not highlighted with red text, even
#' if they are above the cutoff value.
#'
#' @param dfin Dataframe with item data only
#' @param cutoff Relative value above the average of all item residual correlations
#' @param fontsize Set font size for table
#' @param fontfamily Set font family for table
#' @param tbl_width Set table width in percent
#' @export
RIresidcorr <- function(dfin, cutoff, fontsize = 15, fontfamily = "Lato", tbl_width = 70) {

  sink(nullfile()) # suppress output from the rows below

  mirt.rasch <- mirt(dfin, model = 1, itemtype = 'Rasch') # unidimensional Rasch model
  resid = residuals(mirt.rasch, type = "Q3", digits = 2) # get residuals

  sink() # disable suppress output

  diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
  resid <- as.data.frame(resid)
  mean.resid <- resid %>%
    dplyr::select_if(is.numeric) %>%
    apply(2, mean, na.rm=T) %>%
    mean()
  dyn.cutoff <- mean.resid + cutoff # create variable indicating dynamic cutoff above average correlation

  # table
  resid <- resid %>%
    mutate_if(is.character,as.numeric) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  resid[upper.tri(resid)] <- "" # remove values in upper right triangle to clean up table
  diag(resid) <- "" # same for diagonal

  if(dyn.cutoff > 0) {
    resid %>%
      mutate(across(everything(), ~ cell_spec(.x, color = case_when(.x > dyn.cutoff ~ "red", TRUE ~ "black")))) %>%
      kbl(booktabs = T, escape = F,
          table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      row_spec(0, bold = T) %>%
      kable_classic(html_font = fontfamily) %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down")) %>%
      footnote(general = paste0("Relative cut-off value (highlighted in red) is ",
                                round(dyn.cutoff,3), ", which is ", round(cutoff,3),
                                " above the average correlation (",round(mean.resid,3),")."))
  } else {
    resid %>%
      mutate(across(everything(), ~ cell_spec(.x, color = case_when(.x > -dyn.cutoff ~ "red", TRUE ~ "black")))) %>%
      kbl(booktabs = T, escape = F,
          table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = fontfamily) %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down")) %>%
      footnote(general = paste0("Relative cut-off value (highlighted in red) is ",
                                round(dyn.cutoff,3), ", which is ", round(cutoff,3),
                                " above the average correlation (",round(mean.resid,3),")."))
  }

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
#' The figure is made up from three figures using library(patchwork). If desired,
#' you can output the three figures to a list object instead of a single figure.
#' This allows you to modify each figure (change theming, colors, etc). You can
#' put together the three figures into one using patchwork:
#'
#' `list$p1 / list$p2 / list$p3 + plot_layout(heights = c(1, 1, 1.4))`
#'
#' @param dfin Dataframe with item data only
#' @param model Defaults to "PCM", use "RM" for dichotomous data
#' @param xlim Optionally, set lower/upper limits for x axis
#' @param output Default "figure", or "list" to output 3 figures to a list object
#' @param bins Optionally, set number of bins for histograms
#' @export
RItargeting <- function(dfin, model = "PCM", xlim = c(-4,4), output = "figure", bins = 30) {
  if(model == "PCM") {
    if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
      stop("Use `model = 'RM'` for dichotomous data.")
    } else {
      erm_out <- PCM(dfin) # run PCM model
      item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
      #item_difficulty <- item.locations

      names(item.locations) <- paste0("t", c(1:ncol(item.locations))) # re-label variables
      itemloc.long <- item.locations %>%
        rownames_to_column() %>%
        dplyr::rename(names = "rowname") %>%
        mutate(names = factor(names, levels = rev(names(dfin)))) %>%
        pivot_longer(
          cols = starts_with("t"),
          names_to = "thresholds",
          values_to = "par_values"
        )
      ### create df for ggplot histograms
      # person locations
      pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
        as.data.frame() %>%
        pull(WLE)

      # check if xlim upper is below the highest person locations/thetas
      # and adjust if needed
      if (max(pthetas, na.rm = TRUE) > xlim[2]) {
        xlim[2] <- ceiling(max(pthetas, na.rm = TRUE))
      }
      # check if xlim lower is above the lowest person locations/thetas
      # and adjust if needed
      if (min(pthetas, na.rm = TRUE) < xlim[1]) {
        xlim[1] <- floor(min(pthetas, na.rm = TRUE))
      }
      # and then check if any item threshold is outside xlim
      if (max(itemloc.long$par_values, na.rm = TRUE) > xlim[2]) {
        xlim[2] <- ceiling(max(itemloc.long$par_values, na.rm = TRUE))
      }

      if (min(itemloc.long$par_values, na.rm = TRUE) < xlim[1]) {
        xlim[1] <- floor(min(itemloc.long$par_values, na.rm = TRUE))
      }

      # item locations
      thresholds <- c()
      for (i in 1:ncol(item.locations)) {
        thresholds <- c(thresholds, item.locations[, i])
      }
      ### items and persons in the same variable
      #create data frame with 0 rows and 3 columns
      df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
      # provide column names
      colnames(df.locations) <- c("type", "locations")
      # change type of data
      df.locations$type <- as.character(df.locations$type)
      df.locations$locations <- as.numeric(df.locations$locations)
      # insert labels in accurate amounts (N+items)
      nper <- nrow(dfin)
      nperp <- nper + 1
      nthr <- length(thresholds) + nper
      df.locations[1:nper, 1] <- paste0("Persons")
      df.locations[nperp:nthr, 1] <- paste0("Item thresholds")
      # insert data from vectors with thetas and thresholds
      df.locations$locations <- c(pthetas, thresholds)
      # change type to class factor
      df.locations$type <- as.factor(df.locations$type)

      # get mean/SD for item/person locations
      item.mean <- round(mean(as.matrix(item.locations), na.rm = TRUE), 2)
      item.sd <- round(sd(as.matrix(item.locations), na.rm = TRUE), 2)
      item.thresh.sd <- round(sd(as.matrix(item.locations), na.rm = TRUE), 2)
      person.mean <- round(mean(pthetas, na.rm = TRUE), 2)
      person.sd <- round(sd(pthetas, na.rm = TRUE), 2)

      targeting_plots <- list()

      # Person location histogram
      targeting_plots$p1 <- ggplot() +
        geom_histogram(
          data = subset(df.locations, type == "Persons"),
          aes(locations, fill = "Persons"),
          bins = bins
        ) +
        xlab("") +
        ylab("Persons") +
        scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
        geom_vline(xintercept = person.mean, color = "#0e4e65", linetype = 2) +
        annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean - person.sd), xmax = (person.mean + person.sd), alpha = .2) +
        theme_bw() +
        theme(legend.position = "none")

      # Item Threshold location histogram
      targeting_plots$p2 <- ggplot() +
        geom_histogram(
          data = subset(df.locations, type == "Item thresholds"),
          aes(locations, y = after_stat(count))
        ) +
        xlab("") +
        ylab("Thresholds") +
        scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
        scale_y_reverse() +
        geom_vline(xintercept = item.mean, color = "#e83c63", linetype = 2) +
        annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean - item.thresh.sd), xmax = (item.mean + item.thresh.sd), alpha = .2) +
        theme_bw() +
        theme(legend.position = "none")

      # make plot with each items thresholds shown as dots
      targeting_plots$p3 <- ggplot(itemloc.long, aes(x = names, y = par_values, label = thresholds, color = thresholds)) +
        geom_point() +
        geom_text(hjust = 1.2, vjust = 1) +
        scale_color_viridis_d(option = "H", end = 0.97) +
        scale_y_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
        coord_flip() +
        labs(y = "Location (logit scale)",
             x = "Items",
             caption = paste0(
               "Person location average: ", person.mean, " (SD ", person.sd, "), Item threshold location average: ",
               item.mean, " (SD ", item.thresh.sd, "). Sample size: ",nrow(dfin),"."
             )) +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0, face = "italic"),
              legend.position = "none")

    }
  } else if (model == "RM") {

    erm_out <- RM(dfin) # run RM model
    item.estimates <- coef(erm_out, "beta")*-1 # item coefficients

    item.locations <- as.data.frame(item.estimates)
    itemloc.long <- item.locations %>%
      rownames_to_column() %>%
      tidyr::separate(rowname, c(NA, "names"), sep = " ")

    ### create df for ggplot histograms
    # person locations
    pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
      as.data.frame() %>%
      pull(WLE)
    # item locations
    thresholds <- itemloc.long$item.estimates
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
    person.mean <- round(mean(pthetas, na.rm = TRUE),2)
    person.sd <- round(sd(pthetas, na.rm = TRUE),2)
    #provide column names
    colnames(pi.locations) <- c('','Mean', 'SD')
    pi.locations[1,1] <- "Items"
    pi.locations[1,2] <- round(mean(item_difficulty$Location),2)
    pi.locations[1,3] <- round(sd(item_difficulty$Location),2)
    pi.locations[2,1] <- "Persons"
    pi.locations[2,2] <- round(mean(pthetas, na.rm = TRUE),2)
    pi.locations[2,3] <- round(sd(pthetas, na.rm = TRUE),2)

    targeting_plots <- list()

    # Person location histogram
    targeting_plots$p1 <- ggplot() +
      geom_histogram(data=subset(df.locations, type=="Persons"),
                     aes(locations, fill="Persons"),
                     bins = bins
                     ) +
      xlab('') +
      ylab('Persons') +
      scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      geom_vline(xintercept = person.mean, color = "#0e4e65", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean-person.sd), xmax = (person.mean+person.sd), alpha = .2) +
      theme_bw() +
      theme(legend.position = 'none')

    # Item Threshold location histogram
    targeting_plots$p2 <- ggplot() +
      geom_histogram(data=subset(df.locations, type=="Item thresholds"),
                     aes(locations, y= after_stat(count))) +
      labs(x = "",
           y = "Items aggregated") +
      scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      scale_y_reverse() +
      geom_vline(xintercept = item.mean, color = "#e83c63", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean-item.sd), xmax = (item.mean+item.sd), alpha = .2) +
      geom_text(hjust = 1.2, vjust = 1) +
      theme_bw() +
      theme(legend.position = 'none')

    # Plot with each item's thresholds shown as dots
    targeting_plots$p3 <- ggplot(itemloc.long, aes(x = names, y = item.estimates, label = names, color = names)) +
      geom_point() +
      geom_text(hjust = 1.2, vjust = 1) +
      scale_y_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      theme_bw() +
      coord_flip() +
      labs(y = "Location (logit scale)",
           x = "Items",
           caption = paste0("Person location average: ", pi.locations[2,2], " (SD ", pi.locations[2,3],"), Item location average: ",
                            pi.locations[1,2], " (SD ", pi.locations[1,3], "). Sample size: ",nrow(dfin),"."
           )) +
      theme(plot.caption = element_text(hjust = 0, face = "italic"),
            legend.position = 'none')

  }

  if (output == "figure") {
    # combine plots together to create Wright map, and let the individual item threshold plot have some more space
    targeting_plots$p1 / targeting_plots$p2 / targeting_plots$p3 + plot_layout(heights = c(1, 1, 1.4))
  } else if (output == "list") {
    return(targeting_plots)
  }
}


#' Reliability of test
#'
#' Test information shows the reliability curve of the test (not the sample).
#'
#' Use option `samplePSI = TRUE` to add graphical and written representation of
#' the current sample's theta mean/SD, test information (TIF) mean/SD, and
#' Person Separation Index (PSI). According to Wright & Stone (1999), PSI is
#' calculated as \eqn{\frac{\mathrm{SSD}-\mathrm{MSE}}{\mathrm{SSD}}}{(SSD-MSE)/SSD},
#' see `?eRm::SepRel` for details. According to Embretson & Reise (2000),
#' PSI = 1 - SEM^2, and TIF = 1/SEM^2, and the values reported in
#' this function are based on sample average SEM.
#'
#' For reference:
#' TIF 2.5 corresponds to PSI 0.6
#' TIF 3.33 -> PSI 0.7
#' TIF 5 -> PSI 0.8
#' TIF 10 -> PSI 0.9
#'
#' @param dfin Dataframe with item data only
#' @param lo Lower limit of x axis (default = -5)
#' @param hi Upper limit of x axis (default = 5)
#' @param samplePSI Adds information about sample characteristics
#' @param cutoff Caption text will generate information relative to this TIF value
#' @param model Defaults to "PCM", use "RM" for dichotomous data
#' @export
RItif <- function(dfin, lo = -5, hi = 5, samplePSI = FALSE, cutoff = 3.33, model = "PCM") {
  # convert TIF to PSI, if cutoff is set manually
  psi_tif <- round(1-(1/sqrt(cutoff))^2,2)

  if (model == "PCM") {
    if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
      stop("Use `model = 'RM'` for dichotomous data.")
    } else {
      erm_out <- PCM(dfin)
      # item locations
      item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))

      # person locations
      pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
        as.data.frame() %>%
        pull(WLE)
      # item locations
      thresholds<-c()
      for (i in 1:ncol(item.locations)) {
        thresholds<-c(thresholds,item.locations[,i])
      }
    }
  }

  if (model == "RM") {
    erm_out <- RM(dfin)

    # item locations
    item.estimates <- coef(erm_out, "beta")*-1 # item coefficients
    item.locations <- as.data.frame(item.estimates) %>%
      rownames_to_column() %>%
      tidyr::separate(rowname, c(NA, "item"), sep = " ") %>%
      dplyr::rename(location = item.estimates)

    # person locations
    pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
      as.data.frame() %>%
      pull(WLE)
    # item locations
    thresholds <- item.locations$location
  }

  #create data frame with 0 rows and 2 columns
  df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
  #provide column names
  colnames(df.locations) <- c('type', 'locations')
  # change type of data
  df.locations$type <- as.character(df.locations$type)
  df.locations$locations <- as.numeric(df.locations$locations)
  # insert labels in accurate amounts (N+items)
  nper <- nrow(dfin)
  nperp <- nper + 1
  nthr <- length(thresholds) + nper
  df.locations[1:nper, 1] <- paste0("Persons")
  df.locations[nperp:nthr, 1] <- paste0("Item thresholds")
  # insert data from vectors with thetas and thresholds
  df.locations$locations<-c(pthetas,thresholds)
  # change type to class factor
  df.locations$type<-as.factor(df.locations$type)

  # we need to make a new dataframe for the test information plot/curve
  psimatrix <- data.frame(matrix(ncol = 2, nrow = 201))
  names(psimatrix) <- c("psY","psX")
  # this gets 1001 "dots" for the scale information variable y
  psimatrix$psY <- test_info(erm_out, seq(lo, hi, length.out = 201L))
  # this is the x variable in the TIF figure
  psimatrix$psX <- seq(lo, hi, length.out = 201L)

  # check if TIF goes above 3.3
  peak.tif <- psimatrix %>% slice(which.max(psY)) %>% dplyr::select(psY) %>% pull()

  if (peak.tif > cutoff - 0.01) {
    # Indicate which values are above below by a new variable with TRUE/FALSE
    psimatrix <- psimatrix %>%
      mutate(tif_above_cutoff = case_when(psY >= cutoff ~ TRUE,
                                          TRUE ~ FALSE))
    # now find where the cutoff points are for 3.33 on the theta (x) variable
    # this provides the highest and lowest value into two variables
    psep_min <- psimatrix %>%
      dplyr::filter(tif_above_cutoff == TRUE) %>%
      slice(which.min(psX)) %>%
      pull(psX)

    psep_max <- psimatrix %>%
      dplyr::filter(tif_above_cutoff == TRUE) %>%
      slice(which.max(psX)) %>%
      pull(psX)

    # calculate how many participants cross the cutoffs
    nCeilingRel<-length(which(pthetas > psep_max))
    nFloorRel<-length(which(pthetas < psep_min))
    nWithinRel<-(length(pthetas)-(nCeilingRel+nFloorRel))
    # Retrieve the lowest and highest item thresholds into vector variables
    min_thresh <- df.locations %>%
      dplyr::filter(type == "Item thresholds") %>%
      arrange(locations) %>%
      slice(1) %>%
      pull()
    max_thresh <- df.locations %>%
      dplyr::filter(type == "Item thresholds") %>%
      arrange(desc(locations)) %>%
      slice(1) %>%
      pull()

    # calculate how many participants cross the cutoffs
    nCeilingThresh<-length(which(pthetas > max_thresh))
    nFloorThresh<-length(which(pthetas < min_thresh))

    psep_caption <- paste0("Test Information ",cutoff, " (PSI = ",psi_tif,") is reached between ", round(psep_min,2), " and ", round(psep_max,2), " logits, where ",
                           round(nWithinRel/length(pthetas)*100,1), "% of the participants are located. \n",
                           round(nCeilingRel/length(pthetas)*100,1), "% of participants have locations above the area where the scale reaches TIF = ", cutoff,
                           " and ",
                           round(nFloorRel/length(pthetas)*100,1), "% are located below. \n",
                           round(nCeilingThresh/length(pthetas)*100,1), "% have person locations above the highest item threshold (",
                           round(max_thresh,2), ") and ", round(nFloorThresh/length(pthetas)*100,1), "% are below the lowest item threshold (",
                           round(min_thresh,2), ").")
  } else {
    psep_min = 0
    psep_max = 0
    psep_caption <- paste0("Test information is not above ",cutoff, " at any part of the scale.")
  }

  # make basic plot
  TIFplot <- ggplot(psimatrix) +
    geom_line(aes(x = psX, y = psY, group = 1), color = "black", linewidth = 1) +
    geom_hline(yintercept = 3.33, color = "#e83c63", linetype = 2, linewidth = 0.6) +
    geom_hline(yintercept = 5, color = "#e83c63", linetype = 2, linewidth = 0.6) +
    annotate("text", label = "PSI = 0.7", fontface = "italic",
             x = lo+0.2, y = 3.12,
             color = "#e83c63") +
    annotate("text", label = "PSI = 0.8", fontface = "italic",
             x = lo+0.2, y = 4.8,
             color = "#e83c63") +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    scale_x_continuous(breaks = seq(lo, hi, by = 1)) +
    labs(x = "Location (logit scale)", y = "Test information") +
    labs(caption = paste0(psep_caption)) +
    theme(plot.caption = element_text(hjust = 0, face = "italic")) +
    theme(
      panel.background = element_rect(fill = "#ebf5f0",
                                      colour = "#ebf5f0",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                      colour = "white")
    )

  ## Add PSI info for optional use

  # estimate person location/theta mean and SD
  person.locations.estimate <- person.parameter(erm_out)
  ple <- person.locations.estimate$theta.table %>%
    as.data.frame() %>%
    dplyr::filter(Interpolated == FALSE)
  pleMean <- mean(ple$`Person Parameter`)
  pleSD <- sd(ple$`Person Parameter`)

  # estimate person theta SE mean and sd
  ple.se <- person.locations.estimate$se.theta %>%
    as_tibble()
  pleSEmean <- round(mean(ple.se$NAgroup1),2)
  pleSEsd <- round(sd(ple.se$NAgroup1),2)
  # test information = 1/SE^2, and PSI = 1-SE^2
  ple.se <- ple.se %>%
    mutate(TIF = 1/NAgroup1^2,
           PSI = 1-NAgroup1^2)

  sampleTIFmean <- 1/pleSEmean^2
  sampleTIFsd <- sd(ple.se$TIF)
  sampleTIFse <- sampleTIFsd/sqrt(length(ple.se$NAgroup1))
  sampleTIFci95 <- sampleTIFse*1.96
  samplePSImean <- round(mean(ple.se$PSI),2)
  samplePSIsd <- sd(ple.se$PSI)
  samplePSIse <- samplePSIsd/sqrt(length(ple.se$NAgroup1))
  samplePSIci95 <- round(samplePSIse*1.96,2)
  ermpsi <- eRm::SepRel(person.locations.estimate)

  TIFplotPSI <- TIFplot +
    geom_segment(aes(x = pleMean-pleSD, xend = pleMean+pleSD, y = sampleTIFmean, yend = sampleTIFmean),
                 alpha = 0.9, color = "darkgrey", linetype = 1) +
    geom_errorbar(aes(x = pleMean, ymin = sampleTIFmean-sampleTIFsd, ymax = sampleTIFmean+sampleTIFsd),
                  alpha = 0.8, color = "darkgrey", linetype = 1, width = 0.2) +
    geom_point(aes(x = pleMean, y = sampleTIFmean,),
               size = 4, shape = 18, alpha = 0.8, color = "#e83c63") +
    annotate('label', label = glue("Characteristics of current sample:\n
                                      Person theta mean (red dot) and standard deviation (horizontal line)\n
                                      and TIF mean (dot) and SD (vertical line). SEM mean/SD is {pleSEmean}/{pleSEsd}.\n
                                      Person Separation Index (Wright & Stone, 1999) = {round(ermpsi$sep.rel,2)},\n
                                      (Embretson & Reise, 2000) = {samplePSImean}."),
             x = -2, y = 0.7, lineheight = .5, hjust = 0, vjust = 0.5,
             label.padding = unit(0.4, "lines"), alpha = 0.7)

  if (cutoff != 3.33 && samplePSI == FALSE) {
    TIFplot +
      geom_hline(yintercept = cutoff, color = "orange", linetype = 2, linewidth = 0.6) +
      annotate("text", label = paste0("PSI = ",psi_tif), fontface = "italic",
               x = lo+0.2, y = cutoff-0.15,
               color = "orange")
  }
  else if (cutoff == 3.33 && samplePSI == FALSE) {
    TIFplot
  } else if (cutoff == 3.33 && samplePSI == TRUE) {
    TIFplotPSI
  }
  else if (cutoff != 3.33 && samplePSI == TRUE) {
    TIFplotPSI +
      geom_hline(yintercept = cutoff, color = "orange", linetype = 2, linewidth = 0.6) +
      annotate("text", label = paste0("PSI = ",psi_tif), fontface = "italic",
               x = lo+0.2, y = cutoff-0.18,
               color = "orange")
  }
}


#' Person fit
#'
#' Outputs a histogram of person fit ZSTD and a plot with person fit ZSTD and
#' person location/score. Defaults to output a histogram and a hex heatmap.
#'
#' Optional grouped output with colorized points.
#'
#' You can also get a vector with row numbers for persons with infit ZSTD
#' over/under +/- 1.96 by using `output = "rowid"`. Or the full dataframe
#' with all respondents infit ZSTD and estimated theta values with
#' `output = "dataframe`.
#'
#' If you desire another cutoff than +/- 1.96, it can be set with `infit_lim`.
#'
#' Note: theta estimation is done using ML, which is not optimal but should
#' be sufficient for this analysis.
#'
#' @param dfin Dataframe with item data only
#' @param model Rasch model to use, "PCM" or "RM"
#' @param pointsize Size of datapoints for grouped view
#' @param alpha Transparency of points (0-1 where 1 = not transparent)
#' @param bins Number of bins for hexplot
#' @param group Optional grouping variable
#' @param output Can also be "rowid" for a dataframe with rownumbers
#' @param infit_lim Lower/upper limit for person infit ZSTD
#' @export
RIpfit <- function(dfin, model = "PCM", pointsize = 2.5, alpha = 0.5, bins = 30,
                   group, output = c("hist","heatmap"), infit_lim = c(-1.96,1.96)) {
  if (model == "PCM") {
    df.erm <- PCM(dfin)
  } else {
    df.erm <- RM(dfin)
  }
  person.locations.estimate <- person.parameter(df.erm)
  person.fit <- eRm::personfit(person.locations.estimate)
  thetas2 <- as.data.frame(person.locations.estimate$theta.table)

  nPfit <- length(person.fit$p.infitZ)
  nCeilingPfit <- length(which(person.fit$p.infitZ > infit_lim[2]))
  nFloorPfit <- length(which(person.fit$p.infitZ < infit_lim[1]))
  nPgoodfit <- (nPfit - (nCeilingPfit + nFloorPfit))

  # find highest/lowest fit and location to set limits for plots automatically
  xlim_max <- ceiling(max(person.fit$p.infitZ, na.rm = TRUE))
  xlim_min <- floor(min(person.fit$p.infitZ, na.rm = TRUE))
  ylim_max <- ceiling(max(thetas2$`Person Parameter`, na.rm = TRUE))
  ylim_min <- floor(min(thetas2$`Person Parameter`, na.rm = TRUE))

  if ("hist" %in% output) {
    hist(person.fit$p.infitZ, col = "#009ca6", xlim = c(xlim_min, xlim_max), xlab = "Person infit ZSTD", main = "Histogram of Person infit ZSTD")
  }
  # check whether there are excluded observations, and if found, adjust thetas2 df
  if (length(person.fit$excl_obs_num) > 0L) {
    thetas2[person.fit$excl_obs_num, ] <- NA
    thetas2 <- na.omit(thetas2)
  }

  df.pfit <- data.frame(
    p_locs = thetas2$`Person Parameter`,
    p_infit = person.fit$p.infitZ
  ) %>%
    rownames_to_column("rownumber") %>%
    mutate(rownumber = gsub(pattern = "P","", rownumber)) %>%
    mutate(rownumber = as.integer(rownumber)) %>%
    dplyr::rename(`Person locations` = p_locs,
                  `Person infit ZSTD` = p_infit)

  if ("rowid" %in% output) {
    rowid <- df.pfit %>%
      dplyr::filter(`Person infit ZSTD` > infit_lim[2] | `Person infit ZSTD` < infit_lim[1]) %>%
      pull(rownumber)
    return(rowid)
  }

  if ("dataframe" %in% output) {
    return(janitor::clean_names(df.pfit))
  }

  if (missing(group) && "heatmap" %in% output) {
    # figure
    df.pfit %>%
      ggplot(aes(x = `Person infit ZSTD`, y = `Person locations`, label = "")) +
      geom_vline(xintercept = infit_lim[1], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_vline(xintercept = infit_lim[2], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_hex(bins = bins, linewidth = 0.1, color = "darkgrey") +
      scale_fill_viridis_c('Count', option = "inferno", begin = 0.1) +
      scale_y_continuous(breaks = seq(ylim_min, ylim_max, by = 1)) +
      scale_x_continuous(breaks = seq(xlim_min, xlim_max, by = 1)) +
      labs(caption = paste0(
        round(nFloorPfit / nPfit * 100, 1), "% of participants have person infit ZSTD below ",infit_lim[1],", and ",
        round(nCeilingPfit / nPfit * 100, 1), "% are above ",infit_lim[1],". \nThus, ", round(nPgoodfit / nPfit * 100, 1),
        "% of participants without floor/ceiling effects are within infit ZSTD limits.\nNote: ",length(person.fit$excl_obs_num)," (",round(length(person.fit$excl_obs_num)/nrow(dfin)*100,1),"%) observations were excluded due to max/min score."
      )) +
      theme(plot.caption = element_text(hjust = 0, face = "italic")) +
      theme(
        panel.background = element_rect(
          fill = "#ebf5f0",
          colour = "#ebf5f0",
          linewidth = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_line(
          linewidth = 0.5, linetype = "solid",
          colour = "white"
        ),
        panel.grid.minor = element_line(
          linewidth = 0.25, linetype = "solid",
          colour = "white"
        )
      )
  }
  else if (!missing(group) && "heatmap" %in% output) {
    group[person.fit$excl_obs_num] <- NA # remove max/min scoring individuals from grouping variable
    df.pfit$grp <- na.omit(as.factor(group))
    df.pfit %>%
      ggplot(aes(x = `Person infit ZSTD`, y = `Person locations`, label = "", color = grp)) +
      geom_vline(xintercept = infit_lim[1], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_vline(xintercept = infit_lim[2], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_hex(bins = bins, linewidth = 0.5) +
      scale_color_brewer('Group', type = "qual", palette= "Dark2") +
      scale_fill_viridis_c('Count', option = "inferno", begin = 0.2) +
      scale_y_continuous(breaks = seq(ylim_min, ylim_max, by = 1)) +
      scale_x_continuous(breaks = seq(xlim_min, xlim_max, by = 1)) +
      labs(caption = paste0(
        round(nFloorPfit / nPfit * 100, 1), "% of participants have person infit ZSTD below ",infit_lim[1],", and ",
        round(nCeilingPfit / nPfit * 100, 1), "% are above ",infit_lim[1],". \nThus, ", round(nPgoodfit / nPfit * 100, 1),
        "% of participants without floor/ceiling effects are within infit ZSTD limits.\nNote: ",length(person.fit$excl_obs_num)," (",round(length(person.fit$excl_obs_num)/nrow(dfin)*100,1),"%) observations were excluded due to max/min score."
      ))  +
      facet_wrap(~grp) +
      guides(color = "none") +
      theme_rise(fontfamily = "Arial") +
      theme(strip.text = element_text(size = 12))
  }
}




#' Item parameters summary
#'
#' Displays a table with item threshold locations. Can also output a dataframe or
#' a CSV file. Set `detail = "all"` to get more detailed output.
#'
#' @param dfin Dataframe with item data only
#' @param fontsize Option to set font size for table
#' @param output Defaults to "table, can be set to "dataframe" or "file"
#' @param detail Set to "all" to get more detailed summary output
#' @param filename Name of file to save output to
#' @param tbl_width Width of table
#' @export
RIitemparams <- function(dfin, fontsize = 15, output = "table",
                         detail = "thresholds", filename = "item_params.csv",
                         tbl_width = 90) {
  erm_out <- PCM(dfin)
  item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))

  item_difficulty <- item.locations %>%
    mutate(Location = rowMeans(.), .before = `Threshold 1`) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  # detailed df
  item_params <- item_difficulty %>%
    mutate(all_item_avg = mean(Location)) %>%
    mutate(relative_avg_loc = Location - all_item_avg) %>%
    mutate(relative_lowest_tloc = `Threshold 1` - all_item_avg) %>%
    rownames_to_column("itemnr")

  # get the highest threshold value from each item - since the number of thresholds can vary, this needs special treatment
  highest_loc <- item_params %>%
    pivot_longer(cols = starts_with("Threshold"),
                 names_to = "threshold",
                 values_to = "t_location") %>%
    group_by(itemnr) %>%
    dplyr::filter(t_location == max(t_location, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::select(itemnr, t_location) %>%
    dplyr::rename(highest_tloc = t_location)

  # join the highest threshold location to the item_params df
  item_params <- item_params %>%
    left_join(highest_loc, by = "itemnr") %>%
    mutate(relative_highest_tloc = highest_tloc - all_item_avg) %>%
    dplyr::relocate(all_item_avg, .after = relative_highest_tloc) %>%
    dplyr::select(-highest_tloc) %>%
    as.data.frame()

  if (output == "file" & detail == "thresholds") {
    item_difficulty %>%
      dplyr::select(!Location) %>%
      set_names(paste0("threshold_", 1:ncol(.))) %>%
      write_csv(., file = filename)
  }
  else if (output == "file" & detail == "all") {
    item_params %>%
      write_csv(., file = filename)
  }
  else if (output == "dataframe" & detail == "thresholds") {
    return(item_difficulty)
  }
  else if (output == "dataframe" & detail == "all") {
    return(item_params)
  }
  else if (output == "table" & detail == "thresholds") {
    item_difficulty %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      dplyr::relocate(Location, .after = last_col()) %>%
      mutate(Location = cell_spec(Location, bold = T, align = "right")) %>%
      dplyr::rename('Item location' = Location) %>%
      kbl(booktabs = T, escape = F,
          table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      # for latex/PDF output
      kable_styling(latex_options = c("striped","scale_down")) %>%
      footnote(general = "Item location is the average of the thresholds for each item.")
  }
  else if (output == "table" & detail == "all") {
    item_params %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      mutate(Location = cell_spec(Location, bold = T, align = "right")) %>%
      dplyr::rename('Item location' = Location,
                    'Relative item location' = relative_avg_loc,
                    'Relative lowest threshold' = relative_lowest_tloc,
                    'Relative highest threshold' = relative_highest_tloc) %>%
      dplyr::select(!all_item_avg) %>%
      kbl(booktabs = T, escape = F,
          table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      kable_styling(latex_options = c("striped","scale_down")) %>%
      footnote(general = "Item location is the average of the thresholds for each item.
      Relative item location is the difference between the item location and the average of the item locations for all items.
               Relative lowest threshold is the difference between the lowest threshold and the average of all item locations.
               Relative highest threshold is the difference between the highest threshold and the average of all item locations.")
  }

}



##### Construct alley plots

#' Plot with infit ZSTD and item location
#' ZSTD is sample size sensitive, see "RIitemfitPCM"
#' for options
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended 250-500)
#' @param nsamples Desired number of samples (recommended range 10-30)
#' @export
RIinfitLoc <- function(dfin, samplesize, nsamples) {
  if(missing(samplesize)) {
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
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
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
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
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
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}


#' Plot with outfit ZSTD and item location
#' ZSTD is sample size sensitive, see "RIitemfitPCM"
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @export
RIoutfitLoc <- function(dfin, samplesize, nsamples) {
  if(missing(samplesize)) {
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
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
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
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
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
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}


#' Generates a plot showing the first residual contrast loadings based on a PCA
#' of Rasch model residuals vs item locations.
#'
#' Defaults to PCM, use `model = "RM"` for dichotomous data.
#'
#' Note. This function does not work with missing responses in the dataset.
#' You can temporarily remove respondents with missing data when running the
#' function, ie. `RIloadLoc(na.omit(df))`
#'
#' @param dfin Dataframe with item data only
#' @param output Either "figure" (default) or "dataframe"
#' @param model Defaults to "PCM", use "RM" for dichotomous data
#' @param pcx Number of principal components to output for "dataframe"
#' @export
#' @return A plot with item locations (y) and loadings (x)
RIloadLoc <- function(dfin, output = "figure", pcx = c("PC1","PC2","PC3"), model = "PCM") {

  if(model == "PCM") {
    if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
      stop("Use `model = 'RM'` for dichotomous data.")
    } else {
    erm_out <- PCM(dfin)
    item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
    item_difficulty <- item.locations %>%
      mutate(Location = rowMeans(., na.rm = TRUE), .before = `Threshold 1`) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    }

  } else if (model == "RM") {
    erm_out <- RM(dfin)
    item_difficulty <- as.data.frame(coef(erm_out, "beta")*-1)
    names(item_difficulty) <- "Location"
  }

  ple <- person.parameter(erm_out)
  item.fit <- eRm::itemfit(ple)
  std.resids <- item.fit$st.res

  pca2 <- prcomp(std.resids)
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

  if(output == "figure") {

    pcaloadings %>%
      rownames_to_column() %>%
      ggplot(aes(x=PC1, y=Location, label = rowname)) +
      geom_point(size = 3, color = "black") +
      xlab("Loadings on first residual contrast") +
      ylab("Item location (logit scale)") +
      geom_vline(xintercept = 0, color = "#e83c63", linetype = 2) +
      geom_hline(yintercept = 0, color = "#e83c63", linetype = 2) +
      scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, by = 0.25)) +
      scale_y_continuous(limits = ylims, breaks = ybreaks) +
      geom_text_repel() +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  } else {
    pcaloadings <- pcaloadings %>%
      rownames_to_column(var = "itemnr") %>%
      dplyr::select(itemnr,Location,all_of(pcx))
    return(pcaloadings)
  }
}


#' DIF PCM analysis - requires having set up dif.variables previously
#'
#' Makes use of the psychotree package, which also allows for interactions
#' between DIF variables, see `RIdifTable2()`.
#'
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTable(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @param table Set to TRUE = output a table object, and FALSE = dataframe `difTablePCM`
#' @export
RIdifTable <- function(dfin, dif.var, cutoff = 0.5, table = TRUE) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-pctree(difdata ~ dif.var, data = df.tree)

  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    plot(pctree.out)

    difTable <- itempar(pctree.out) %>% # identify the nodes to compare (see plot above)
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      mutate('Mean location' = rowMeans(.),
             StDev = rowSds(as.matrix(.))) %>%
      rowwise() %>%
      mutate(MaxDiff = (max(c_across(c(1:(ncol(.)-2))))) - min(c_across(c(1:(ncol(.)-2))))) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>%
      dplyr::relocate(MaxDiff, .after = last_col())
    if(table == TRUE) {

      formattable(difTable, list(
        'MaxDiff' =
          formatter("span", style = ~ style(color = ifelse(MaxDiff < -cutoff, "red",
                                                           ifelse(MaxDiff > cutoff, "red",  "black"))))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"')

      } else {

        difTablePCM <<- difTable
    }

  } else {
    print("No significant DIF found.")
  }
}

#' DIF PCM interaction analysis
#'
#' Makes use of the psychotree package. This function is for interaction between
#' two DIF variables
#'
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: `RIdifTable2(df, dif.age, dif.gender)`
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @export
RIdifTable2 <- function(dfin, dif.var1, dif.var2, cutoff = 0.5) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var1 <- dif.var1
  df.tree$dif.var2 <- dif.var2
  pctree.out<-pctree(difdata ~ dif.var1 + dif.var2, data = df.tree)

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
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>%
      dplyr::relocate(MaxDiff, .after = last_col()) %>%
      formattable(list(
        'MaxDiff' =
          formatter("span", style = ~ style(color = ifelse(MaxDiff < -cutoff, "red",
                                                           ifelse(MaxDiff > cutoff, "red",  "black"))))),
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
  pctree.par$Item <- factor(pctree.par$Item, levels = names(dfin)) # order items
    # make plot
  ggplot(pctree.par, aes(x=Item, y=Logits, color=Group, group = Group)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2.5)
  } else {
    print("No significant DIF found.")
  }
}

#' Create a DIF line graph over time, showing groups' PCM item locations
#'
#' This function is specifically intended for examining DIF over time, to
#' create a figure that follows each item location on the y axis
#' with time on the x axis.
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigTime <- function(dfin, dif.var) {
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
    pctree.par$Item <- factor(pctree.par$Item, levels = names(dfin))
    # make plot
    ggplot(pctree.par, aes(x=Group, y=Logits, color=Item, group = Item)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      xlab("DIF node/Time point (see DIF table)")
  } else {
    print("No significant DIF found.")
  }
}

#' Create a DIF line graph for item PCM thresholds
#'
#' Produces a panel of linegraphs showing item thresholds over DIF nodes.
#'
#' NOTE: only works with PCM data where all variables have multiple thresholds,
#' since the `threshpar()` function has problems when dichotomous data are
#' included.
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigThresh <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var <- dif.var
  pctree.out <- pctree(difdata ~ dif.var, data = df.tree)

  if (nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    # create dataframe for ggplot
    unidif <- threshpar(pctree.out) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Threshh") %>%
      pivot_longer(where(is.numeric)) %>%
      tidyr::separate(Threshh, c("Item", "Threshold"), sep = "-") %>%
      tidyr::separate(Item, c(NA, "Item"), sep = "ata") %>%
      dplyr::rename(
        "DIF node" = name,
        Location = value
      ) %>%
      mutate(`DIF node` = as.numeric(`DIF node`)) %>%
      mutate(Threshold = dplyr::recode(Threshold,"C1"="T1",
                                     "C2"="T2",
                                     "C3"="T3",
                                     "C4"="T4",
                                     "C5"="T5",
                                     "C6"="T6",
                                     "C7"="T7",
                                     "C8"="T8",
                                     "C9"="T9",
                                     "C10"="T10")) %>%
      mutate(Item = factor(Item, levels = names(dfin)))

    ggplot(unidif, (aes(
      x = factor(`DIF node`),
      y = Location,
      group = Threshold,
      color = Threshold
    ))) +
      geom_line() +
      geom_point() +
      xlab("DIF node") +
      facet_wrap(~Item)
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
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @export
RIdifTableRM <- function(dfin, dif.var, cutoff = 0.5) {
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
      mutate('Mean location' = rowMeans(.), StDev = rowSds(as.matrix(.))) %>%
      rowwise() %>%
      mutate(MaxDiff = (max(c_across(c(1:(ncol(.)-2))))) - min(c_across(c(1:(ncol(.)-2))))) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>%
      dplyr::relocate(MaxDiff, .after = last_col()) %>%
      formattable(list(
        'MaxDiff' =
          formatter("span", style = ~ style(color = ifelse(MaxDiff < -cutoff, "red",
                                                           ifelse(MaxDiff > cutoff, "red",  "black"))))),
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
      geom_line(linewidth = 1.5) +
      geom_point(size = 2.5)
  } else {
    print("No significant DIF found.")
  }
}

#' Create a figure showing items and thresholds (with CI)
#'
#' Items are sorted by item average location. Confidence intervals are 84% by
#' default to enable visual interpretation of statistically significant
#' differences (Payton et al., 2003). The CI can be changed using the
#' `sem_multiplier` option (ie. use 1.96 for 95% CI).
#'
#' Only works with partial credit models currently. For dichotomous data, use
#' `df.erm <- RM(data)` followed by `plotPImap(df.erm, sorted = T)`
#'
#' @param dfin Dataframe with item data only
#' @param numbers Display text in figure with item threshold locations
#' @param sem_multiplier For confidence intervals displayed in figure
#' @export
RIitemHierarchy <- function(dfin, numbers = TRUE, sem_multiplier = 1.405){

  if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
    stop("Dichotomous data currently not supported. See `?RIitemHierarchy` for workaround.")
  } else {

    erm_out <- PCM(dfin)
    item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
    item_difficulty <- item.locations %>%
      mutate(Location = rowMeans(.), .before = `Threshold 1`) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    names(item.locations) <- paste0("T", c(1:ncol(item.locations)))
    itemloc.long <- item.locations %>%
      rownames_to_column() %>%
      dplyr::rename(names = "rowname") %>%
      pivot_longer(
        cols = starts_with("T"),
        names_to = "thresholds",
        values_to = "par_values"
      ) %>%
      dplyr::rename(itemnr = names,
                    Threshold = thresholds,
                    Locations = par_values
      )
    # get SEM estimates for each threshold
    item.estimates <- eRm::thresholds(erm_out)
    itemSE <- as.data.frame(item.estimates[["se.thresh"]]) %>%
      rownames_to_column(var = 'itemThresh') %>%
      dplyr::rename(ThreshSEM = 'item.estimates[["se.thresh"]]')
    # long format dataframe with separate variables for item and threshold

    # vector of threshold names as "T1" etc
    Tthresh <- paste0("T",c(1:100))
    # vector with threshold names as "c1" etc (since eRm outputs these)
    names(Tthresh) <- paste0("c",c(1:100))
    # create df and recode c1 to T1, etc
    itemSE <- itemSE %>%
      tidyr::separate(itemThresh, c(NA,"itemThresh"), sep = "beta ") %>%
      tidyr::separate(itemThresh, c("itemnr","threshnr"), sep = "\\.") %>%
      mutate(Threshold = dplyr::recode(threshnr, !!!Tthresh)) %>%
      dplyr::select(!threshnr)

    # join all dataframes together
    itemLocs <- item_difficulty %>%
      rownames_to_column(var = "itemnr") %>%
      dplyr::select(!any_of(starts_with("Thresh"))) %>%
      left_join(itemloc.long, ., by = "itemnr") %>%
      left_join(., itemlabels, by = "itemnr") %>%
      dplyr::rename(itemDescr = item) %>%
      left_join(., itemSE, by = c("itemnr","Threshold"))

    # get order of items
    itemOrder <- itemLocs %>%
      arrange(Location) %>%
      distinct(itemnr) %>%
      pull()

    # and itemlabels in the same order
    itemLabels <- itemLocs %>%
      arrange(Location) %>%
      distinct(itemDescr) %>%
      pull()

    # use the ordering and create plot

    if(numbers == FALSE){
      itemLocs %>%
        mutate(Item = factor(itemnr, levels = itemOrder)) %>%
        ggplot(aes(x = Item, color = Threshold)) +
        geom_point(aes(y = Location),
                   size = 4,
                   shape = 18,
                   color = "black"
        ) +
        geom_point(aes(y = Locations),
                   position = position_nudge()) +
        geom_errorbar(aes(ymin = Locations - sem_multiplier*ThreshSEM, ymax = Locations + sem_multiplier*ThreshSEM),
                      width = 0.11
        ) +
        geom_text(aes(y = Locations, label = Threshold), hjust = 0.5, vjust = 1.4,
                  show.legend = FALSE) +
        geom_rug(aes(y = Locations), color = "darkgrey", sides = "l", length = unit(0.02, "npc")) +
        scale_x_discrete(labels = str_wrap(paste0(itemOrder, " - ", itemLabels), width = 36)) +
        coord_flip() +
        #scale_color_brewer(palette = "Dark2", guide = "none") +
        scale_color_viridis_d(guide = "none", option = "H") + # enables any number of thresholds to be colorized with good contrast between adjacent categories
        labs(caption = glue("Note. Item locations are indicated by black diamond shapes.
            Item threshold locations are indicated by colored dots and colored text.
            Horizontal error bars indicate 84% confidence intervals around threshold locations.")) +
        theme(plot.caption = element_text(hjust = 0, face = "italic"),
              legend.position = "none") +
        theme_bw()

    }
    else {
      itemLocs %>%
        mutate(Item = factor(itemnr, levels = itemOrder)) %>%
        ggplot(aes(x = Item, color = Threshold)) +
        geom_point(aes(y = Location),
                   size = 4,
                   shape = 18,
                   color = "black"
        ) +
        geom_point(aes(y = Locations),
                   position = position_nudge()) +
        geom_text(aes(y = Location, label = round(Location,2)),
                  hjust = 0.5, vjust = -1.3, color = "black", size = 3,
                  show.legend = FALSE) +
        geom_errorbar(aes(ymin = Locations - sem_multiplier*ThreshSEM, ymax = Locations + sem_multiplier*ThreshSEM),
                      width = 0.11
        ) +
        geom_text(aes(y = Locations, label = Threshold), hjust = 0.5, vjust = 1.4,
                  show.legend = FALSE) +
        geom_text(aes(y = Locations, label = round(Locations,2)), hjust = 0.5, vjust = -1.1, size = 3,
                  show.legend = FALSE) +
        geom_hline(aes(yintercept = mean(Location)),
                   linetype = "dashed",
                   color = "darkgrey") +
        geom_rug(aes(y = Locations), color = "darkgrey", sides = "l", length = unit(0.02, "npc")) +
        scale_x_discrete(labels = str_wrap(paste0(itemOrder, " - ", itemLabels), width = 36)) +
        coord_flip() +
        #scale_color_brewer(palette = "Dark2", guide = "none") +
        scale_color_viridis_d(guide = "none", option = "H") + # enables any number of thresholds to be colorized with good contrast between adjacent categories
        labs(caption = glue("Note. Item locations are indicated by black diamond shapes and black text.
            Item threshold locations are indicated by colored dots and colored text.
            Horizontal error bars indicate 84% confidence intervals around threshold locations.")) +
        theme(plot.caption = element_text(hjust = 0, face = "italic"),
              legend.position = "none") +
        theme_bw()
    }
  }
}

#' Raw sum score to logit score transformation table & figure
#'
#' By default displays a table with raw sum scores and their corresponding logit score
#' and logit standard error. Depends on functions from package `iarm`.
#'
#' Automatically chooses PCM or RM depending on data structure.
#'
#' Optional figure or dataframe output.
#'
#' NOTE: the figure uses `coord_flip()`, take this into account if you wish to add theming.
#'
#' @param data Dataframe with item data only
#' @param output Options: "table" (default), "figure", or "dataframe"
#' @param point_size Point size for figure
#' @param error_width Width of error bar ends for figure
#' @param error_multiplier Range of error bars to multiply with SEM
#' @param ... Options for `kbl_rise()` for table creation
#' @export
RIscoreSE <- function(data, output = "table", point_size = 3,
                      error_width = 0.5, error_multiplier = 1.96, ...) {
  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")

  } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- eRm::RM(data)
  } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- eRm::PCM(data)
  }

  scoreList <- RI_iarm_person_estimates(erm_out, properties = TRUE)
  scoreTable <- scoreList[[2]] %>%
    as.data.frame() %>%
    dplyr::select(`Raw Score`, WLE, SEM) %>%
    dplyr::rename(`Logit score` = WLE,
                  `Ordinal sum score` = `Raw Score`,
                  `Logit std.error` = SEM)
  rownames(scoreTable) <- NULL

  if (output == "table") {
    scoreTable %>%
      round(3) %>%
      kbl_rise(...)
  } else if (output == "dataframe") {
    return(scoreTable)
  } else if (output == "figure") {
    ggplot(scoreTable, aes(`Ordinal sum score`, `Logit score`)) +
      geom_errorbar(aes(ymin = `Logit score` - (error_multiplier * `Logit std.error`),
                        ymax = `Logit score` + (error_multiplier * `Logit std.error`)),
                    width = error_width, color = "darkgrey"
      ) +
      geom_point(size = point_size, shape = 18) +
      scale_x_continuous() +
      scale_y_continuous() +
      coord_flip() +
      labs(x = "Ordinal sum score",
           y = "Logit interval score") +
      theme_bw()
  }
}


#' Person location estimation (old version)
#'
#' NOTE: Does not work with dichotomous data
#'
#' Outputs a vector of person locations, one for each row in the dataframe.
#'
#' Uses thetaEst function from catR package to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#' Defaults to use WL estimation (lower bias than ML) and PCM.
#' See ?thetaEst for options available.
#'
#' A version for multi-core processing is available as `RIestThetasOLD2()`.
#'
#' @param dfin Dataframe with response data only (no demographics etc), items as columns
#' @param itemParams Optional item (threshold) location matrix
#' @param model Rasch model to use (currently only default PCM works)
#' @param method Estimation method (defaults to "WL")
#' @param theta_range Range of theta (person location) values
#' @export
RIestThetasOLD <- function(dfin, itemParams, model = "PCM", method = "WL",
                        theta_range = c(-7,7)) {

  # define function to call from purrr::map_dbl later.
  estTheta <- function(personResponse, itemParameters = itemParams, rmod = model,
                       est = method, rtheta = theta_range) {
    thetaEst(itemParameters, as.numeric(as.vector(personResponse)), model = rmod,
             method = est, range = rtheta)
  }
  # if no itemParams are given, calculate them based on input dataframe
  if (missing(itemParams) & model == "PCM") {
    erm_out <- PCM(dfin)
    itemParams <- thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as.data.frame() %>%
      map_dbl(., estTheta)

  } else if (missing(itemParams) & is.null(model)) {
    df.erm <- RM(dfin)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as.data.frame() %>%
      map_dbl(., estTheta)

  } else {

# Transpose dataframe to make persons to columns, then output a vector with thetas
  dfin %>%
    t() %>%
    as.data.frame() %>%
    map_dbl(., estTheta)
  }
}

#' Person location estimation
#'
#'
#' Outputs a dataframe of person locations and measurement error (SEM) for each person
#'
#' Uses `iarm::person_estimates()` to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#'
#' Defaults to use WLE estimation (lower bias than MLE, see Warm, 1989) and PCM.
#'
#' Note: If you want to use a pre-specified set of item parameters, please use
#' `RIestThetasOLD()`.
#'
#' @param dfin Dataframe with response data only (no demographics etc), items as columns
#' @param model Rasch model to use ("RM" for dichotomous data)
#' @param method Estimation method (defaults to "WLE")
#' @param theta_range Range of theta (person location) values
#' @export
RIestThetas <- function(data, model = "PCM", method = "WLE") {

  if (model == "PCM") {
    erm_out <- PCM(data)
  } else if (model == "RM") {
    erm_out <- RM(data)
  }

  thetas <- RI_iarm_person_estimates(erm_out, allperson = TRUE) %>%
    as.data.frame()

  semList <- RI_iarm_person_estimates(erm_out, properties = TRUE)

  if (method == "WLE") {
    theta_df <- data.frame(WLE = thetas$WLE)
    sem_df <- semList[[2]] %>%
      as.data.frame() %>%
      dplyr::select(WLE,SEM)

    theta_df <- left_join(theta_df,sem_df, by = "WLE")

    return(theta_df)

  } else if (method == "MLE") {
    theta_df <- data.frame(MLE = thetas$MLE)
    sem_df <- semList[[1]] %>%
      as.data.frame() %>%
      dplyr::select(MLE,SEM)

    theta_df <- left_join(theta_df,sem_df, by = "MLE")

    return(theta_df)
  }
}


#' Person location estimation with parallel processing
#'
#' Yields about 2-3x speed increase when using 4-8 CPU cores.
#' Requires `library(furrr)`
#'
#' NOTE: Does not yet work with dichotomous data
#'
#' Outputs a vector of person locations, one for each row in the dataframe.
#'
#' Uses thetaEst function from catR package to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#' Defaults to use WL estimation (lower bias than ML, see Warm, 1989) and PCM.
#' See ?thetaEst for options available.
#'
#' @param dfin Dataframe with response data only (no demographics etc), items as columns
#' @param itemParams Optional item (threshold) location matrix
#' @param model Rasch model to use (use `NULL` for dichotomous data)
#' @param method Estimation method (defaults to `"WL"`)
#' @param cpu Number of CPUs/cores to utilize (default is 4)
#' @param theta_range Range of theta (person location) values
#' @export
RIestThetasOLD2 <- function(dfin, itemParams, model = "PCM", method = "WL", cpu = 4,
                         theta_range = c(-7,7)) {
  library(furrr)
  plan(multisession, workers = cpu)
  # define function to call from purrr::map_dbl later.
  estTheta <- function(personResponse, itemParameters = itemParams, rmod = model,
                       est = method, rtheta = theta_range) {
    thetaEst(itemParameters, as.numeric(as.vector(personResponse)), model = rmod,
             method = est, range = rtheta)
  }
  # if no itemParams are given, calculate them based on input dataframe
  if (missing(itemParams) & model == "PCM") {
    erm_out <- PCM(dfin)
    itemParams <- thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., estTheta)

  } else if (missing(itemParams) & is.null(model)) {
    df.erm <- RM(dfin)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., estTheta)

  } else {

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., estTheta)
  }
}


#' DIF PCM analysis with table output for item locations
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a table with item
#' average locations, group differences, and standard errors.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableE(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param model Defaults to "PCM", optional "RM" under development
#' @param sort Set to TRUE to sort the table based on DIF size
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @param fontfamily Set table font
#' @export
RIdifTableLR <- function(dfin, dif.var, model = "PCM", sort = FALSE,
                         fontfamily = "sans-serif", cutoff = 0.5) {
  if (model == "PCM") {
    erm.out <- PCM(dfin)
    lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
    groups <- levels(droplevels(dif.var)) # remove unused factor levels
    nr.groups <- length(groups) # get number of subgroups

    # get item location for each subgroupx
    itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
      .[["threshpar"]] %>%
      as.data.frame() %>%
      rownames()

    lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
    for (i in 1:nr.groups){
      lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
        .[["threshpar"]] %>%
        as.data.frame(nm = groups[i]) %>%
        pull(groups[i])
    }
    lrt.locs <- setNames(lrt.locs, groups)
    lrt.locs$Item <- itemthresh

    # get thresholds from non-DIF-split model
    erm.par <- eRm::thresholds(erm.out)
    lrt.locs$All <- erm.par[["threshpar"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)

    # bind in one df
    lrt.diff <- lrt.locs %>%
      mutate_if(is.numeric, ~ round(.x, 3)) %>%
      tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
      tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
      mutate(Item = factor(Item, levels = names(dfin)))

    # add standard errors for all subgroups + whole group
    lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
    for (i in 1:nr.groups){
      lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
        .[["se.thresh"]] %>%
        as.data.frame(nm = "Location") %>%
        pull(Location)
    }
    lrt.se$All <- erm.par$se.thresh %>%
      as.data.frame(nm = "sem") %>%
      pull(sem)
    lrt.se <- setNames(lrt.se, c(groups,"All"))
    lrt.se$Item <- lrt.diff$Item
    lrt.se$Threshold <- lrt.diff$Threshold

    # make version with average item location
    lrt.avg <- lrt.diff %>%
      pivot_wider(names_from = "Item",
                  values_from = all_of(c(groups,"All")),
                  names_sep = ".") %>%
      t() %>%
      as.data.frame()
    lrt.avg <- lrt.avg[-1,]

    lrt.avg <- lrt.avg %>%
      mutate(across(everything(), ~ as.numeric(.x))) %>%
      mutate(Location = rowMeans(., na.rm = T)) %>%
      mutate_if(is.double, round, digits = 3) %>%
      rownames_to_column("groupitem") %>%
      tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

    ### add SE
    # pivot_wider for easier calculation
    lrt.avg.se <- lrt.se %>%
      pivot_wider(names_from = "Item",
                  values_from = all_of(c(groups,"All")),
                  names_sep = ".") %>%
      t() %>%
      as.data.frame()
    lrt.avg.se <- lrt.avg.se[-1,] # remove first row

    lrt.avg.se <- lrt.avg.se %>%
      mutate(across(everything(), ~ as.numeric(.x))) %>%
      mutate(SE = rowMeans(., na.rm = T)) %>%
      mutate_if(is.double, round, digits = 3) %>%
      rownames_to_column("groupitem") %>%
      tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

    lrt.avg$SE <- lrt.avg.se$SE

    # prepare table output
    lrt.table <- lrt.avg %>%
      dplyr::select(!starts_with("V")) %>%
      pivot_wider(
        names_from = "DIFgroup",
        values_from = c("Location", "SE")
      ) %>%
      dplyr::rename_with(.fn = ~ gsub("Location_","", .x),
                         .cols = contains("Location")) %>%
      rowwise() %>%
      mutate(MaxDiff = max(c_across(c(2:(nr.groups+1)))) - min(c_across(c(2:(nr.groups+1)))),
             .before = "All") %>%
      ungroup() %>%
      mutate_if(is.double, round, digits = 3) %>%
      mutate(MaxDiff = cell_spec(MaxDiff,
                                 color = case_when(
                                   MaxDiff > cutoff ~ "red",
                                   MaxDiff < -cutoff ~ "red",
                                   TRUE ~ "black"
                                 )
      )) %>%
      rowwise() %>%
      mutate(across(all_of(groups), ~ cell_spec(.x,
                                                background = case_when(
                                                  .x == max(c_across(c(2:(nr.groups+1)))) ~ "lightblue",
                                                  .x == min(c_across(c(2:(nr.groups+1)))) ~ "burlywood",
                                                  TRUE ~ ""
                                                ))))

    if (sort == TRUE) {
      lrt.table %>%
        arrange(desc(MaxDiff)) %>%
        kbl_rise() %>%
        column_spec(length(groups)+2,
                    bold = T) %>%
        column_spec(c(length(groups)+3,ncol(lrt.table)),
                    italic = T) %>%
        add_header_above(c(" " = 1, "Item locations" = nr.groups+2, "Standard errors" = nr.groups+1),
                         bold = T,
                         line_sep = 5) %>%
        footnote(general = paste0("Values highlighted in red are above the chosen cutoff ",
                                  cutoff,
                                  " logits. Background color brown and blue indicate the lowest and highest values among the DIF groups."))
    } else {

      lrt.table %>%
        kbl_rise() %>%
        column_spec(length(groups)+2,
                    bold = T) %>%
        column_spec(c(length(groups)+3,ncol(lrt.table)),
                    italic = T) %>%
        add_header_above(c(" " = 1, "Item locations" = nr.groups+2, "Standard errors" = nr.groups+1),
                         bold = T,
                         line_sep = 5) %>%
        footnote(general = paste0("Values highlighted in red are above the chosen cutoff ",
                                  cutoff,
                                  " logits. Background color brown and blue indicate the lowest and highest values among the DIF groups."))
    }
  } else if (model == "RM") {
    erm.out <- RM(dfin)
    lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
    groups <- levels(droplevels(dif.var)) # remove unused factor levels
    nr.groups <- length(groups) # get number of subgroups
  }

}

#' DIF PCM analysis with table output for item thresholds
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a table with item
#' average locations, group differences, and standard errors.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableThreshE(df, dif.age, fontfamily = "Arial")
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @param fontfamily Set table font
#' @export
RIdifThreshTblLR <- function(dfin, dif.var,
                             fontfamily = "sans-serif", cutoff = 0.5) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroupx
  itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
    .[["threshpar"]] %>%
    as.data.frame() %>%
    rownames()

  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["threshpar"]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  erm.par <- eRm::thresholds(erm.out)
  lrt.locs$All <- erm.par[["threshpar"]] %>%
    as.data.frame(nm = "Location") %>%
    pull(Location)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
    tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["se.thresh"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)
  }
  lrt.se$All <- erm.par$se.thresh %>%
    as.data.frame(nm = "sem") %>%
    pull(sem)
  lrt.se <- setNames(lrt.se, c(groups,"All"))
  lrt.se$Item <- lrt.diff$Item
  lrt.se$Threshold <- lrt.diff$Threshold

  # make version with average item location
  lrt.avg <- lrt.diff %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg <- lrt.avg[-1,]

  lrt.avg <- lrt.avg %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(Location = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  ### add SE
  # pivot_wider for easier calculation
  lrt.avg.se <- lrt.se %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg.se <- lrt.avg.se[-1,] # remove first row

  lrt.avg.se <- lrt.avg.se %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(SE = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  lrt.avg$SE <- lrt.avg.se$SE

  # prepare table
  lrt.plot <- lrt.diff %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "Location")
  lrt.plot.se <- lrt.se %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "SE")
  lrt.plot <- full_join(lrt.plot,lrt.plot.se, by = c("Item","Threshold","DIFgroup"))

  lrt.table <- lrt.plot %>%
    dplyr::select(!starts_with("V")) %>%
    pivot_wider(names_from = "DIFgroup",
                values_from = c("Location","SE")) %>%
    dplyr::rename_with(.fn = ~ gsub("Location_","", .x),
                       .cols = contains("Location")) %>%
    rowwise() %>%
    mutate(MaxDiff = max(c_across(c(3:(nr.groups+2)))) - min(c_across(c(3:(nr.groups+2)))),
           .before = "All") %>%
    ungroup() %>%
    mutate_if(is.double, round, digits = 3) %>%
    mutate(MaxDiff = cell_spec(MaxDiff,
                               color = case_when(
                                 MaxDiff > cutoff ~ "red",
                                 MaxDiff < -cutoff ~ "red",
                                 TRUE ~ "black"
                               )
    )) %>%
    rowwise() %>%
    mutate(across(all_of(groups), ~ cell_spec(.x,
                                              background = case_when(
                                                .x == max(c_across(c(3:(nr.groups+2)))) ~ "lightblue",
                                                .x == min(c_across(c(3:(nr.groups+2)))) ~ "burlywood",
                                                TRUE ~ ""
                                              ))))

  lrt.table %>%
    dplyr::select(!Item) %>%
    dplyr::rename(`Item threshold` = Threshold) %>%
    kbl_rise() %>%
    pack_rows(index = table(lrt.table$Item)) %>%
    column_spec(column = c(1,nr.groups+2),
                bold = T) %>%
    column_spec(column = c(nr.groups+3,(ncol(lrt.table)-1)),
                italic = T) %>%
    add_header_above(c(" " = 1, "Threshold locations" = nr.groups+2, "Standard errors" = nr.groups+1),
                     bold = T,
                     line_sep = 5) %>%
    footnote(general = paste0("Values highlighted in red are above the chosen cutoff ",
                              cutoff,
                              " logits. Background color brown and blue indicate the lowest and highest values among the DIF groups.")
    )

}


#' DIF PCM analysis with panel figure output for items' average locations
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a panel of figures
#' with item average locations and 95% confidence intervals.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableE(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigureLR <- function(dfin, dif.var) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroupx
  itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
    .[["threshpar"]] %>%
    as.data.frame() %>%
    rownames()

  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["threshpar"]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  erm.par <- eRm::thresholds(erm.out)
  lrt.locs$All <- erm.par[["threshpar"]] %>%
    as.data.frame(nm = "Location") %>%
    pull(Location)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
    tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["se.thresh"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)
  }
  lrt.se$All <- erm.par$se.thresh %>%
    as.data.frame(nm = "sem") %>%
    pull(sem)
  lrt.se <- setNames(lrt.se, c(groups,"All"))
  lrt.se$Item <- lrt.diff$Item
  lrt.se$Threshold <- lrt.diff$Threshold

  # make version with average item location
  lrt.avg <- lrt.diff %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg <- lrt.avg[-1,]

  lrt.avg <- lrt.avg %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(Location = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  ### add SE
  # pivot_wider for easier calculation
  lrt.avg.se <- lrt.se %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg.se <- lrt.avg.se[-1,] # remove first row

  lrt.avg.se <- lrt.avg.se %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(SE = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  lrt.avg$SE <- lrt.avg.se$SE

  ggplot(data = subset(lrt.avg, DIFgroup %in% groups),
         aes(
           x = factor(DIFgroup, levels = groups),
           y = Location,
           group = Item,
           color = Item
         )) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(y = Location, ymin = Location - 1.96*SE, ymax = Location + 1.96*SE),
                  width = 0.1
    ) +
    geom_point(data = subset(lrt.avg, DIFgroup == "All"),
               aes(x = (nr.groups+1)/2+0.15, y = Location),
               shape = 18,
               color = "black",
               size = 3,
               alpha = 0.6) +
    xlab("DIF node") +
    facet_wrap(~Item) +
    labs(title = "DIF for item location",
         subtitle = "Item locations are calculated as the mean of each item's threshold locations",
         caption = "Note. Error bars indicate 95% confidence interval.\nDark grey diamonds indicate item location for all participants as one group.",
         x = "DIF group",
         y = "Item location (logit scale") +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic"))

}

#' DIF PCM analysis with panel figure output for item thresholds
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a panel of figures
#' with item threshold locations and 95% confidence intervals.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableE(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifThreshFigLR <- function(dfin, dif.var) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroupx
  itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
    .[["threshpar"]] %>%
    as.data.frame() %>%
    rownames()

  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["threshpar"]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  erm.par <- eRm::thresholds(erm.out)
  lrt.locs$All <- erm.par[["threshpar"]] %>%
    as.data.frame(nm = "Location") %>%
    pull(Location)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
    tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["se.thresh"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)
  }
  lrt.se$All <- erm.par$se.thresh %>%
    as.data.frame(nm = "sem") %>%
    pull(sem)
  lrt.se <- setNames(lrt.se, c(groups,"All"))
  lrt.se$Item <- lrt.diff$Item
  lrt.se$Threshold <- lrt.diff$Threshold

  # make version with average item location
  lrt.avg <- lrt.diff %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg <- lrt.avg[-1,]

  lrt.avg <- lrt.avg %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(Location = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  ### add SE
  # pivot_wider for easier calculation
  lrt.avg.se <- lrt.se %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg.se <- lrt.avg.se[-1,] # remove first row

  lrt.avg.se <- lrt.avg.se %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(SE = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  lrt.avg$SE <- lrt.avg.se$SE

  lrt.plot <- lrt.diff %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "Location")
  lrt.plot.se <- lrt.se %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "SE")
  lrt.plot <- full_join(lrt.plot,lrt.plot.se, by = c("Item","Threshold","DIFgroup"))

  ggplot(data = subset(lrt.plot, DIFgroup %in% groups),
         aes(
           x = factor(DIFgroup, levels = groups),
           y = Location,
           group = Threshold,
           color = Threshold
         )) +
    geom_line() +
    geom_point(alpha = 0.9) +
    geom_errorbar(aes(y = Location, ymin = Location - 1.96*SE, ymax = Location + 1.96*SE),
                  width = 0.1
    ) +
    geom_errorbar(data = subset(lrt.plot, DIFgroup == "All"),
                  aes(x = (nr.groups+1)/2+0.15,
                      y = Location,
                      ymin = Location - 1.96*SE,
                      ymax = Location + 1.96*SE),
                  width = 0.1,
                  color = "darkgrey"
    ) +
    geom_point(data = subset(lrt.plot, DIFgroup == "All"),
               aes(x = (nr.groups+1)/2+0.15,
                   y = Location
               ),
               shape = 18,
               color = "black",
               size = 3,
               alpha = 0.6
    ) +
    xlab("DIF group") +
    facet_wrap(~Item) +
    labs(title = "Item threshold locations",
         caption = "Note. Error bars indicate 95% confidence interval.\nDark grey diamonds indicate item location for all participants as one group.") +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic"))

}

#' Get simulation based cutoff values for Yen's Q3 residual correlations
#'
#' Based on Christensen et al. (2017, DOI: 10.1177/0146621616677520).
#'
#' Uses a dataframe with response data to simulate residual correlation values
#' across n simulations based on estimated item & person locations.
#'
#' Results include mean, max and difference between the mean and max for each
#' iteration. Also, 95th and 99th percentile values are reported, and the latter is
#' recommended for use with `RIresidcorr()` as cutoff value, since the max value
#' seems spurious and reliant on number of iterations.
#'
#' Uses multi-core processing. To find how many cores you have on your computer,
#' use `parallel::detectCores()`. Remember to keep 1-2 cores free.
#'
#' @param data Dataframe with response data
#' @param iterations Number of simulation iterations (needed)
#' @param cpu Number of CPU cores to use
#' @export
RIgetResidCor <- function (data, iterations, cpu = 4) {

  require(doParallel)
  registerDoParallel(cores = cpu)

  # get sample size
  sample_n <- nrow(data)

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")

  } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {

    # get item threshold locations for response data
    item_locations <- RIitemparams(data, output = "dataframe") %>%
      dplyr::select(!Location) %>%
      janitor::clean_names() %>%
      as.matrix()

    n_items <- nrow(item_locations)

    # item threshold locations in list format for simulation function
    itemlist <- list()
    for (i in 1:n_items) {
      itemlist[[i]] <- list(na.omit(item_locations[i, ]))
    }

    # get number of response categories for each item for later use in checking complete responses
    itemlength <- list()
    for (i in 1:n_items) {
      itemlength[i] <- length(na.omit(item_locations[i, ]))
      names(itemlength)[i] <- names(data)[i]
    }

    # estimate theta values in response data
    thetas <- RIestThetas(data)

    # create object to store results from multicore loop
    residcor <- list()
    residcor <- foreach(icount(iterations)) %dopar%
      {
        # resampled vector of theta values (based on sample properties)
        inputThetas <- sample(thetas$WLE, size = sample_n, replace = TRUE)

        # simulate response data based on thetas and items above
        testData <- SimPartialScore(
          deltaslist = itemlist,
          thetavec = inputThetas
        ) %>%
          as.data.frame()

        names(testData) <- names(data)

        # check that simulated dataset has responses in all categories
        data_check <- testData %>%
          # make factor to not drop any consequtive response categories with 0 responses
          mutate(across(everything(), ~ factor(.x, levels = c(0:itemlength[[as.character(expression(.x))]])))) %>%
          pivot_longer(everything()) %>% # screws up factor levels, which makes the next step necessary
          dplyr::count(name, value, .drop = FALSE) %>%
          pivot_wider(
            names_from = "name",
            values_from = "n"
          ) %>%
          dplyr::select(!value) %>%
          # mark missing cells with NA for later logical examination
          mutate(across(everything(), ~ car::recode(.x, "0=NA", as.factor = FALSE))) %>%
          as.data.frame()

        # match response data generated with itemlength
        item_ccount <- list()
        for (i in 1:n_items) {
          item_ccount[i] <- list(data_check[c(1:itemlength[[i]]),i])
        }

        # check if any item has 0 responses in a response category that should have data
        if (any(is.na(unlist(item_ccount)))) {
          return("Missing cells in generated data.")
        }

        # create Yen's Q3 residual correlation matrix
        sink(nullfile())
        mirt.rasch <- mirt(testData, model = 1, itemtype = "Rasch")
        resid = residuals(mirt.rasch, type = "Q3", digits = 2)
        sink()
        diag(resid) <- NA

        data.frame(mean = mean(resid, na.rm = TRUE),
                   max = max(resid, na.rm = TRUE)
        )

      }

  } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    # estimate item threshold locations from data
    erm_out <- eRm::RM(data)
    item_locations <- erm_out$betapar * -1
    names(item_locations) <- names(data)

    # estimate theta values from data using WLE
    thetas <- RIestThetas(data, model = "RM")

    # create object to store results from multicore loop
    residcor <- list()
    residcor <- foreach(icount(iterations)) %dopar%
      {
        # resample vector of theta values (based on sample properties)
        inputThetas <- sample(thetas$WLE, size = sample_n, replace = TRUE)

        # simulate response data based on thetas and items above
        testData <-
          psychotools::rrm(inputThetas, item_locations, return_setting = FALSE) %>%
          as.data.frame()

        # check that all items have at least 4 "1" responses, otherwise eRm::RM() fails
        # n_4 <-
        #   testData %>%
        #   as.matrix() %>%
        #   colSums2() %>%
        #   t() %>%
        #   as.vector()
        #
        # if (min(n_4, na.rm = TRUE) < 5) {
        #   return("Missing cells in generated data.")
        # }

        # create Yen's Q3 residual correlation matrix
        sink(nullfile())
        mirt.rasch <- mirt(testData, model = 1, itemtype = "Rasch")
        resid = residuals(mirt.rasch, type = "Q3", digits = 2)
        sink()
        diag(resid) <- NA

        data.frame(mean = mean(resid, na.rm = TRUE),
                   max = max(resid, na.rm = TRUE)
        )

      }
  }

  # identify datasets with inappropriate missingness
  nodata <- lapply(residcor, is.character) %>% unlist()
  iterations_nodata <- which(nodata)

  actual_iterations = iterations - length(iterations_nodata)

  # get all results to a dataframe
  if (actual_iterations == iterations) {
    results <-
      bind_rows(residcor) %>%
      mutate(diff = max - mean)
  } else {
    results <-
      bind_rows(residcor[-iterations_nodata]) %>%
      mutate(diff = max - mean)
  }

  out <- list()
  out$results <- results
  out$actual_iterations <- actual_iterations

  out$sample_n <- sample_n
  out$sample_summary <- summary(thetas$WLE)

  out$max_diff <- max(results$diff)
  out$sd_diff <- sd(results$diff)
  out$p95 <- quantile(results$diff, .95)
  out$p99 <- quantile(results$diff, .99)

  return(out)
}

#' Calculate conditional outfit & infit MSQ statistics
#'
#' Automatically uses RM (dichotomous data) or PCM (polytomous data) depending
#' on data structure.
#'
#' Uses `iarm::out_infit()` to calculate conditional mean square fit statistics
#' for all items. See Müller (2020, DOI: 10.1186/s40488-020-00108-7) for details.
#' Only uses complete cases.
#'
#' Cutoff threshold values from simulation data (using option `simcut`) are
#' used with the `quantile()` function with .005 and .995 values to filter out
#' extremes. Actual cutoff values are shown in the output.
#'
#' Simulated datasets that have zero responses in any response category that
#' should have data will automatically be removed/skipped from analysis,
#' which means that final set of iterations may be lower than specified by user.
#'
#' Optional sorting (only) for table output with conditional highlighting, by
#' either `sort = "infit"` or `sort = "outfit`.
#'
#' @param data Dataframe with response data
#' @param simcut Object output from `RIgetfit()`
#' @param output Optional "dataframe" or "quarto"
#' @param sort Optional "infit" or "outfit"
#' @param ... Options passed on to `kbl_rise()` for table creation
#' @export
RIitemfit <- function(data, simcut, output = "table", sort = "items", ...) {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- eRm::RM(data)
  } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- eRm::PCM(data)
  }

  # get conditional MSQ
  cfit <- iarm::out_infit(erm_out)

  # create dataframe
  item.fit.table <- data.frame(InfitMSQ = cfit$Infit,
                               OutfitMSQ = cfit$Outfit) %>%
    round(3) %>%
    rownames_to_column("Item")

  if (!missing(simcut)) {

    # get number of iterations used to get simulation based cutoff values
    iterations <- length(simcut) - 2

    nodata <- lapply(simcut, is.character) %>% unlist()
    iterations_nodata <- which(nodata)

    actual_iterations <- iterations - length(iterations_nodata)

    # summarise simulations and set cutoff values
    if (actual_iterations == iterations) {
      lo_hi <-
        bind_rows(simcut[1:iterations]) %>%
        group_by(Item) %>%
        summarise(min_infit_msq = quantile(InfitMSQ, .005),
                  max_infit_msq = quantile(InfitMSQ, .995),
                  min_outfit_msq = quantile(OutfitMSQ, .005),
                  max_outfit_msq = quantile(OutfitMSQ, .995)
        )
    } else {
      lo_hi <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        group_by(Item) %>%
        summarise(min_infit_msq = quantile(InfitMSQ, .005),
                  max_infit_msq = quantile(InfitMSQ, .995),
                  min_outfit_msq = quantile(OutfitMSQ, .005),
                  max_outfit_msq = quantile(OutfitMSQ, .995)
        )
    }

    lo_hi$Item <- names(data)

    # get upper/lower values into a dataframe
    if (actual_iterations == iterations) {
      fit_table <-
        bind_rows(simcut[1:iterations]) %>%
        group_by(Item) %>%
        summarise(inf_thresh = paste0("[",round(quantile(InfitMSQ, .005),3),", ",round(quantile(InfitMSQ, .995),3),"]"),
                  outf_thresh = paste0("[",round(quantile(OutfitMSQ, .005),3),", ",round(quantile(OutfitMSQ, .995),3),"]")
        )
    } else {
      fit_table <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        group_by(Item) %>%
        summarise(inf_thresh = paste0("[",round(quantile(InfitMSQ, .005),3),", ",round(quantile(InfitMSQ, .995),3),"]"),
                  outf_thresh = paste0("[",round(quantile(OutfitMSQ, .005),3),", ",round(quantile(OutfitMSQ, .995),3),"]")
        )
    }
    # add thresholds to dataframe and calculate differences between thresholds and observed values
    item.fit.table <-
      item.fit.table %>%
      add_column(`Infit thresholds` = fit_table$inf_thresh, .after = "InfitMSQ") %>%
      add_column(`Outfit thresholds` = fit_table$outf_thresh, .after = "OutfitMSQ") %>%
      left_join(lo_hi, by = "Item") %>%
      mutate(infit_lo = abs(InfitMSQ - min_infit_msq),
             infit_hi = abs(InfitMSQ - max_infit_msq),
             `Infit diff` = round(pmin(infit_lo,infit_hi),3),
             outfit_lo = abs(OutfitMSQ - min_outfit_msq),
             outfit_hi = abs(OutfitMSQ - max_outfit_msq),
             `Outfit diff` = round(pmin(outfit_lo,outfit_hi),3)
      ) %>%
      mutate(`Infit diff` = ifelse(yes = "no misfit", no = `Infit diff`, InfitMSQ > min_infit_msq & InfitMSQ < max_infit_msq),
             `Outfit diff` = ifelse(yes = "no misfit", no = `Outfit diff`, OutfitMSQ > min_outfit_msq & OutfitMSQ < max_outfit_msq)) %>%
      dplyr::select(!contains(c("lo","hi","min","max")))

    if (output == "table" & sort == "items") {
      # set conditional highlighting based on cutoffs
      for (i in 1:nrow(lo_hi)) {
        item.fit.table[i,"OutfitMSQ"] <- cell_spec(item.fit.table[i,"OutfitMSQ"],
                                                   color = ifelse(item.fit.table[i,"OutfitMSQ"] < lo_hi[i,"min_outfit_msq"], "red",
                                                                  ifelse(item.fit.table[i,"OutfitMSQ"] > lo_hi[i,"max_outfit_msq"], "red", "black")))
        item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"],
                                                  color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                 ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
      }

      # output table
      item.fit.table %>%
        kbl_rise(...) %>%
        footnote(general = paste0("MSQ values based on conditional calculations (n = ", nrow(data),").
                                Simulation based thresholds based on ", actual_iterations," simulated datasets."))

    } else if (output == "table" & sort == "infit") {
      for (i in 1:nrow(lo_hi)) {
        item.fit.table[i,"OutfitMSQ"] <- cell_spec(item.fit.table[i,"OutfitMSQ"],
                                                   color = ifelse(item.fit.table[i,"OutfitMSQ"] < lo_hi[i,"min_outfit_msq"], "red",
                                                                  ifelse(item.fit.table[i,"OutfitMSQ"] > lo_hi[i,"max_outfit_msq"], "red", "black")))
        item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"],
                                                  color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                 ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
      }

      item.fit.table %>%
        arrange(desc(`Infit diff`)) %>%
        kbl_rise(...) %>%
        footnote(general = paste0("MSQ values based on conditional calculations (n = ", nrow(data),").
                                Simulation based thresholds based on ", actual_iterations," simulated datasets."))

    } else if (output == "table" & sort == "outfit") {
      for (i in 1:nrow(lo_hi)) {
        item.fit.table[i,"OutfitMSQ"] <- cell_spec(item.fit.table[i,"OutfitMSQ"],
                                                   color = ifelse(item.fit.table[i,"OutfitMSQ"] < lo_hi[i,"min_outfit_msq"], "red",
                                                                  ifelse(item.fit.table[i,"OutfitMSQ"] > lo_hi[i,"max_outfit_msq"], "red", "black")))
        item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"],
                                                  color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                 ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
      }
      item.fit.table %>%
        arrange(desc(`Outfit diff`)) %>%
        kbl_rise(...) %>%
        footnote(general = paste0("MSQ values based on conditional calculations and complete cases (n = ", nrow(na.omit(data)),").
                                Simulation based thresholds based on ", actual_iterations," simulated datasets."))

    }

    else if(output == "dataframe") {
      return(janitor::clean_names(item.fit.table))

    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
  } else if (missing(simcut)) {
    if (output == "table") {
      kbl_rise(item.fit.table) %>%
        footnote(general = paste0("MSQ values based on conditional estimation and complete cases (n = ", nrow(na.omit(data)),")."))

    } else if (output == "dataframe") {
      return(janitor::clean_names(item.fit.table))
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
  }
}


#' Get simulation based cutoff values for item fit values
#'
#' This function uses your response data to simulate datasets that fit the
#' Rasch model to find a credible range of item fit values. The function
#' outputs an object that is strongly recommended to save to an object, since it
#' takes some time to run this function when using many iterations/simulations.
#'
#' The output is a list object, which can in turn be used with two different
#' functions. Most importantly, you can use it with `RIitemfit()` to get
#' conditional highlighting of cutoff values based on your sample size and
#' item parameters. Each item gets its own cutoff thresholds.
#'
#' The function `RIgetfitPlot()` uses the package `ggdist` to plot the
#' distribution of fit values from the simulation results.
#'
#' Uses multi-core processing. To find how many cores you have on your computer,
#' use `parallel::detectCores()`. Remember to keep 1-2 cores free.
#'
#' @param data Dataframe with response data
#' @param iterations Number of simulation iterations (use at least 1000)
#' @param cpu Number of CPU cores to use
#' @export
RIgetfit <- function(data, iterations, cpu = 4) {
  sample_n <- nrow(data)

  require(doParallel)
  registerDoParallel(cores = cpu)

  if (missing(iterations)) {
    stop("Please set a number of iterations (at least 1000 is recommended).")
  }

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    # estimate item threshold locations from data
    erm_out <- eRm::RM(data)
    item_locations <- erm_out$betapar * -1
    names(item_locations) <- names(data)

    # estimate theta values from data using WLE
    thetas <- RIestThetas(data, model = "RM")

    fitstats <- list()
    fitstats <- foreach(icount(iterations)) %dopar% {
      # resampled vector of theta values (based on sample properties)
      inputThetas <- sample(thetas$WLE, size = sample_n, replace = TRUE)

      # simulate response data based on thetas and items above
      testData <-
        psychotools::rrm(inputThetas, item_locations, return_setting = FALSE) %>%
        as.data.frame()

      # TEMPORARY FIX START
      # check that all items have at least 8 positive responses, otherwise eRm::RM() fails
      n_resp <-
        testData %>%
        as.matrix() %>%
        colSums2() %>%
        t() %>%
        as.vector()

      if (min(n_resp, na.rm = TRUE) < 8) {
        return("Missing cells in generated data.")
      }
      # END TEMP FIX

      # get conditional MSQ
      rm_out <- eRm::RM(testData)
      cfit <- iarm::out_infit(rm_out)

      # create dataframe
      item.fit.table <- data.frame(
        InfitMSQ = cfit$Infit,
        OutfitMSQ = cfit$Outfit
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }
  } else if (max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
    # estimate item threshold locations from data
    item_locations <- RIitemparams(data, output = "dataframe") %>%
      dplyr::select(!Location) %>%
      janitor::clean_names() %>%
      as.matrix()

    n_items <- nrow(item_locations)

    # item threshold locations in list format for simulation function
    itemlist <- list()
    for (i in 1:n_items) {
      itemlist[[i]] <- list(na.omit(item_locations[i, ]))
    }

    # get number of response categories for each item for later use in checking complete responses
    itemlength <- list()
    for (i in 1:n_items) {
      itemlength[i] <- length(na.omit(item_locations[i, ]))
      names(itemlength)[i] <- names(data)[i]
    }

    # estimate theta values from data using WLE
    thetas <- RIestThetas(data)

    fitstats <- list()
    fitstats <- foreach(icount(iterations)) %dopar% {
      # resampled vector of theta values (based on sample properties)
      inputThetas <- sample(thetas$WLE, size = sample_n, replace = TRUE)

      # simulate response data based on thetas and items above
      testData <- SimPartialScore(
        deltaslist = itemlist,
        thetavec = inputThetas
      ) %>%
        as.data.frame()

      names(testData) <- names(data)

      # check that data has responses in all categories
      data_check <- testData %>%
        # make factor to not drop any consecutive response categories with 0 responses
        mutate(across(everything(), ~ factor(.x, levels = c(0:itemlength[[as.character(expression(.x))]])))) %>%
        pivot_longer(everything()) %>% # screws up factor levels, which makes the next step necessary
        dplyr::count(name, value, .drop = FALSE) %>%
        pivot_wider(
          names_from = "name",
          values_from = "n"
        ) %>%
        dplyr::select(!value) %>%
        # mark missing cells with NA for later logical examination with if(is.na)
        mutate(across(everything(), ~ car::recode(.x, "0=NA", as.factor = FALSE))) %>%
        as.data.frame()

      # match response data generated with itemlength
      item_ccount <- list()
      for (i in 1:n_items) {
        item_ccount[i] <- list(data_check[c(1:itemlength[[i]]), i])
      }

      # check if any item has 0 responses in a response category that should have data
      if (any(is.na(unlist(item_ccount)))) {
        return("Missing cells in generated data.")
      }

      # get conditional MSQ
      pcm_out <- psychotools::PCModel.fit(testData)
      cfit <- iarm::out_infit(pcm_out)

      # create dataframe
      item.fit.table <- data.frame(
        InfitMSQ = cfit$Infit,
        OutfitMSQ = cfit$Outfit
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }
  }

  fitstats$sample_n <- sample_n
  fitstats$sample_summary <- summary(thetas$WLE)

  return(fitstats)
}

#' Creates a plot with distribution of simulation based item fit values
#'
#' Uses the output from `RIgetfit()` as input. Uses `median_qi`
#' and `.width = c(.66,.99)` with `ggdist::stat_dotsinterval()`.
#'
#' @param simcut Output object from `RIgetfit()`
#' @param data Optional response dataframe for plotting observed item fit
#' @export
RIgetfitPlot <- function(simcut, data) {
  require(ggdist)

  # get number of iterations used to get simulation based cutoff values
  iterations <- length(simcut) - 2

  # check which iterations have incomplete data
  nodata <- lapply(simcut, is.character) %>% unlist()
  iterations_nodata <- which(nodata)
  # in case the first iteration does not have complete data
  first_iteration <- c(1:iterations)[-iterations_nodata][1]

  actual_iterations <- iterations - length(iterations_nodata)

  if (missing(data)) {
    # summarise simulation results
    if (actual_iterations == iterations) {
      results <- bind_rows(simcut[1:iterations]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value")
    } else {
      results <- bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value")
    }
    # plot
    results %>%
      ggplot(aes(x = Value, y = factor(Item, levels = rev(simcut[[first_iteration]][["Item"]])), slab_fill = after_stat(level))) +
      stat_dotsinterval(quantiles = iterations, point_interval = median_qi,
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.99)) +
      labs(x = "Conditional MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      labs(caption = str_wrap(paste0("Note: Results from ",actual_iterations," simulated datasets with ",
                                     simcut$sample_n," respondents."))
      ) +
      facet_wrap(~statistic, ncol = 2) +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal() +
      theme(panel.spacing = unit(0.7, "cm", data = NULL))
  }
  else if (!missing(data)) {
    if(min(as.matrix(data), na.rm = T) > 0) {
      stop("The lowest response category needs to coded as 0. Please recode your data.")
    } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
      erm_out <- RM(data)
    } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
      erm_out <- PCM(data)
    }

    # get conditional MSQ
    cfit <- iarm::out_infit(erm_out)
    #bfit <- iarm::boot_fit(erm_out, B = 100)

    # create dataframe with observed MSQ
    item.fit.table <- data.frame(InfitMSQ = cfit$Infit,
                                 OutfitMSQ = cfit$Outfit,
                                 InfitSE = cfit$Infit.se,
                                 OutfitSE = cfit$Outfit.se) %>%
      rownames_to_column("Item") %>%
      mutate(infit_ci_hi = InfitMSQ + (InfitSE * 1.96),
             infit_ci_lo = InfitMSQ - (InfitSE * 1.96),
             outfit_ci_hi = OutfitMSQ + (OutfitSE * 1.96),
             outfit_ci_lo = OutfitMSQ - (OutfitSE * 1.96)) %>%
      dplyr::select(!contains("SE"))

    observed <- item.fit.table %>%
      pivot_longer(contains("MSQ"),
                   names_to = "statistic",
                   values_to = "observed")

    # join simulated and observed MSQ
    if (actual_iterations == iterations) {
      infit <-
        bind_rows(simcut[1:iterations]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "InfitMSQ")
    } else {
      infit <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "InfitMSQ")
    }

    # and plot
    infit_p <-
      infit %>%
      ggplot(aes(x = Value, y = factor(Item, levels = rev(simcut[[first_iteration]][["Item"]])))) +
      stat_dotsinterval(aes(slab_fill = after_stat(level)),
                        quantiles = iterations, point_interval = median_qi,
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.99)) +
      geom_point(aes(x = observed),
                 color = "sienna2", shape = 18,
                 position = position_nudge(y = -0.1), size = 4) +
      # geom_segment(aes(x = infit_ci_lo, xend = infit_ci_hi),
      #              color = "sienna2",
      #              position = position_nudge(y = -0.1), linewidth = 0.2) +
      labs(x = "Conditional Infit MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      # labs(caption = str_wrap(paste0("Note: Results from ",iterations," simulated datasets with ",
      #                                simcut$sample_n," respondents\n(mean theta = ", round(simcut$sample_mean,2),", SD = ",round(simcut$sample_sd,2),")."))
      # ) +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal() +
      theme(panel.spacing = unit(0.7, "cm", data = NULL))

    if (actual_iterations == iterations) {
      outfit <-
        bind_rows(simcut[1:iterations]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "OutfitMSQ")
    } else {
      outfit <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "OutfitMSQ")
    }
    # and plot
    outfit_p <-
      outfit %>%
      ggplot(aes(x = Value, y = factor(Item, levels = rev(simcut[[first_iteration]][["Item"]])))) +
      stat_dotsinterval(aes(slab_fill = after_stat(level)),
                        quantiles = iterations, point_interval = median_qi,
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.99)) +
      geom_point(aes(x = observed),
                 color = "sienna2", shape = 18,
                 position = position_nudge(y = -0.1), size = 4) +
      # geom_segment(aes(x = outfit_ci_lo, xend = outfit_ci_hi),
      #              color = "sienna2",
      #              position = position_nudge(y = -0.1), linewidth = 0.2) +
      labs(x = "Conditional Outfit MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      labs(caption = str_wrap(paste0("Note: Results from ",actual_iterations," simulated datasets with ",
                                     simcut$sample_n," respondents.\n
                                     Orange diamond shaped dots indicate observed conditional item fit.")) # Orange dots and lines are observed MSQ and 95% CI.
      ) +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal() +
      theme(panel.spacing = unit(0.7, "cm", data = NULL))

    infit_p + outfit_p
  }
}



#' Temporary fix for upstream bug in `iarm::person_estimates()`
#'
#' To get `RIscoreSE()` working properly for cases with theta range up til
#' c(-10,10).
#'
#' code from package `iarm`
#' <https://github.com/cran/iarm/blob/master/R/Person-Fit.R>
#'
#' @param object Output from PCM() or RM()
#' @param properties All properties or not
#' @param allperson All respondents or not
#' @export

RI_iarm_person_estimates <- function(object, properties = F, allperson = F){
  if (!any("Rm"%in%class(object),class(object)%in%c("raschmodel","pcmodel"))) stop("object must be of class Rm, raschmodel or pcmodel!")
  if(class(object)[1]=="pcmodel") object$model <- "pcmodel"
  if(class(object)[1]=="raschmodel") object$model <- "raschmodel"
  if (object$model%in%c("raschmodel","pcmodel")) {X <- object$data
  } else {X <- object$X
  }
  if (object$model%in%c("RM","raschmodel")) {
    k <- dim(X)[2]
    if (object$model == "RM") coeff <- (-1)*coef(object)
    else coeff <- itempar(object)
    m <- k
    respm <- rbind(rep(0, k), lower.tri(matrix(1, k, k)) + diag(k))
  } else {
    if (object$model == "PCM"){
      coeff <- thresholds(object)[[3]][[1]][, -1]- mean(thresholds(object)[[3]][[1]][, -1], na.rm=T)
    } else {
      coeff <- coef(threshpar(object),type="matrix")
    }
    k <- dim(X)[2]
    mi <- apply(X, 2, max, na.rm = TRUE)
    m <- sum(mi)
    respm <- matrix(0, ncol = k, nrow = m + 1)
    respm[, 1] <- c(0:mi[1], rep(mi[1], nrow(respm) - mi[1] - 1))
    for (i in 2:k) respm[, i] <- c(rep(0, cumsum(mi)[i - 1] + 1), 1:mi[i], rep(mi[i], nrow(respm) - cumsum(mi)[i]  -1))
  }
  if (object$model=="pcmodel"){
    mode <- "PCM"
  } else {
    if (object$model=="raschmodel"){
      mode  <- "RM"
    } else {
      mode <- object$model
    }
  }
  mm <- cbind(0:m, RI_iarm_persons_mle(respm, coeff, model=mode, type = "MLE" )[, 1],
              RI_iarm_persons_mle(respm, coeff, model=mode, type="WLE")[,1])
  rownames(mm) <- rep(" ", m + 1)
  colnames(mm) <- c("Raw Score", "MLE", "WLE")
  if (allperson){
    properties <- F
    rv <- rowSums(X, na.rm = TRUE)
    mm <- mm[rv+1,]
    mm
  } else {
    if (properties == F) {
      mm
    } else {
      if (object$model%in%c("RM","raschmodel")){
        koeff <- as.list(coeff)
      } else {
        koeff <- lapply(as.list(as.data.frame(t(coeff))), function(x) cumsum(na.omit(x)))
      }
      gr <- elementary_symmetric_functions(koeff)[[1]]
      s.theta <- function(r){
        function(x){
          ((exp(x*(0:m))*gr)/as.vector(exp(x*(0:m))%*%gr))%*%(0:m) - r
        }
      }
      if (object$model%in%c("pcmodel","raschmodel")) mm[1, 2] <- NA else  mm[1, 2] <- person.parameter(object)$pred.list[[1]]$y[1]
      try(mm[1, 2] <- uniroot(s.theta(0.25), c(-10, 10))$root)
      mm[m + 1, 2] <- uniroot(s.theta(m - 0.25), c(-10, 10))$root # this is where the change was made
      rvec = 0:m
      pers_prop <- function(x, persons){
        pr <- (exp(x[2]*rvec)*gr)/as.vector(exp(x[2]*rvec)%*%gr)
        bias <- pr%*%persons - x[2]
        sem <- sqrt((persons - as.vector(pr%*%persons))^2%*%pr)
        rsem <- sqrt((persons - x[2])^2%*%pr)
        scoresem <- sqrt((rvec- x[1])^2%*%pr)
        c(SEM = sem, Bias = bias, RMSE = rsem, Score.SEM = scoresem)
      }
      result <- list(cbind(mm[, 1:2],t(apply(mm[, c(1, 2)], 1, pers_prop, persons = mm[, 2]))),
                     cbind(mm[, c(1,3)], t(apply(mm[, c(1, 3)], 1, pers_prop, persons = mm[, 3]))))
      result
    }
  }
}

#' Temporary fix for upstream bug in `iarm::person_estimates()`
#'
#' To get `RIscoreSE()` working properly for cases with theta range up til
#' c(-10,10).
#'
#' code from package `iarm`
#' <https://github.com/cran/iarm/blob/master/R/Person-Fit.R>
#'
#' @param respm temp
#' @param thresh temp
#' @param model temp
#' @param theta temp
#' @param type temp
#' @param extreme temp
#' @param maxit temp
#' @param maxdelta temp
#' @param tol temp
#' @param maxval temp
#' @export

RI_iarm_persons_mle <- function (respm, thresh, model=c("RM","PCM"), theta = rep(0, dim(respm)[1]),
                         type = c("MLE","WLE"), extreme=TRUE, maxit = 20, maxdelta = 3, tol = 1e-04, maxval = 9) {
  # thresh Matrix mit  Schwellenwerten  oder betas
  n <- dim(respm)[1]
  k <- dim(respm)[2]
  rv <- rowSums(respm, na.rm = TRUE)
  mode <- match.arg(model)
  typ <- match.arg(type)
  cll.rasch <- function(theta){
    ksi   <- exp(theta)
    mm <- outer(ksi,1/exp(thresh))
    mm[is.na(respm)] <- 0
    dll <- rv - rowSums(mm/(1+mm))
    d2ll <- - rowSums(mm/(1+mm)^2)
    d3ll <- 0
    if (typ=="WLE") {
      d3ll <- - rowSums((mm*(1-mm))/(1+mm)^3)
      if (extreme==FALSE){
        d3ll[rv==0] <- 0
        d3ll[rv==maxr] <- 0
      }
    }
    list(dll=dll,d2ll=d2ll,d3ll=d3ll)
  }
  cll.pcm <- function(theta){
    dlogki <-function(i) {
      mmn <- exp(outer(theta,1:mi[i]) + matrix(psi.l[[i]],ncol=mi[i],nrow=n,byrow=T))
      kd <- 1 + rowSums(mmn)
      kd1 <- rowSums(matrix(1:mi[i],ncol=mi[i],nrow=n,byrow=T)*mmn)
      kd2 <- rowSums(matrix((1:mi[i])^2,ncol=mi[i],nrow=n,byrow=T)*mmn)
      kd3 <- rowSums(matrix((1:mi[i])^3,ncol=mi[i],nrow=n,byrow=T)*mmn)
      cbind(dlli=kd1/kd, d2lli=kd2/kd -(kd1/kd)^2, d3lli=-kd3/kd + 3*kd2*kd1/(kd^2) -2*(kd1/kd)^3)
    }
    mm <- sapply(1:k,dlogki)
    mm[is.na(rbind(respm,respm,respm))] <- 0
    dll <- rv -rowSums(mm[1:n,])
    d2ll <- -rowSums(mm[(n+1):(2*n),])
    d3ll <- 0
    if (typ=="WLE") {
      d3ll <- rowSums(mm[(2*n+1):(3*n),])
      if (extreme==FALSE){
        d3ll[rv==0] <- 0
        d3ll[rv==maxr] <- 0
      }
    }
    list(dll=dll,d2ll=d2ll,d3ll=d3ll)
  }
  iter <- 1
  conv <- 1
  if (mode=="RM") {
    maxr <- apply(respm,1,function(x) sum(!is.na(x)))
    clog <- cll.rasch
  }
  if (mode=="PCM") {
    thresh.l <- apply(thresh,1,function(x) as.vector(na.omit(x)), simplify=F)
    psi.l <- lapply(thresh.l, function(x){(-1)*cumsum(x)})
    mi <- sapply(psi.l, length)
    m <- sum(mi)
    maxr <- apply(respm,1,function(x) sum(mi[!is.na(x)]))
    clog <- cll.pcm
  }

  while ((conv > tol) & (iter <= maxit)) {
    theta0 <- theta
    fn <- clog(theta)
    delta <- -fn[[1]]/fn[[2]]
    if (typ=="WLE") {
      delta <- -fn[[1]]/fn[[2]] - fn[[3]]/(2 * fn[[2]]^2)
    }
    maxdelta <- maxdelta/1.05
    delta <- ifelse(abs(delta) > maxdelta, sign(delta) * maxdelta, delta)
    theta <- theta + delta
    theta <- ifelse(abs(theta) > maxval, sign(theta) * maxval, theta)
    conv <- max(abs(theta - theta0))
    iter <- iter + 1
  }
  se <- sqrt(abs(-1/fn[[2]]))
  se <- ifelse(abs(theta) == maxval, NA, se)
  theta <- ifelse(theta == maxval, Inf, ifelse(theta == -maxval, -Inf, theta))
  res <- structure(data.frame(est = theta, se = se), model = mode, type = typ)
  return(res)
}
