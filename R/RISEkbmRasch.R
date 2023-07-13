### RISE KBM Rasch analysis package, https://github.com/pgmj/RISEkbmRasch
### Created by magnus.p.johansson@ri.se ORCID: 0000-0003-1669-592X
### The contents of this file is licensed according to
### Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/

### See .qmd files at https://github.com/pgmj/pgmj for use cases
### Link to vignette is available at the GitHub page.

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
theme_rise <- function(fontfamily = "Arial", axissize = 13, titlesize = 15,
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

#' A kableExtra function to simplify table code
#'
#' @param data Dataframe/tibble to create table from
#' @param width Width of table (0-100)
#' @param fontsize Font size
#' @param fontfamily Font family
#' @param ... Passes options to kbl()
#' @export
kbl_rise <- function(data, width = 65, fontsize = 14, fontfamily = "Arial",
                     options = c("striped", "hover"), ...) {
  kbl(data, booktabs = T, escape = F, table.attr = glue("style='width:{width}%;'")) %>%
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
#' @param ... Options for `theme_rise()`
#' @export
RImissing <- function(data, itemStart, ...) {

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
      geom_text(aes(label = glue("{round(Percentage,1)}%")),
                hjust = 1.5, vjust = 0.5,
                color = "white"
      ) +
      coord_flip() +
      ggtitle("Missing data per item") +
      xlab("Items") +
      ylab("Percentage of responses missing") +
      theme_minimal() +
      theme_rise(...)

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
    geom_text(aes(label = glue("{round(Percentage,1)}%")),
      hjust = 1.5, vjust = 0.5,
      color = "white"
    ) +
    coord_flip() +
    ggtitle("Missing data per item") +
    xlab("Items") +
    ylab("Percentage of responses missing") +
    theme_minimal() +
    theme_rise(...)
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
#' @param ... Options for `theme_rise()`
#' @export
RImissingP <- function(data, itemStart, output, n = 10, ...) {

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
          geom_text(aes(label = glue("{round(Missing*100/ncol(data),1)}%")),
                    hjust = 1.1, vjust = 0.5,
                    color = "white"
          ) +
          scale_y_continuous(breaks = seq(from = 0,
                                          to = max(.$Missing),
                                          by = 1),
                             labels = scales::number_format(accuracy = 1),
                             minor_breaks = NULL) +
          coord_flip() +
          ggtitle("Missing data per participant") +
          xlab("Participant rownumber") +
          ylab("Number of responses missing") +
          labs(caption = glue("Note. Total number of items is {ncol(data)}.")) +
          theme_minimal() +
          theme_rise(...)
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
          geom_text(aes(label = glue("{round(Missing*100/ncol(data),1)}%")),
                    hjust = 1.1, vjust = 0.5,
                    color = "white"
          ) +
          scale_y_continuous(breaks = seq(from = 0,
                                          to = max(.$Missing),
                                          by = 1),
                             labels = scales::number_format(accuracy = 1),
                             minor_breaks = NULL) +
          coord_flip() +
          ggtitle("Missing data per participant") +
          xlab("Participant rownumber") +
          ylab("Number of responses missing") +
          labs(caption = glue("Note. Total number of items is {ncol(data)}.")) +
          theme_minimal() +
          theme_rise(...)
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
#' @param dfin Dataframe with item data only
#' @export
RItileplot <- function(dfin) {
  dfin %>%
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
    geom_text(aes(label = n), colour = "orange")
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
      scale_fill_viridis_d("Category") +
      ggtitle("Item responses") +
      xlab("Number of responses")
  } else {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(name, value) %>%
      mutate(Item = factor(name, levels = rev(names(dfin))),
             value = factor(value)) %>%
      mutate(value = forcats::fct_rev(value)) %>%
      ggplot(aes(x = n, y = Item, fill = value)) +
      geom_col() +
      scale_fill_viridis_d("Category") +
      ggtitle("Item responses") +
      xlab("Number of responses") +
      labs(fill = "Category")
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
    relocate("0", .after = Item) %>%
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
      kbl(booktabs = T, escape = F, table.attr = "style='width:40%;'") %>%
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
#' conducts a PCA of residuals to get eigenvalues.
#'
#' @param dfin Dataframe with item data only
#' @param no.table Set to TRUE to avoid output of table
#' @param fontsize Set font size
#' @export
RIpcmPCA <- function(dfin, no.table, fontsize = 15) {
  if (missing(no.table)) {
    df.erm <- PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty <- as.data.frame(item_difficulty)
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
      dplyr::rename("Eigenvalues" = "value") %>%
      kbl(booktabs = T, escape = F, table.attr = "style='width:25%;'") %>%
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
    df.erm <- PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty <- as.data.frame(item_difficulty)
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
      kbl(booktabs = T, escape = F, table.attr = "style='width:25%;'") %>%
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
    xlab = "Person location",
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
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce
#' sample size and run analysis using multiple random samples to get average ZSTD
#' If you are using Quarto, the YAML execute setting "cache: yes" will be a
#' useful chunk option to speed things up. 50 samples seems to give stable
#' output, but 10 is probably sufficient for a quick look at the approximate
#' ZSTD statistics. It is recommended to use sample size 250-500, based on
#' Hagell & Westergren, 2016.
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 250-500)
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param fontsize Set fontsize for table
#' @param fontfamily Set font family for table
#' @param table Set to TRUE = table, and FALSE = dataframe `itemfitPCM`
#' @export
RIitemfitPCM <- function(dfin, samplesize, nsamples, zstd_min = -2, zstd_max = 2,
                         msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                         table = TRUE) {
  if (missing(samplesize)) {
    df.erm <- PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                          item.fit$i.infitMSQ,
                                          item.fit$i.outfitZ,
                                          item.fit$i.infitZ)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    colnames(item.fit.table) <- c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    if (table == TRUE) {
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
        kbl(booktabs = T, escape = F) %>%
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
        kable_styling(latex_options = c("striped", "scale_down"))
    } else {
      itemFitPCM <<- item.fit.table
    }
  } else {
    df.erm <- PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)

    # ZSTD multisample
    outfitZ <- c()
    infitZ <- c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ <- cbind(outfitZ, item.fit.z$i.outfitZ)
      infitZ <- cbind(infitZ, item.fit.z$i.infitZ)
    }
    item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                          item.fit$i.infitMSQ,
                                          rowMeans(outfitZ),
                                          rowMeans(infitZ))) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    colnames(item.fit.table) <- c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    if (table == TRUE) {
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
        kbl(booktabs = T, escape = F) %>%
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
        kable_styling(latex_options = c("striped", "scale_down"))
      } else {
        itemfitPCM <<- item.fit.table
      }
  }
}

#' Create table with Rasch PCM model item fit values for each item.
#'
#' Special version of RIitemfitPCM that utilizes multiple CPU cores to improve
#' performance. Requires `library(doParallel)`. To find how many cores you
#' have on your computer, use `parallel::detectCores()`, but remember to keep
#' some cores free.
#'
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce
#' sample size and run analysis using multiple random samples to get average ZSTD
#' 25 samples seems to give a stable output, but 10 is probably
#' sufficient for a reliable look at the approximate ZSTD statistics.
#' It is recommended #' to use sample size 250-500, based on
#' Hagell & Westergren, 2016.
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 250-500)
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param cpu Number of CPU cores to utilize (default = 4)
#' @param fontsize Set fontsize for table
#' @param fontfamily Set font family for table
#' @param table Set to FALSE if you want a dataframe instead of a table output
#' @export
RIitemfitPCM2 <- function(dfin, samplesize = 300, nsamples = 10, cpu = 4,
                          zstd_min = -2, zstd_max = 2, msq_min = 0.7,
                          msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                          table = TRUE) {
  library(doParallel)
  registerDoParallel(cores = cpu)
  df.erm <- PCM(dfin) # run PCM model
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)

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

  item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                        item.fit$i.infitMSQ,
                                        rowMeans(outfitZ),
                                        rowMeans(infitZ))) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  colnames(item.fit.table) <- c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

  if (table == TRUE) {
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
    kbl(booktabs = T, escape = F) %>%
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
    kable_styling(latex_options = c("striped", "scale_down"))
  } else {
    itemFitPCM <<- item.fit.table
  }
}

#' Create table with Rasch dichotomous model item fit values for each item.
#'
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce
#' sample size and run analysis using multiple random samples to get average ZSTD
#' If you are using Quarto/Rmarkdown, "cache: yes" will be a useful chunk option to
#' speed things up. 50 samples seems to give stable output, but 10 is probably
#' sufficient for a quick look at the approximate ZSTD statistics. It is recommended
#' to use sample size 250-500, based on Hagell & Westergren, 2016.
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
#' @param table Set to FALSE if you want a dataframe instead of a table output
#' @export
RIitemfitRM <- function(dfin, samplesize, nsamples, zstd_min = -2, zstd_max = 2,
                        msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                        table = TRUE) {
  if(missing(samplesize)) {
    df.erm <- RM(dfin) # run Rasch model
    # get estimates
    #item.estimates <- coef(df.erm, "eta") # item coefficients
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                        item.fit$i.infitMSQ,
                                        item.fit$i.outfitZ,
                                        item.fit$i.infitZ)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    colnames(item.fit.table) <- c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    if (table == TRUE) {

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
      kbl(booktabs = T, escape = F) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = fontfamily) %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
    } else {
      itemFitRM <<- item.fit.table
    }
  } else {
    df.erm <- RM(dfin) # run Rasch model
    # get estimates
    #item.estimates <- coef(df.erm, "eta") # item coefficients
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)

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
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ,
                                        item.fit$i.infitMSQ,
                                        rowMeans(outfitZ),
                                        rowMeans(infitZ))) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    colnames(item.fit.table) <- c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    if (table == TRUE) {

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
      kbl(booktabs = T, escape = F) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = fontfamily) %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped","scale_down"))
    } else {
      itemFitRM <<- item.fit.table
    }
  }
}


#' Correlation matrix of Rasch residuals
#'
#' Mandatory option to set relative cutoff-value over
#' the average of all item residual correlations (usually 0.2-0.3).
#'
#' @param dfin Dataframe with item data only
#' @param cutoff Relative value above the average of all item residual correlations
#' @param fontsize Set font size for table
#' @param fontfamily Set font family for table
#' @export
RIresidcorr <- function(dfin, cutoff, fontsize = 15, fontfamily = "Lato") {

  sink(nullfile()) # suppress output from the rows below

  mirt.rasch <- mirt(dfin, model = 1, itemtype = 'Rasch') # unidimensional Rasch model
  resid = residuals(mirt.rasch, type = "Q3", digits = 2) # get residuals

  sink() # disable suppress output

  diag(resid) <- NA # make the diagonal of correlation matrix NA instead of 1
  resid <- as.data.frame(resid)
  mean.resid <- resid %>%
    select_if(is.numeric) %>%
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
          table.attr = "style='width:70%;'") %>%
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
      footnote(general = paste0("Relative cut-off value (highlighted in red) is ", round(dyn.cutoff,3), ", which is ", cutoff, " above the average correlation."))
  } else {
    resid %>%
      mutate(across(everything(), ~ cell_spec(.x, color = case_when(.x > -dyn.cutoff ~ "red", TRUE ~ "black")))) %>%
      kbl(booktabs = T, escape = F,
          table.attr = "style='width:70%;'") %>%
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
                                round(dyn.cutoff,3),
                                ", which is ",
                                cutoff,
                                " above the average correlation."))
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
#' @param dfin Dataframe with item data only
#' @param dich Set to TRUE if your data is dichotomous
#' @param xlim Optionally, set lower/upper limits for x axis
#' @export
RItargeting <- function(dfin, dich = FALSE, xlim = c(-5,6)) {
  if(dich == FALSE) {
  df.erm <- PCM(dfin) # run PCM model
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty <- as.data.frame(item_difficulty)
  item.se <- item.estimates$se.thresh
  person.locations.estimate <- person.parameter(df.erm)
   item.fit <- eRm::itemfit(person.locations.estimate)

  item.locations <- item_difficulty[, 2:ncol(item_difficulty)]
  names(item.locations) <- paste0("T", c(1:ncol(item.locations))) # re-number items
  itemloc.long <- item.locations %>%
    rownames_to_column() %>%
    dplyr::rename(names = "rowname") %>%
    mutate(names = factor(names, levels = rev(names(dfin)))) %>%
    pivot_longer(
      cols = starts_with("T"),
      names_to = "thresholds",
      values_to = "par_values"
    )
  ### create df for ggplot histograms
  # person locations
  thetas <- as.data.frame(person.locations.estimate$theta.table)
  pthetas <- thetas$`Person Parameter`
  # item locations
  thresholds <- c()
  for (i in 2:ncol(item_difficulty)) {
    thresholds <- c(thresholds, item_difficulty[, i])
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
  pi.locations <- data.frame(matrix(ncol = 3, nrow = 3))

  item.mean <- round(mean(item_difficulty$Location), 2)
  item.sd <- round(sd(item_difficulty$Location), 2)
  item.thresh.sd <- item_difficulty %>%
    dplyr::select(starts_with("Threshold")) %>%
    pivot_longer(everything()) %>%
    pull() %>%
    na.omit() %>%
    sd() %>%
    round(2)
  person.mean <- round(mean(pthetas), 2)
  person.sd <- round(sd(pthetas), 2)
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
  p2 <- ggplot() +
    geom_histogram(
      data = subset(df.locations, type == "Persons"),
      aes(locations, fill = "Persons", y = after_stat(count))
    ) +
    xlab("") +
    ylab("Persons") +
    scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
    geom_vline(xintercept = person.mean, color = "#0e4e65", linetype = 2) +
    annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean - person.sd), xmax = (person.mean + person.sd), alpha = .2) +
    geom_text(hjust = 1.1, vjust = 1) +
    theme_bw() +
    theme(
      legend.position = "none",
      text = element_text(family = "sans")
    )

  # Item Threshold location histogram
  p3 <- ggplot() +
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
    geom_text(hjust = 1.1, vjust = 1) +
    theme_bw() +
    theme(legend.position = "none")

  # make plot with each items thresholds shown as dots
  p1 <- ggplot(itemloc.long, aes(x = names, y = par_values, label = thresholds, color = names)) +
    geom_point() +
    geom_text(hjust = 1.1, vjust = 1) +
    ylab("Location (logit scale)") +
    xlab("Items") +
    scale_y_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
    theme_bw() +
    theme(legend.position = "none") +
    coord_flip() +
    labs(caption = paste0(
      "Person location average: ", pi.locations[3, 2], " (SD ", pi.locations[3, 3], "), Item threshold location average: ",
      pi.locations[2, 2], " (SD ", pi.locations[2, 3], "). Sample size: ",nrow(dfin),"."
    )) +
    theme(plot.caption = element_text(hjust = 0, face = "italic"))

  # combine plots together to create Wright map, and let the individual item threshold plot have some more space
  plot_grid(p2,p3,p1, labels=NULL, nrow = 3, align ="hv", rel_heights = c(1,1,1.4))

  } else {

    df.erm <- RM(dfin) # run RM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    person.locations.estimate <- person.parameter(df.erm)
    item.estimates <- coef(df.erm, "beta")*-1 # item coefficients
    #item.fit <- eRm::itemfit(person.locations.estimate)

    item.locations <- as.data.frame(item.estimates)
    #names(item.locations) <- paste0("T", c(1:ncol(item.locations))) #re-number items
    itemloc.long <- item.locations %>%
      rownames_to_column() %>%
      separate(rowname, c(NA, "names"), sep = " ")

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
      scale_y_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      theme_bw() +
      theme(legend.position = 'none') +
      coord_flip() +
      labs(caption = paste0("Person location average: ", pi.locations[2,2], " (SD ", pi.locations[2,3],"), Item location average: ",
                            pi.locations[1,2], " (SD ", pi.locations[1,3], "). Sample size: ",nrow(dfin),"."
                            )) +
      theme(plot.caption = element_text(hjust = 0, face = "italic"))

    # Person location histogram
    p2<-ggplot() +
      geom_histogram(data=subset(df.locations, type=="Persons"),
                     aes(locations, fill="Persons", y= after_stat(count))) +
      xlab('') +
      ylab('Persons') +
      scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      geom_vline(xintercept = person.mean, color = "#0e4e65", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean-person.sd), xmax = (person.mean+person.sd), alpha = .2) +
      geom_text(hjust = 1.1, vjust = 1) +
      theme_bw() +
      theme(legend.position = 'none',
            text=element_text(family = "sans"))

    # Item Threshold location histogram
    p3 <- ggplot() +
      geom_histogram(data=subset(df.locations, type=="Item thresholds"),
                     aes(locations, y= after_stat(count))) +
      xlab('') +
      ylab('Items aggregated') +
      scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      scale_y_reverse() +
      geom_vline(xintercept = item.mean, color = "#e83c63", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean-item.sd), xmax = (item.mean+item.sd), alpha = .2) +
      geom_text(hjust = 1.1, vjust = 1) +
      theme_bw() +
      theme(legend.position = 'none')

    # combine plots together to create Wright map, and let the individual item threshold plot have some more space
    plot_grid(p2,p3,p1, labels=NULL, nrow = 3, align ="hv", rel_heights = c(1,1,1.4))

  }
}


#' Reliability of test
#'
#' Test information shows the reliability curve of the test (not the sample).
#' Use option `samplePSI = TRUE` to add graphical and written representation of
#' the current sample's theta mean/SD, test information (TIF) mean/SD , and
#' Person Separation Index (PSI). According to Wright & Stone (1999), PSI is
#' calculated as \eqn{\frac{\mathrm{SSD}-\mathrm{MSE}}{\mathrm{SSD}}}{(SSD-MSE)/SSD},
#' see `?eRm::SepRel` for details. According to Embretson & Reise (2000),
#' PSI = 1 - SEM^2, and TIF = 1/SEM^2, and the values reported in
#' this function are based on sample average SEM.
#'
#' @param dfin Dataframe with item data only
#' @param lo Lower limit of x axis (default = -5)
#' @param hi Upper limit of x axis (default = 5)
#' @param samplePSI Adds information about sample characteristics
#' @export
RItif <- function(dfin, lo = -5, hi = 5, samplePSI = FALSE) {
  df.erm <- PCM(dfin)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty <- as.data.frame(item_difficulty)
  person.locations.estimate <- person.parameter(df.erm)
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
  psimatrix <- data.frame(matrix(ncol = 2, nrow = 201))
  names(psimatrix) <- c("psY","psX")
  # this gets 1001 "dots" for the scale information variable y
  psimatrix$psY <- test_info(df.erm, seq(lo, hi, length.out = 201L))
  # this is the x variable in the TIF figure
  psimatrix$psX <- seq(lo, hi, length.out = 201L)

  # check if TIF goes above 3.3
  peak.tif <- psimatrix %>% slice(which.max(psY)) %>% dplyr::select(psY) %>% pull()

  if (peak.tif > 3.32) {
    # now find where the cutoff points are for 3.33 on the theta (x) variable
    # this provides the highest and lowest value into two variables
    psep_min <- psimatrix %>% dplyr::filter(psX < 0) %>% slice(which.min(abs(psY - 3.33))) %>% dplyr::select(psX) %>% pull()
    psep_max <- psimatrix %>% dplyr::filter(psX > 0) %>%  slice(which.min(abs(psY - 3.33))) %>% dplyr::select(psX) %>% pull()
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
    psep_caption <- paste0("Test Information 3.33 (PSI 0.7) is reached between ", round(psep_min,2), " and ", round(psep_max,2), " logits, where ",
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

  TIFplot <- ggplot(psimatrix) +
    geom_line(aes(x = psX, y = psY, group = 1), color = "black", linewidth = 1) +
    geom_hline(yintercept = 3.33, color = "#e83c63", linetype = 2, size = 0.5) +
    geom_hline(yintercept = 5, color = "#e83c63", linetype = 2, size = 0.7) +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    scale_x_continuous(breaks = seq(lo, hi, by = 1)) +
    labs(x = "Logits", y = "Test information") +
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
  if (samplePSI == FALSE) {
    TIFplot
  } else {
    # estimate person location/theta mean and SD
    ple <- person.locations.estimate$theta.table %>%
      as_tibble() %>%
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

    TIFplot +
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
  }
}

#' Person fit
#'
#' Outputs a histogram of person fit ZSTD and a plot with person fit ZSTD and
#' person location/score. Defaults to use a hex heatmap. Option to
#' display grouped output with colorized points.
#'
#' @param dfin Dataframe with item data only
#' @param model Rasch model to use, "PCM" or "RM"
#' @param pointsize Size of datapoints for grouped view
#' @param alpha Transparency of points (0-1 where 1 = not transparent)
#' @param bins Number of bins for hexplot
#' @param group Grouping variable. Needs to be a vector, such as DIF variables
#' @export
RIpfit <- function(dfin, model = "PCM", pointsize = 2.5, alpha = 0.5, bins = 30, group) {
  if (model == "PCM") {
    df.erm <- PCM(dfin)
  } else {
    df.erm <- RM(dfin)
  }
  person.locations.estimate <- person.parameter(df.erm)
  person.fit <- eRm::personfit(person.locations.estimate)
  thetas2 <- as.data.frame(person.locations.estimate$theta.table)

  nPfit <- length(person.fit$p.infitZ)
  nCeilingPfit <- length(which(person.fit$p.infitZ > 2))
  nFloorPfit <- length(which(person.fit$p.infitZ < -2))
  nPgoodfit <- (nPfit - (nCeilingPfit + nFloorPfit))

  hist(person.fit$p.infitZ, col = "#009ca6", xlim = c(-4, 6), xlab = "Person infit ZSTD", main = "Histogram of Person infit ZSTD")

  # check whether there are excluded observations, and if found, adjust thetas2 df
  if (length(person.fit$excl_obs_num) > 0L) {
    thetas2[person.fit$excl_obs_num, ] <- NA
    thetas2 <- na.omit(thetas2)
  }
  df.pfit <- data.frame(matrix(ncol = 2, nrow = nrow(thetas2)))
  # provide column names
  colnames(df.pfit) <- c("Person locations", "Person infit ZSTD")
  df.pfit$`Person locations` <- thetas2$`Person Parameter`
  df.pfit$`Person infit ZSTD` <- person.fit$p.infitZ

  if (missing(group)) {
  # figure
  df.pfit %>%
    ggplot(aes(x = `Person infit ZSTD`, y = `Person locations`, label = "")) +
    geom_vline(xintercept = -2, color = "#e83c63", linetype = 2, size = 0.7) +
    geom_vline(xintercept = 2, color = "#e83c63", linetype = 2, size = 0.7) +
    geom_hex(bins = bins) +
    scale_fill_viridis_c('Count', option = "inferno", begin = 0.1) +
    scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
    scale_x_continuous(breaks = seq(-5, 7, by = 1)) +
    labs(caption = paste0(
      round(nFloorPfit / nPfit * 100, 1), "% of participants have person infit ZSTD below -2.0, and ",
      round(nCeilingPfit / nPfit * 100, 1), "% are above 2.0. \nThus, ", round(nPgoodfit / nPfit * 100, 1), "% of participants without floor/ceiling effects are within +/- 2 infit ZSTD.\nNote: ",length(person.fit$excl_obs_num)," (",round(length(person.fit$excl_obs_num)/nrow(dfin)*100,1),"%) observations were excluded due to max/min score."
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
  else {
    group[person.fit$excl_obs_num] <- NA # remove max/min scoring individuals from grouping variable
    df.pfit$grp <- na.omit(group)
    df.pfit %>%
      ggplot(aes(x = `Person infit ZSTD`, y = `Person locations`, label = "", color = grp)) +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2, size = 0.7) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2, size = 0.7) +
      geom_point(size = pointsize, alpha = alpha) +
      scale_color_viridis_d('Group', begin = 0.3, end = 0.9, option = 7) +
      scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
      scale_x_continuous(breaks = seq(-5, 7, by = 1)) +
      labs(caption = paste0(
        round(nFloorPfit / nPfit * 100, 1), "% of participants have person infit ZSTD below -2.0, and ",
        round(nCeilingPfit / nPfit * 100, 1), "% are above 2.0. \nThus, ", round(nPgoodfit / nPfit * 100, 1), "% of participants without floor/ceiling effects are within +/- 2 infit ZSTD.\nNote: ",length(person.fit$excl_obs_num)," (",round(length(person.fit$excl_obs_num)/nrow(dfin)*100,1),"%) observations were excluded due to max/min score."
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
}


#' Item parameters summary
#'
#' Displays a table with item threshold locations
#' and exports a CSV file ("itemParameters.csv") to your working
#' directory. This file can be used for estimating person thetas/scores.
#'
#' @param dfin Dataframe with item data only
#' @param filename Optional specification of filename for item parameters output
#' @param fontsize Set font size for table
#' @export
RIitemparams <- function(dfin, filename = "itemParameters.csv", fontsize = 15) {
  df.erm <- PCM(dfin)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)
  item_difficulty %>%
    dplyr::select(!Location) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    write_csv(., file = filename)

  item_difficulty %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
    relocate(Location, .after = last_col()) %>%
    mutate(Location = cell_spec(Location, bold = T, align = "right")) %>%
    dplyr::rename('Item location' = Location) %>%
    kbl(booktabs = T, escape = F) %>%
    # bootstrap options are for HTML output
    kable_styling(bootstrap_options = c("striped", "hover"),
                  position = "left",
                  full_width = F,
                  font_size = fontsize,
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
#' Note. This function does not work with missing responses in the dataset.
#' You can temporarily remove respondents with missing data when running the
#' function, ie. `RIloadLoc(na.omit(df))`
#'
#' @param dfin Dataframe with item data only
#' @export
#' @return A plot with item locations (y) and loadings (x)
RIloadLoc <- function(dfin) {
  df.erm<-PCM(dfin) # run PCM model
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)

  item.se <- item.estimates$se.thresh # not used yet

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
    geom_point(size = 3, color = "black") +
    xlab("Loadings on first residual contrast") +
    ylab("Item location") +
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
      relocate(MaxDiff, .after = last_col())
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
      relocate(MaxDiff, .after = last_col()) %>%
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
      separate(Threshh, c("Item", "Threshold"), sep = "-") %>%
      separate(Item, c(NA, "Item"), sep = "ata") %>%
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
      relocate(MaxDiff, .after = last_col()) %>%
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

#' Create a figure showing items and thresholds (with 95% CI)
#'
#' Items are sorted by item location.
#'
#' Only works for PCM models currently. For dichotomous data, use
#' `df.erm<-RM(data)` followed by `plotPImap(df.erm, sorted = T)`
#'
#' @param dfin Dataframe with item data only
#' @param ci Show 95% CI (default) around each threshold location, or "none"
#' @export
RIitemHierarchy <- function(dfin, ci = "95"){
  df.erm <- PCM(dfin)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty<-as.data.frame(item_difficulty)

  item.locations<-item_difficulty[,2:ncol(item_difficulty)]
  names(item.locations) <- paste0("T", c(1:ncol(item.locations))) #re-number items

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
    separate(itemThresh, c(NA,"itemThresh"), sep = "beta ") %>%
    separate(itemThresh, c("itemnr","threshnr"), sep = "\\.") %>%
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

  if(ci == "none"){
    itemLocs %>%
      mutate(Item = factor(itemnr, levels = itemOrder)) %>%
      ggplot(aes(x = Item, color = Threshold)) +
      geom_point(aes(y = Locations)) +
      geom_text(aes(y = Locations, label = Threshold), hjust = 1, vjust = 1.3) +
      geom_point(aes(y = Location),
                 size = 4,
                 shape = 18,
                 color = "black") +
      theme(legend.position = "none") +
      scale_x_discrete(labels = str_wrap(paste0(itemOrder, " - ", itemLabels), width = 36)) +
      coord_flip() +
      labs(caption = str_wrap("Note. Item locations are indicated by black diamond shapes. Item threshold locations are indicated by colored dots.")) +
      theme(plot.caption = element_text(hjust = 0, face = "italic")) +
      scale_color_brewer(palette = "Dark2")

  }
  else {
    itemLocs %>%
      mutate(Item = factor(itemnr, levels = itemOrder)) %>%
      ggplot(aes(x = Item, color = Threshold)) +
      geom_point(aes(y = Locations),
                 position = position_nudge()) +
      geom_errorbar(aes(ymin = Locations - 1.96*ThreshSEM, ymax = Locations + 1.96*ThreshSEM),
                    width = 0.1
      ) +
      geom_text(aes(y = Locations, label = Threshold), hjust = 1, vjust = 1.3) +
      geom_point(aes(y = Location),
                 size = 4,
                 shape = 18,
                 color = "black"
      ) +
      theme(legend.position = "none") +
      scale_x_discrete(labels = str_wrap(paste0(itemOrder, " - ", itemLabels), width = 36)) +
      coord_flip() +
      labs(caption = str_wrap("Note. Item locations are indicated by black diamond shapes. Item threshold locations are indicated by colored dots.
                              Horizontal error bars indicate 95% confidence intervals around threshold locations.")) +
      theme(plot.caption = element_text(hjust = 0, face = "italic")) +
      scale_color_brewer(palette = "Dark2")
  }
}

#' Raw sum score to logit score transformation table
#'
#' Displays a table with raw sum scores and their corresponding logit score
#' and logit standard error.
#'
#' @param dfin Dataframe with item data only
#' @export
RIscoreSE <- function(dfin) {
  sink(nullfile()) # suppress output from the rows below
  ppar <- dfin %>%
    PCM() %>%
    person.parameter() %>%
    print() %>%
    as.data.frame()
  sink()

  ppar %>%
    #dplyr::select(!X1.Raw.Score) %>%
    dplyr::rename('Logit score' = 'X1.Estimate',
           'Logit std.error' = 'X1.Std.Error',
           'Ordinal sum score' = 'X1.Raw.Score') %>%
    remove_rownames() %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
    formattable(., table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 80%"')
}

#' Person location estimation
#'
#' NOTE: Does not yet work with dichotomous data
#'
#' Outputs a vector of person locations, one for each row in the dataframe.
#'
#' Uses thetaEst function from catR package to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#' Defaults to use WL estimation (lower bias than ML) and PCM.
#' See ?thetaEst for options available.
#'
#' @param dfin Dataframe with response data only (no demographics etc), items as columns
#' @param itemParams Optional item (threshold) location matrix
#' @param model Rasch model to use (use NULL for dichotomous data)
#' @param method Estimation method (defaults to "WL")
#' @export
RIestThetas <- function(dfin, itemParams, model = "PCM", method = "WL") {

  # define function to call from purrr::map_dbl later.
  estTheta <- function(personResponse, itemParameters = itemParams, rmod = model, est = method) {
    thetaEst(itemParameters, as.numeric(as.vector(personResponse)), model = rmod, method = est)
  }
  # if no itemParams are given, calculate them based on input dataframe
  if (missing(itemParams) & model == "PCM") {
    df.erm <- PCM(dfin)
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- as.data.frame(item.estimates[["threshtable"]][["1"]])
    item_difficulty$Location <- NULL
    itemParams <- item_difficulty %>%
      mutate_if(is.character, as.numeric) %>%
      as.matrix()

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as_tibble() %>%
      map_dbl(., estTheta)

  } else if (missing(itemParams) & is.null(model)) {
    df.erm <- RM(dfin)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as_tibble() %>%
      map_dbl(., estTheta)

  } else {

# Transpose dataframe to make persons to columns, then output a vector with thetas
  dfin %>%
    t() %>%
    as_tibble() %>%
    map_dbl(., estTheta)
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
#' Defaults to use WL estimation (lower bias than ML) and PCM.
#' See ?thetaEst for options available.
#'
#' @param dfin Dataframe with response data only (no demographics etc), items as columns
#' @param itemParams Optional item (threshold) location matrix
#' @param model Rasch model to use (use NULL for dichotomous data)
#' @param method Estimation method (defaults to "WL")
#' @param cpu Number of CPUs/cores to utilize (default is 4)
#' @export
RIestThetas2 <- function(dfin, itemParams, model = "PCM", method = "WL", cpu = 4) {
  library(furrr)
  plan(multisession, workers = cpu)
  # define function to call from purrr::map_dbl later.
  estTheta <- function(personResponse, itemParameters = itemParams, rmod = model, est = method) {
    thetaEst(itemParameters, as.numeric(as.vector(personResponse)), model = rmod, method = est)
  }
  # if no itemParams are given, calculate them based on input dataframe
  if (missing(itemParams) & model == "PCM") {
    df.erm <- PCM(dfin)
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- as.data.frame(item.estimates[["threshtable"]][["1"]])
    item_difficulty$Location <- NULL
    itemParams <- item_difficulty %>%
      mutate_if(is.character, as.numeric) %>%
      as.matrix()

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as_tibble() %>%
      future_map_dbl(., estTheta)

  } else if (missing(itemParams) & is.null(model)) {
    df.erm <- RM(dfin)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as_tibble() %>%
      future_map_dbl(., estTheta)

  } else {

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    dfin %>%
      t() %>%
      as_tibble() %>%
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
#' @param sort Set to TRUE to sort the table based on DIF size
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @param fontfamily Set table font
#' @export
RIdifTableLR <- function(dfin, dif.var, sort = FALSE,
                         fontfamily = "sans-serif", cutoff = 0.5) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroup
  itemthresh <- lrt.out[["betalist"]][[1]] %>%
    as.data.frame() %>%
    rownames()
  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- lrt.out[["betalist"]][[i]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  lrt.locs$All <- erm.out[["betapar"]] %>%
    as.data.frame(nm = "All") %>%
    pull(All)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ .x * -1) %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    separate(Item, c(NA,"Item"), sep = " ") %>%
    separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- lrt.out$selist[[i]]
  }
  lrt.se$All <- erm.out$se.beta
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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
  lrt.out <- LRtest(erm.out, splitcr = dif.var)
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroup
  itemthresh <- lrt.out[["betalist"]][[1]] %>%
    as.data.frame() %>%
    rownames()
  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- lrt.out[["betalist"]][[i]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  lrt.locs$All <- erm.out[["betapar"]] %>%
    as.data.frame(nm = "All") %>%
    pull(All)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ .x * -1) %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    separate(Item, c(NA,"Item"), sep = " ") %>%
    separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- lrt.out$selist[[i]]
  }
  lrt.se$All <- erm.out$se.beta
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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
    add_header_above(c(" " = 1, "Treshold locations" = nr.groups+2, "Standard errors" = nr.groups+1),
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
  groups <- levels(droplevels(dif.var))
  nr.groups <- length(groups)

  # get item location for each subgroup
  itemthresh <- lrt.out[["betalist"]][[1]] %>%
    as.data.frame() %>%
    rownames()
  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- lrt.out[["betalist"]][[i]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  lrt.locs$All <- erm.out[["betapar"]] %>%
    as.data.frame(nm = "All") %>%
    pull(All)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ .x * -1) %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    separate(Item, c(NA,"Item"), sep = " ") %>%
    separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- lrt.out$selist[[i]]
  }
  lrt.se$All <- erm.out$se.beta
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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
         y = "Item location") +
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
  groups <- levels(droplevels(dif.var))
  nr.groups <- length(groups)

  # get item location for each subgroup
  itemthresh <- lrt.out[["betalist"]][[1]] %>%
    as.data.frame() %>%
    rownames()
  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- lrt.out[["betalist"]][[i]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  lrt.locs$All <- erm.out[["betapar"]] %>%
    as.data.frame(nm = "All") %>%
    pull(All)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ .x * -1) %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    separate(Item, c(NA,"Item"), sep = " ") %>%
    separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- lrt.out$selist[[i]]
  }
  lrt.se$All <- erm.out$se.beta
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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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
    separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

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

