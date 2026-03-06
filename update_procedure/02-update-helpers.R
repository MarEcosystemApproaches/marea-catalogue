# helper functions for auto-generating marea catalogue indicator pages

library(marea)

# create marea page -------------------------------------------------------
create_marea_page <- function(indicator){


  # run some checks ---------------------------------------------------------
  # check if page exists
  path <- paste0("chapters/",indicator,"_template",".Rmd")

  # check if indicator exists
  in_pkg <- indicator %in% marea_metadata()$Dataset
  if(!in_pkg){
    stop("Indicator is not in package. Use create_marea_template().")
  }

  # check if there's already a page for indicator
  if (file.exists(path)) {

    cat("Warning: Chapter template already exists.\n",
        "File: ", path, "\n",
        "Do you want to proceed?\n",
        "The existing chapter file will be lost.")
    ans <- readline(("Type 'y' to overwrite or 'n' to cancel: "))

    # stop if answer isn't y or n
    if(!ans %in% c("y","n")){
      stop("Answer must be 'y' or 'n' ", call. = FALSE)
    }

    # stop if answer is n
    if(ans == c("n")){
      message("Cancelled. Existing chapter kept: ", path)
      return(invisible(FALSE)) # end function
    }

    # if yes, print that the chapter will be overwritten
    message("Overwriting existing chapter: ", path)

  } # end check file exists


  # get indicator data from marea
  data <- get(data(list = indicator,package = "marea"))
  indicator_name <- data@meta$data_type
  indicator_datatype <-  ifelse('sf' %in% class(data@data), 'Spatial Data','Tabular Data')
  indicator_scope <- data@meta$region



  # template pieces ---------------------------------------------------------
  ## 1. chunk to upload indicator data ---------
  upload_data_chunk <- c(
    "",
    sprintf("```{r setup-%s, include=FALSE}", indicator),
    "library(marea)",
    "library(dplyr)",
    "knitr::opts_chunk$set(echo = F, message = F)",
    sprintf('indicator <- "%s"', indicator),
    "",
    "# get metadata & data for this variable",
    "data <- get(data(list = indicator, package = 'marea'))",
    "```","","",""
  )

  ## 2. chunk for page header: inputs name and metadata ----------
  header_chunk <- c(
    "<!------- Header Section -------------->",

    # Title
    "<!-- Title -->",
    sprintf("# %s", indicator_name),"",

    # Metadata
    "<!-- Metadata -->",
    sprintf("**Data Type:** %s", indicator_datatype),"",
    # "**Data Type:** `r ifelse('sf' %in% class(data@data), 'Spatial Data','Tabular Data')` ","",
    sprintf("**Spatial Scope:** %s", indicator_scope),"",
    # "**Spatial Scope:** `r data@meta$region` ","",
    "**Duration** `r marea_metadata() %>% filter(Dataset == indicator) %>% pull(TimeSpan)`","",
    "**Source:** `r data@meta$source_citation` ","",
    "**Contact:** <!--ADD CONTACT-->","","",""
  )


  ## 3. Intro chunk placeholder -----------------------
  intro_chunk <- c(
    "<!----------- Intro Section -------------->",
    "## Introduction to Indicator",
    "<!-- ADD DESCRIPTION -->","","",""
    )


  ## 4. Plot data chunk --------------------------------
  plot_chunk <- c(
    "<!----------- Plots Section -------------->",
    "## View Data",
    "<!-- USE DEFAULT MAREA PLOT OR REPLACE WITH CUSTOM -->",
    sprintf("```{r plot-%s, echo=TRUE}", indicator),
    sprintf("plot(%s, style = 'default')",indicator),
    "```","","",""
  )


  ## 5. Calculate trends chunk
  # see if data have a region value, which will determine which trends template we use.
  has_region <- ifelse("region" %in% colnames(data@data),"yes","no")
  if(has_region == "yes")
    has_region <- ifelse(length(unique(data@data$region)) > 1, "yes","one")


  # make data chunk for no region datasets ----------------------------
  trends_chunk_no_regions <- c(
    "<!----------- Trends Section -------------->",
    "## Trends",
    "<!-- ADD SPECIFICATION TO DEFAULTS -->","",

    sprintf("```{r calc-trends-%s, include=F}",indicator),"",

    "# assess dataset",
    "# find the most recent recoded value, and its relation to broader data:",
    "last_row <- last(data@data)",
    'value_col <- colnames(data@data)[first(which(stringr::str_detect(colnames(data@data), "value")))]',
    'has_months <- "month" %in% colnames(last_row)',
    'has_regions <- "region" %in% colnames(last_row)',"",

    "# calculate most recent value",
    'last_date <- ifelse(has_months,
                        paste0(last_row$month,"/",last_row$year),
                        paste0(last_row$year))',
    "last_val <- last(data@data)[[value_col]]",
    "last_percentile <- round(ecdf(data@data[[value_col]])(last_val) * 100,2)", "",

    "# calculate trend over the last 10 years",
    "max_year <- max(data@data$year)",
    "max_year_minus_ten <- max_year - 10",
    "min_year <- min(data@data$year)",
    "last_ten <- data@data[data@data$year %in% c((max_year_minus_ten):max_year),]",
    "last_ten_mod <- lm(last_ten[[value_col]] ~ c(1:nrow(last_ten)))",
    "last_ten_slope <- last_ten_mod$coefficients[[2]]",
    "last_ten_slope <- ifelse(has_months, last_ten_slope * 12, last_ten_slope)",
    'last_ten_direction <- ifelse(last_ten_slope > 0,"increased","decreased")',
    'last_ten_p <- summary(last_ten_mod)$coefficients[2,"Pr(>|t|)"]',
    'last_ten_p_annotate <-
      if(last_ten_p <= .01) "Very Strong" else
        if(last_ten_p <= .05) "Strong" else
          if(last_ten_p <= .1) "Marginal" else
            "Nonsignificant"',
    'last_ten_p_annotate2 <-
      if(last_ten_p <= .01) "p < 0.01" else
        if(last_ten_p <= .05) "p < 0.05" else
          if(last_ten_p <= .1) "p < 0.1" else
            "p > 0.1"',

    "","","",

    "# calculate trend over full timeseries",
    "full_mod <- lm(data@data[[value_col]] ~ c(1:nrow(data@data)))",
    "full_mod_slope <- full_mod$coefficients[[2]]",
    "full_mod_slope <- ifelse(has_months,full_mod_slope * 12, full_mod_slope)",
    'full_mod_p <- summary(full_mod)$coefficients[2,"Pr(>|t|)"]',
    'full_mod_p_annotate <-
      if(full_mod_p <= .01) "Very Strong" else
        if(full_mod_p <= .05) "Strong" else
          if(full_mod_p <= .1) "Marginal" else
            "Nonsignificant"',
    'full_mod_p_annotate2 <-
      if(full_mod_p <= .01) "p < 0.01" else
        if(full_mod_p <= .05) "p < 0.05" else
          if(full_mod_p <= .1) "p < 0.1" else
            "p > 0.1"',
    'full_mod_direction <- ifelse(full_mod_slope > 0,"increased","decreased")',

    "","","",

    "# calculate highest point in time series",
    "highest_row <- data@data[which.max(data@data[[value_col]]),]",
    'highest_timepoint <- ifelse(has_months,
                                paste0(highest_row$month,"/",highest_row$year),
                                highest_row$year)',
    "highest_val <- highest_row[[value_col]]",

    "","","",

    "# calculate lowest point in time series",
    "lowest_row <- data@data[which.min(data@data[[value_col]]),]",
    'lowest_timepoint <- ifelse(has_months,
                               paste0(lowest_row$month,"/",lowest_row$year),
                               lowest_row$year)',
    "lowest_val <- lowest_row[[value_col]]",


    "","","",

    "# find current values",
    'current_trend <- ifelse(last_ten_slope > 0,"increasing", "decreasing")',
    'current_magnitude <-
      if(last_percentile > 70) "high" else
        if(last_percentile < 30) "low" else
          "moderate"',

    "","","",

    "# find overall mean",
    "overall_mean <-  mean(data@data[[value_col]])",
    "highest_vs_mean <- round(highest_val - overall_mean,2)",
    "lowest_vs_mean <- round(overall_mean - lowest_val,2)",

    "","","",

    "# find whether the current value is above or below the mean, and how long it has been",
    "current_vs_mean <- last_val - overall_mean",
    'current_vs_mean_text <- ifelse(sign(current_vs_mean) == 1,"above","below")',
    'last_different_sign <- switch(current_vs_mean_text,
                                  "above" = last(which(data@data[value_col] < overall_mean)),
                                  "below" = last(which(data@data[value_col] > overall_mean)))',
    'last_date_different_sign <- ifelse(has_months,
                                       paste0(data@data[last_different_sign,"month"],"/",data@data[last_different_sign,"year"]),
                                       paste0(data@data[last_different_sign,"year"]))',

    "",



    "```", # end code chunk

    # analysis text -------------------
    "","","","<!---------- Trends Text ----------->",

    sprintf("As of the most recent data entry in **`r last_date`**, the %s value is `r last_val`, which is **`r current_magnitude`** among values in the timeseries.",toupper(indicator)),
    sprintf("The %s value is currently **`r current_vs_mean_text` the long-term mean**, and has been **since `r last_date_different_sign`**.", toupper(indicator)),
    sprintf("The %s value has followed a(n) **`r current_trend` trend** in recent years.",toupper(indicator)),
    "","","",

    "<!--------- Trends Table--------->",
    "### Summary Table",
    "|Metric      |Value     |Description          |",
    "|:-----------|:---------|:--------------------|",
    "|Most Recent Value (`r last_date`)  | `r last_val` |The most recent value is **`r current_magnitude`** within the timeseries, in the `r last_percentile` percentile of all values. The most recent value is `r abs(round(current_vs_mean,2))` `r current_vs_mean_text` the long-term mean.|",
    sprintf("|Recent Trend (`r max_year_minus_ten` -- `r max_year`)  | `r round(last_ten_slope,3)` per year | In the last ten years, %s has **`r last_ten_direction`** by `r round(last_ten_slope,3)` per year with a **`r last_ten_p_annotate` trend** (`r last_ten_p_annotate2`). |",indicator),
    sprintf("|Overall Trend (`r min_year` -- `r max_year`)  | `r round(full_mod_slope,3)` per year | Since the beginning of the timeseries, %s has **`r full_mod_direction`** by `r round(full_mod_slope,3)` per year with a **`r full_mod_p_annotate` trend** (`r full_mod_p_annotate2`). |",indicator),
    "|Highest Value | `r highest_val` | The highest value in the timeseries was recorded on **`r highest_timepoint`**, and was **`r highest_vs_mean`** higher than the overall timeseries mean. |",
    "|Lowest Value | `r lowest_val` | The lowest value in the timeseries was recorded on **`r lowest_timepoint`**, and was **`r lowest_vs_mean`** lower than the overall timeseries mean. |",

    "","",""


  )

  # make data chunk for regional datasets ------------------------
  trends_chunk_with_regions <- c(
    "<!----------- Trends Section -------------->",
    "## Trends",
    "<!-- ADD SPECIFICATION TO DEFAULTS -->","",

    sprintf("```{r calc-trends-%s, include=F}",indicator),"",

    " # get value colname",
    'value_col <- colnames(data@data)[first(which(stringr::str_detect(colnames(data@data), "value")))]',
    "","","",


    "# calculate mean per region",
    'region_means <- data@data %>%
      group_by(region) %>%
      summarize(mean = mean(get(value_col)),
                sd = sd(get(value_col)),
                .groups = "drop") %>%
      mutate(region = forcats::fct_reorder(region, mean))',
    " # find overall mean",
    "overall_mean <- data@data %>%
      summarize(mean = mean(get(value_col))) %>% pull(mean)","","","",



    "# find trends per region",
    "data_split <-  data@data  %>% split(f =  data@data$region)",
    "data_split_mods <- purrr::map(.x = data_split,
                                  .f = ~lm(.x, formula = get(value_col) ~ year))",
    "# find coefficients per region",
    'data_coefficients <- purrr::map(.x = data_split_mods,
                                    .f = ~coefficients(.x)[[2]] %>% as_tibble) %>%
      bind_rows(.id = "region") %>%
      rename(trend = value)',
    "# find confidence intervals per region",
    'data_confints <- purrr::map(.x = data_split_mods,
                                .f = ~confint(.x)[2,] %>%
                                  setNames(c("lower","upper"))) %>%
      bind_rows(.id = "region")',
    "# find p values per region",
    'data_p_values <- purrr::map(.x = data_split_mods,
                                .f = ~summary(.x)$coefficients[2,"Pr(>|t|)"] %>% as_tibble) %>%
      bind_rows(.id = "region") %>%
      rename(p_val = value)',"","",

    "# merge",
    "merge <- region_means %>%
    merge(data_coefficients) %>%
    merge(data_confints) %>%
    merge(data_p_values) %>%
    arrange(region)","",

    "# plot",
    'slopes_p <- merge %>%
      ggplot( aes(x = region, y = mean,
                  ymin = (mean - sd), ymax = (mean + sd),
                  color = trend)) +
      geom_hline(yintercept = overall_mean, linetype = "dashed") +
      geom_pointrange(linewidth = 1) +
      geom_text(data = . %>% filter(p_val < 0.05),
                label = "*", nudge_y = 2, size = 6, color = "black") +
      scale_color_gradient2(low = "cornflowerblue", mid = "grey80",high = "tomato2") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +',
      sprintf(
      'labs(x = NULL,
           y = "%s",
           color = "Temporal Trend",
           title = "Mean values per region")',indicator_name),"","","",


    "#plot coefficients",
    'slopes_p2 <- merge %>%
     ggplot(aes(x = region, y = trend,
                ymin = lower, ymax = upper,
                color = mean)) +
     geom_hline(yintercept = 0, linetype = "dashed") +
     geom_pointrange(linewidth = 1) +
     geom_text(data = . %>% filter(p_val < 0.05),
               label = "*",
               nudge_y = mean(merge$upper - merge$trend)*2,
               size = 6, color = "black") +
     scale_color_viridis_c() +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +',
    sprintf(
      'labs(x = "Region; arranged by mean temperature",
     y = "%s",
     color = "Mean Value",
     title = "Change over time per region")', indicator_name),
    "","","",


    "# find number of regions increasing and decreasing",
    "n_regions <- length(unique(data@data$region))",
    "n_increasing <- sum(data_coefficients$trend > 0)",
    "n_decreasing <- sum(data_coefficients$trend < 0)",
    "n_significant <- sum(merge$p_val < 0.05)",
    'majority_trend <- ifelse(n_increasing > n_decreasing, "increasing","decreasing")',"",

    "# find highest and lowest regions",
    "highest_region <- as.character(merge$region[which.max(merge$mean)])",
    "highest_mean <- round(merge$mean[merge$region == highest_region],2)",
    "lowest_region <- as.character(merge$region[which.min(merge$mean)])",
    "lowest_mean <- round(merge$mean[merge$region == lowest_region],2)",
    "highest_trend_region <-  as.character(merge$region[which.max(merge$trend)])",
    "lowest_trend_region <-  as.character(merge$region[which.min(merge$trend)])","","","",



    "# find mean quantile of most recent value",
    "last_year <- max(data@data$year)",
    "data_split_last <- purrr::map(
      .x = data_split,
      .f = ~last(.x)
    )",
    'last_percentile <- purrr::map2(
      .x = data_split,
      .y = data_split_last,
      .f = ~round(ecdf(.x[[value_col]])(.y[[value_col]]) * 100,2) %>%
        as_tibble
    ) %>%
      bind_rows(.id = "region") %>%
      rename(percentile = value)',
    "last_percentile_groups <- last_percentile %>%
      summarize(n_high = sum(percentile > 70 ),
                n_mid = sum(percentile > 30 & percentile < 70),
                n_low = sum(percentile < 30))",
    "last_percentile_mean <- last_percentile %>% summarize(mean = mean(percentile))","",

    "```",

    sprintf("%s varies between regions, with the highest value of **`r highest_mean` found in `r highest_region`**, and the lowest value of **`r lowest_mean` found in `r lowest_region`**. ",indicator_name),"","",


    sprintf("```{r plot-trends-%s, echo=F}",indicator),
    "slopes_p2",
    "```","","",


    sprintf("The overall trend is **`r majority_trend`** across regions. `r n_increasing` regions have increasing %s trends and `r n_decreasing` have negative trends. `r n_significant` of these trends are statistically significant over time.",indicator_name),
    "",

    'In the most recent year of sampling (`r last_year`), **`r last_percentile_groups[["n_high"]]` regions had values that are high** compared to their overall timeseries (>70th percentile), **`r last_percentile_groups[["n_mid"]]` regions had values that were moderate** (between 30th and 70th percentile), and **`r last_percentile_groups[["n_low"]]` regions had low values** (<30th percentile) within their timeseries.',
    ""

  )


  # choose trends chunk version based on data structure -----------------
  trends_chunk <- switch(has_region,
                         "no" = trends_chunk_no_regions,
                         "yes" =trends_chunk_with_regions,
                         "one" = trends_chunk_no_regions)




  # relevance text ----------------------------------------------------------
  relevance_chunk <- c(
    "<!----------- Relevance Section -------------->",
    "## Relevance to Research and Stock Assessments",
    "<!-- ADD RELEVANCE-->",
    "","",""
  )



  # variable definitions table -----------------------------------------------
  definitions_chunk <- c(
    "<!----------- Column Definitions Section -------------->",
    "## Variable Definitions",
    "",
    knitr::kable(data.frame(variable = colnames(data@data),description = "ADD",unit = "ADD")),
    "", "",""
  )


  # additional data chunk ---------------------------------------------------
  additional_data_chunk <- c(
    "<!----------- Additional Data Section -------------->",
    "## Additional Data",
    "","","")



  # get data chunk ----------------------------------------------------------

  get_data_chunk <- c(
    "<!----------- Data Access Section -------------->",
    "## Get the Data",
    sprintf("```{r get-data-%s, include=T, echo=T, eval=F}",indicator),
    "library(marea)",
    sprintf("data('%s')",indicator),
    sprintf("plot(%s)",indicator),
    "```"
  )





  # create template rmd -----------------------------------------------------

  # Build Rmd lines
  lines <- c(

    "",
    sprintf("<!-- Indicator Chapter for %s: %s -->",indicator,indicator_name),
    "",

    # note: we don't want rmd headers on chapter pages
    #  # rmd header -----
    #  "---",
    #  sprintf('title: "%s"', indicator_name),
    #  "output: bookdown::html_document2",
    #  "---",


    # code chunk to upload data --------------
    upload_data_chunk,

    # add header section -----------------------
    header_chunk,

    # placeholder for intro --------------------
    intro_chunk,

    # code chunk for plotting ------------------
    plot_chunk,

    # trends chunk -----------------------------
    trends_chunk,

    # relevance chunk -----------------------------
    relevance_chunk,


    # column definitions table -------------------
    definitions_chunk,


    # additionl data section -------------------
    additional_data_chunk,


    # get data chunk ------------------------------
    get_data_chunk



  )

  outpath <- paste0("chapters/",indicator,"_template",".Rmd")
  writeLines(lines, outpath)
  message("Created page template: ", outpath)
  invisible(path)

}





# create marea template ----------------------------------------------------------
create_marea_template <- function(indicator, indicator_long = NULL){

  if(is.null(indicator_long)) indicator_long <- indicator

  # run some checks ---------------------------------------------------------
  # check if page exists
  path <- paste0("chapters/",indicator,"_template",".Rmd")

  # check if indicator exists
  in_pkg <- indicator %in% marea_metadata()$Dataset

  # check if there's already a page for indicator
  if (file.exists(path)) {

    cat("Warning: Chapter template already exists.\n",
        "File: ", path, "\n",
        "Do you want to proceed?\n",
        "The existing chapter file will be lost.")
    ans <- readline(("Type 'y' to overwrite or 'n' to cancel: "))

    # stop if answer isn't y or n
    if(!ans %in% c("y","n")){
      stop("Answer must be 'y' or 'n' ", call. = FALSE)
    }

    # stop if answer is n
    if(ans == c("n")){
      message("Cancelled. Existing chapter kept: ", path)
      return(invisible(FALSE)) # end function
    }

    # if yes, print that the chapter will be overwritten
    message("Overwriting existing chapter: ", path)

  } # end check file exists


  # get indicator data from marea
  indicator_name <- indicator_long


  # template pieces ---------------------------------------------------------
  ## 1. chunk to upload indicator data ---------
  upload_data_chunk <- c(
    "",
    sprintf("```{r setup-%s, include=FALSE}", indicator),
    "library(marea)",
    "library(dplyr)",
    "knitr::opts_chunk$set(echo = F, message = F)",
    "# Upload Indicator Data",
    "",
    "```","","",""

  )

  ## 2. chunk for page header: inputs name and metadata ----------
  header_chunk <- c(
    "<!------- Header Section -------------->","",

    # Title
    "<!-- Title -->",
    sprintf("# %s", indicator_name),"",

    # Metadata
    "<!-- Metadata -->",
    "**Data Type:**","",
    "**Spatial Scope:**","",
    "**Duration:**","",
    "**Source:**","",
    "**Contact:**","","",""
  )


  ## 3. Intro chunk placeholder -----------------------
  intro_chunk <- c(
    "<!----------- Intro Section -------------->",
    "## Introduction to Indicator",
    "<!-- ADD DESCRIPTION -->","","",""
  )


  ## 4. Plot data chunk --------------------------------
  plot_chunk <- c(
    "<!----------- Plots Section -------------->",
    "## View Data",
    "<!-- MAKE INDICATOR PLOT HERE -->",
    sprintf("```{r plot-%s, echo=TRUE}", indicator),
    "","",
    "```","","",""
  )


  ## 5. Calculate trends chunk
  trends_chunk <- c(
    "<!----------- Trends Section -------------->",
    "## Trends",
    "<!-- ADD SPECIFICATION TO DEFAULTS -->","",

    sprintf("```{r calc-trends-%s, include=F}",indicator),

    "","",


    "```", # end code chunk

    # analysis text -------------------
    "","","","<!---------- Trends Text ----------->",

    "","","",

    "<!--------- Trends Table--------->",
    "### Summary Table",
    "|Metric      |Value     |Description          |",
    "|:-----------|:---------|:--------------------|",
    "|            |          |                     |",

    "","",""


  )





  # relevance text ----------------------------------------------------------
  relevance_chunk <- c(
    "<!----------- Relevance Section -------------->",
    "## Relevance to Research and Stock Assessments",
    "<!-- ADD RELEVANCE-->",
    "","",""
  )



  # variable definitions table -----------------------------------------------
  definitions_chunk <- c(
    "<!----------- Column Definitions Section -------------->",
    "## Variable Definitions",
    "",
    "|variable   |description |unit |",
    "|:----------|:-----------|:----|",
    "|           |            |     |",
    "", "",""
  )


  # additional data chunk ---------------------------------------------------
  additional_data_chunk <- c(
    "<!----------- Additional Data Section -------------->",
    "## Additional Data",
    "","","")



  # get data chunk ----------------------------------------------------------

  get_data_chunk <- c(
    "<!----------- Data Access Section -------------->",
    "## Get the Data",
    sprintf("```{r get-data-%s, include=T, echo=T, eval=F}",indicator),
    "","","",
    "```"
  )





  # create template rmd -----------------------------------------------------

  # Build Rmd lines
  lines <- c(

    "",
    sprintf("<!-- Indicator Chapter for %s: %s -->",indicator,indicator_name),
    "",

    # code chunk to upload data --------------
    upload_data_chunk,

    # add header section -----------------------
    header_chunk,

    # placeholder for intro --------------------
    intro_chunk,

    # code chunk for plotting ------------------
    plot_chunk,

    # trends chunk -----------------------------
    trends_chunk,

    # relevance chunk -----------------------------
    relevance_chunk,


    # column definitions table -------------------
    definitions_chunk,


    # additionl data section -------------------
    additional_data_chunk,


    # get data chunk ------------------------------
    get_data_chunk



  )

  outpath <- paste0("chapters/",indicator,"_template",".Rmd")
  writeLines(lines, outpath)
  message("Created page template: ", outpath)
  invisible(path)

}









