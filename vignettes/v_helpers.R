library(jsonlite)




getAttrs <- function(file, base = getwd()) {
  base = getwd()
  path <- file.path(getwd(), file) #"vignettes",
  attrs <- jsonlite::read_json(path)$Entries

  dfattrs <- do.call(rbind, lapply(attrs, as.data.frame))
  rownames(dfattrs) <- NULL  # Remove row names

  return(dfattrs)
}




getSkills <- function(file, base = getwd()) {
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(tidyr, quietly = TRUE, warn.conflicts = FALSE)

  path <- file.path(base, file) #"vignettes",
  skills <- jsonlite::read_json(path)$Entries

  # Get all unique column names
  allCols <- lapply(skills, names) |> unlist() |> unique()
  skillsAligned <- lapply(skills, \(df) {
    missing <- setdiff(allCols, names(df))
    if (length(missing) > 0) {
      df[missing] <- NA
    }
    # Reorder columns to match
    df <- df[allCols]
    return(df)
  })
  # Combine into one data frame
  dfskills <- do.call(rbind, skillsAligned)
  rownames(dfskills) <- NULL  # Remove row names

  #
  # RESHAPE
  dfskillsLong <- as.data.frame(dfskills) %>%
    pivot_longer(
      cols = c(ab1, ab2, ab3),
      names_to = "abVar",       # New column for the original variable name
      values_to = "attrLevel"   # New column for the ATTR_X values
    )

  # Calculate the frequency (1, 2, or 3) of each attribute per original row
  dfskillsPlot <- dfskillsLong %>%
    group_by(name, attrLevel) %>%
    mutate(freq = n()) %>%
    ungroup() %>%
    # Convert frequency to a factor to ensure correct stacking order and legend
    mutate(freq = factor(freq, levels = c(3, 2, 1)))
  dfskillsPlot$attrLevel <- unlist(dfskillsPlot$attrLevel) |>
    factor(levels = paste0("ATTR_", 1:8), labels = dfattrs$shortname)
  dfskillsPlot$name <- unlist(dfskillsPlot$name) |> factor()


  return(dfskillsPlot)
}



plotSkillFrequencies <- function(file, base = getwd()) {

  dfattrs <- getAttrs("attributes_en.json", base)
  skills <- getSkills(file, base)

  #
  # PLOT
  ggplot(skills, aes(x = attrLevel, fill = freq)) +
    geom_bar() +
    labs(
      x = "Attribute",
      y = "Number of Occurrences",
      fill = "Times per Skill",
      title = "Distribution of Attributes by Occurrence"
    ) +
    scale_x_discrete(drop = FALSE) + # Ensure all ATTR_# show up even if count is 0
    theme_minimal()
}
