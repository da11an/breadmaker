# bakers_formula is a table with the following columns
# - ingredient
# - wt fraction in completed dough
# - stage added

library(dplyr)

ingredients <- tibble(
  ingredient = c(
    "wheat flour","white flour","water","milk","salt","yeast",
    "baking powder","leaven","poolish","starter","cooking oil","sugar",
    "butter","eggs","honey"),
  vol_unit = c(
    "cups", "cups", "cups", "cups", "tbsp", "tbsp",
    "tsp", "cups", "cups", "cups", "tbsp", "cups",
    "tbsp", "eggs", "tbsp"),
  grams = c(
    140, 140, 210, 210*1.03, 21, 10,
    3, 200, 200, 200, 13, 110,
    14, 60, 21)
)

recipe_inputs <- tibble(
  ingredient = c("wheat flour", "water", "salt", "yeast", "sugar", "cooking oil"),
  info = c("(rem. = white)", "", "", "", "", ""),
  g_min = c(0, 35, 0, 0, 0, 0),
  g_max = c(100, 105, 3, 2, 15, 2),
  g_step = c(5, 1, 0.1, 0.01, 1, 0.05),
  g_value = c(50, 68, 1.5, 0.5, 6, 0.3)
) %>%
  left_join(ingredients %>% select(ingredient, vol_unit))

conv_vol <- function(value, unit_in, unit_out = "cups") {
  per_cup <- c("cups" = 1, "tbsp" = 16, "tsp" = 48, "eggs" = 4,
               "gal" = 1/16, "pint" = 1/2, "quart" = 1/4)
  out <- value / per_cup[unit_in]
  if (unit_out != "cups") out <- out * per_cup[unit_out]
  return(unname(out))
}

conv_ingr_vol2g <- function(value, unit, ingredient) {
  tbl_val <- ingredients %>%
    filter(ingredient == !!ingredient) %>%
    pull(grams, vol_unit)
  wt <- unname(conv_vol(value, unit, names(tbl_val)) * tbl_val)
  attr(wt, "unit") <- "g"
  return(wt)
}

conv_ingr_g2vol <- function(value, ingredient) {
  tbl_val <- ingredients %>%
    filter(ingredient == !!ingredient) %>%
    pull(grams, vol_unit)
  vol <- unname(value / tbl_val)
  attr(vol, "unit") <- "tbsp"
  return(vol)
}

bakers_formula <- function(ingredients, wt_frac, stage = "dough") {
  tbl <- data.frame(
    ingredient = ingredients,
    wt_frac = wt_frac,
    stage = stage
  )
  tbl <- tbl %>%
    mutate(wt_frac = wt_frac / (tbl %>%
                                  filter(ingredient %in% c("wheat flour", "white flour")) %>%
                                  pull(wt_frac) %>%
                                  sum()))
  class(tbl) <- c('bakers_formula', 'tbl_df', 'tbl', 'data.frame')
  return(tbl)
}

nutrition <- function(my_formula, nutrition_tbl, basis) {
  nutrition_tbl %>%
    left_join(my_formula) %>%
    mutate(value = value * wt_frac * !!basis) %>%
    select(value, metric, unit) %>%
    group_by(metric, unit) %>%
    summarize(value = sum(value)) %>%
    ungroup()
}