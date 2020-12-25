# https://www.livestrong.com/article/281394-nutrition-information-on-wheat-berries/
# 163.5 calories, 1 gram of fat, 6.5 gram of protein, 35.5 gram of carbohydrates, 6 gram of dietary fiber and 1 milligram of sodium
wheat_flour <- tibble(
  ingredient = "wheat flour",
  basis = 50,
  basis_unit = "g",
  metric = c("Calories", "Fat", "Protein", "Carbohydrates", "Fiber", "Sodium"),
  value = c(163.5, 1, 6.5, 35.5, 6, 0.001),
  unit = c("Cal", "g", "g", "g", "g", "g")
)

# https://www.walmart.com/ip/Great-Value-All-Purpose-Flour-10-lb/122814298
white_flour <- tibble(
  ingredient = "white flour",
  basis = 30,
  basis_unit = "g",
  metric = c("Calories", "Fat", "Protein", "Carbohydrates", "Fiber", "Sodium"),
  value = c(110, 0, 3, 23, 0.85, 0),
  unit = c("Cal", "g", "g", "g", "g", "g")
)

# USDA (google) salt
salt <- tibble(
  ingredient = "salt",
  basis = 18,
  basis_unit = "g",
  metric = c("Sodium", "Potassium"),
  value = c(6976e-3, 1e-3),
  unit = c("g", "g")
)

# bakers yeast: https://nutritiondata.self.com/facts/baked-products/5130/2
yeast <- tibble(
  ingredient = "yeast",
  basis = 12, #(1 tbsp)
  basis_unit = "g",
  metric = c("Calories", "Carbohydrates", "Fiber", "Protein", "Fat", "Sodium", "Potassium"),
  value = c(35.4, 4.6, 2.5, 4.6, 0.6, 6e-3, 240e-3),
  unit = c("Cal", "g", "g", "g", "g", "g", "g")
)

# sugar
sugar <- tibble(
  ingredient = "sugar",
  basis = 4.2, # 1 tsp
  basis_unit = "g",
  metric = c("Calories", "Carbohydrates", "Sugars"),
  value = c(16, 4.2, 4.2),
  unit = c("Cal", "g", "g")
)

# oil
cooking_oil <- tibble(
  ingredient = "cooking oil",
  basis = 13.6, # 1 tbsp
  basis_unit = "g",
  metric = c("Calories", "Fat"),
  value = c(120, 13.6),
  unit = c("Cal", "g")  
)

nutrition_tbl <- bind_rows(
  white_flour,
  wheat_flour,
  salt,
  yeast,
  sugar,
  cooking_oil
) %>%
  mutate(value = value / basis, basis = basis / basis)
