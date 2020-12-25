#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinyWidgets)

source("R/formulas.R")
source("data/nutrition.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Breadmaker"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                id = "sidepanels",
                type = "pills",
                
                tabPanel(
                    "Formula",
                    icon = icon("balance-scale"),
                    
                    numericInput(
                        "bakers_basis", "Total flour in grams (batch size)",
                        min = 1, max = 50000, step = 50, value = 1000),
                    
                    uiOutput('wt_sliders'),
                    hr(),
                    actionButton('vol2formula', "Enter Volumes", icon = icon("flask")),
                ),
                
                tabPanel(
                    "Finishing",
                    icon = icon("fire"),
                    
                    h3("Wt loss: (in - out)/in * 100"),
                    sliderInput(
                        'total_loss', "% ingredient wt lost in final loaf",
                        min = 5, max = 25, value = 13, step = 1),
                    
                    h3("Serving size"),
                    sliderInput(
                        'slice_g', "Slice Weight (g)",
                        min = 25, max = 60, value = 40
                    ),
                    sliderInput(
                        'num_slices', "Slices",
                        min = 1, max = 4, value = 1
                    )
                )
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            panel(
                heading = "Recipe",
                tableOutput('recipe')
            ),
            panel(
                heading = "Estimated Nutrition Facts",
                status = "info",
                uiOutput('serving_size_ui'),
                tableOutput('nutrition_facts')
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    rv <- reactiveValues(formula = bakers_formula(
        ingredients = c("wheat flour", "white flour", "water", "salt", "yeast", "sugar"),
        wt_frac = c(50, 50, 70, 1.5, 0.5, 6)
    ))
    
    output$wt_sliders <- renderUI({
        lapply(seq(nrow(recipe_inputs)), function(i) {
            sliderInput(gsub(" ", "_", recipe_inputs$ingredient[i]),
                        paste("%", recipe_inputs$ingredient[i], recipe_inputs$info[i]),
                        min = recipe_inputs$g_min[i],
                        max = recipe_inputs$g_max[i],
                        step = recipe_inputs$g_step[i],
                        value = recipe_inputs$g_value[i])
        })
    })
    
    observe({
        req(input$wheat_flour)
        rv$formula <- bakers_formula(
            ingredients = c("wheat flour", "white flour", "water", "salt", "yeast", "cooking oil", "sugar"),
            wt_frac = c(input$wheat_flour, 100 - input$wheat_flour, input$water, input$salt, input$yeast, input$cooking_oil, input$sugar)
        )
        # dput(rv$formula)
    })
    
    output$recipe <- renderTable({
        rv$formula %>% select(ingredient, wt_frac) %>%
            mutate(
                `Wt (grams)` = wt_frac * !!input$bakers_basis,
                wt_frac = wt_frac * 100,
                volume = conv_ingr_g2vol(`Wt (grams)`, ingredient)
            ) %>%
            rename(`Bakers %` = wt_frac) %>%
            left_join(ingredients %>% select(ingredient, vol_unit))
    })
    
    output$nutrition_facts <- renderTable({
        # numerator <- rv$formula %>% filter(ingredient == "salt") %>% pull(wt_frac)
        # denom <- sum(rv$formula$wt_frac)*(1 - input$total_loss/100)
        # mg_na <- round((numerator / denom) * input$slice_g * input$num_slices * 1000 * 7 /18)
        # paste(as.character(mg_na), "mg sodium per serving size, or ", round(mg_na/2300 * 100), "% daily value") %>% print()
        # 
        bakers_to_baked_percent <- 1/(sum(rv$formula$wt_frac)*(1 - input$total_loss/100))
        
        my_nutrition <- nutrition(rv$formula, nutrition_tbl,
                                  input$num_slices * input$slice_g * bakers_to_baked_percent) %>%
            mutate(
                unit  = if_else(metric %in% c("Sodium", "Potassium"), "mg", unit),
                value = if_else(metric %in% c("Sodium", "Potassium"),  value * 1000,  value),
                value = signif(value, 3)
            ) %>%
            tidyr::unite(Value, c(value, unit), sep = " ")
    })
    
    output$serving_size_ui <- renderUI({
        strong("Serving size", input$num_slices, paste0("slices (", input$num_slices * input$slice_g, "g)"))
    })
    
    observeEvent(input$vol2formula, {
        vol_ingredients <- c(
            "wheat flour",
            "white flour",
            "water",
            "salt",
            "yeast",
            "sugar",
            "cooking oil"
        )
        showModal(modalDialog(
            title = "Volumetric Recipe Entry",
            lapply(seq_along(vol_ingredients), function(i) {
                numericInput(paste0("vol_", gsub(" ", "_", vol_ingredients[i])),
                             paste0(vol_ingredients[i], " (",
                                    ingredients %>% filter(ingredient == !!vol_ingredients[i]) %>% pull(vol_unit),
                                    ")"),
                             value = 0)
            }),
            footer = tagList(
                modalButton("Dismiss"),
                actionButton('apply_vol_recipe', "Apply", icon = icon("sliders"))
            )
        ))
    })
    
    
    vol_inputs <- reactive({
        myvalues <- NULL
        for(i in 1:length(names(input))){
            myvalues <- as.data.frame(rbind(myvalues,(cbind(names(input)[i],input[[names(input)[i]]]))))
        }
        names(myvalues) <- c("ingredient", "volume")
        myvalues %>% filter(stringr::str_detect(ingredient, "^vol_")) %>% 
            mutate(ingredient = gsub("_", " ", sub("vol_", "", ingredient)))
    })
    
    observeEvent(input$apply_vol_recipe, {
        new_recipe <- vol_inputs() %>%
            left_join(ingredients) %>%
            mutate(wt_frac = conv_ingr_vol2g(as.numeric(volume), vol_unit, ingredient))
        
        basis <- new_recipe %>% filter(stringr::str_detect(ingredient, "flour$")) %$% sum(wt_frac)
        new_recipe <- new_recipe %>% mutate(wt_frac = wt_frac/basis)
        
        for (i in new_recipe$ingredient) {
            updateSliderInput(
                session,
                gsub(" ", "_", i),
                value = new_recipe %>%
                    filter(ingredient == !!i) %>%
                    pull(wt_frac) %>%
                    as.numeric() %>%
                    `*`(100))
        }
        updateNumericInput(session, "bakers_basis", value = basis)
    })
    output$show_inputs <- renderTable({
        vol_inputs()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
