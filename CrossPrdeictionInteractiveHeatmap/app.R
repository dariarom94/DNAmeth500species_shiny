#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(InteractiveComplexHeatmap)
library(ComplexHeatmap)
library(data.table)
library(tidyverse)
library(circlize)
library(shiny)
library(BiocGenerics)
#adding info about the repo
#options(repos = BiocManager::repositories())

all_aucs <- fread("all_aucs_full.csv")
sp_list <- fread("species_list_ordered_2021_2.txt", header = F)$V1
stats_annot <- fread("all_stats.tsv")

all_auc_m <- all_aucs %>% 
    filter(ifRand=="noRandTest"|ifRand=="noRand" ) %>%
    dplyr::select(c("train_species", "type", "auc")) %>% 
    spread(type, auc)



##fixing the english names for the future:
english_fix_typos = c("Blaubauchracke" = "Blue-bellied roller", 
                      "Common Octopus" = "Common octopus", 
                      "Ostrich"  = "Common ostrich",
                      "Wolf" = "Polarwolf", 
                      "Spot-Fin porcupinefish" = "Spot-fin porcupinefish",
                      "stone marten" = "Stone marten", 
                      "Monterey stalked tunicate" = "Stalked tunicate",
                      "Yellowfin snook" = "YellowFin snook", 
                      "Kangaroo " = "Kangaroo",
                      "Sulmtaler" = "Chicken",
                      "Yellow-billed amazon" = "White-fronted amazon"
)
for (i in names(english_fix_typos)){
    stats_annot[stats_annot$English == i,]$English <- english_fix_typos[i]
}
proper_names <- unique(stats_annot[, c("species", "English")])
proper_names[species=="KAN", English:= "Kangaroo",]
proper_names <- unique(proper_names)
proper_names[, full_name:= paste0(English, " (", species, ")"),by = row.names(proper_names)]

sp_list <- sp_list[sp_list %in% all_auc_m$train_species]
sp_list_full <- as.character(sapply(sp_list, function(x) as.character(proper_names[species==x]$full_name)))

##transferring in the matrix
all_auc_m <- as.data.frame(all_auc_m)
full_names <- sapply(all_auc_m$train_species, function(x) as.character(proper_names[species==x]$full_name))
row.names(all_auc_m) <- as.character(full_names)

all_auc_m <- as.matrix(all_auc_m[, -c(1)])
full_names <- sapply(colnames(all_auc_m), function(x) as.character(proper_names[species==x]$full_name))
colnames(all_auc_m) <- as.character(full_names)



col_annot <- as.data.frame(unique(all_aucs[, c("type", "color_class_test")]))

row.names(col_annot) <- col_annot$type
head(col_annot)

col_annot <- data.frame(col_annot[sp_list, "color_class_test"])
colnames(col_annot) <- c("class")

col_fun = colorRamp2(c(0, 0.4,0.5,0.6, 1), c("#2b83ba","#abd9e9", "#ffffbf", "#fdae61","#d7191c"))
class_colors <- c("Invertebrata" = "#cfcfcf",
                  "Jawless_vertebrate" = "#636363",
                  "Chondrichthyes" = "#F68383", 
                  "Actinopteri" = "#EA4142",
                  "Amphibia" = "#5AB349",
                  "Reptilia" = "#8761AC",
                  "Aves"="#FE9222",
                  "Marsupialia"="#8EBFDA",
                  "Mammalia"="#4892C2")


ha = columnAnnotation(df = col_annot, col = list("class" = class_colors), 
                      show_legend=c(F), 
                      height = unit(0.1, "cm"))
ra = rowAnnotation(df = col_annot, col = list("class" = class_colors),
                   show_legend=c(T), width = unit(0.1, "cm"))

ht = Heatmap(all_auc_m[sp_list_full, sp_list_full], cluster_columns = F, cluster_rows = F,
            name="ROC-AUC", col = col_fun,
            top_annotation = ha,
            left_annotation = ra,
            show_row_names = FALSE,
            show_column_names = FALSE,
            column_names_gp = grid::gpar(fontsize = 10),
            row_names_gp = grid::gpar(fontsize = 10))

suppressPackageStartupMessages(library(kableExtra))
brush_action = function(df, output) {
  print("activating brush action")
  row_index = sp_list_full[unique(unlist(df$row_index))]
  column_index = sp_list_full[unique(unlist(df$column_index))]
  print(column_index)
  output[["info"]] = renderUI({
    if(!is.null(df)) {
      HTML(kable_styling(kbl(all_auc_m[row_index, column_index, drop = FALSE], digits = 2, format = "html"), 
                         full_width = FALSE, position = "left"))
    }
  })
}


ui = fluidPage(
    h3("Interactive visualization of across-species predictions"),
    p("This is an interactive heatmap visualization of AUC results of models, trained and tested in all species pairs."),
    p("If labels are unreadable or the sub-heatmap is not loading, select a smaller area and adjust the size of the sub-heatmap."),
    p("To search row/column names by pattern, tick the 'Regular expression' and type in a pattern, i.e. *monkey"),
    InteractiveComplexHeatmapOutput(width1 = 600, height1 = 500, width2 = 600, height2 = 500, output_ui = htmlOutput("info"))
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, 
                                  brush_action = brush_action)#,
#    sub_heatmap_cell_fun = function(j, i, x, y, width, height, fill) {
  #grid.text(sprintf("%.3f", all_auc_m[i, j]), x, y, gp = gpar(fontsize = 5))} )
}

shiny::shinyApp(ui, server)