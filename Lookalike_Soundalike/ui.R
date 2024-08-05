# Load necessary libraries
library(shiny)
library(shinythemes)
library(png)
library(jpeg)
library(tiff)
library(EBImage)
library(dplyr)
library(keras)
library(stringdist)

options(shiny.maxRequestSize = 10000 * 1024^2)

shinyUI(
        navbarPage("Product Copyright Identifier app", theme = shinytheme("cerulean"),
                   tabPanel("Product Name Search",
                            fluidPage(
                                    titlePanel("Sound-Alike Identifier"),
                                    sidebarLayout(
                                            sidebarPanel(
                                                    fileInput("product_names", "Upload Product Names Dataset (CSV format):"),
                                                    textInput("search_product", "Input New Product Name:"),
                                                    actionButton("search", "Search", class = "btn-success"),
                                                    actionButton("reset", "Reset", class = "btn-danger")
                                            ),
                                            mainPanel(
                                                    h4("Soundalike Product Names Search Result:"),
                                                    verbatimTextOutput("soundalike_output"),
                                                    
                                            )
                                    )
                            )
                   ),
                   tabPanel("Product Label Search",
                            fluidPage(
                                    titlePanel("Look-alike Identifier"),
                                    sidebarLayout(
                                            sidebarPanel(
                                                    fileInput("existing_labels", "Upload Existing Labels (ZIP file):", accept = ".zip"),
                                                    fileInput("new_label", "Upload New Label (Image file):", multiple = FALSE, 
                                                              accept = c(".png", ".jpg", ".jpeg", ".tiff")),
                                                    actionButton("search_button", "Search",  class = "btn-success"),
                                                    actionButton("reset_button", "Reset", class = "btn-danger")
                                            ),
                                            mainPanel(
                                                    h4("Lookalike Labels:"),
                                                    verbatimTextOutput("lookalike_output")
                                            )
                                    )
                            )
                   )
        )
)

