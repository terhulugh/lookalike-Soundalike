library(shiny)
library(phonics)
library(keras)
library(stringdist)

# Define server logic
shinyServer(function(input, output, session) {
        # Initialize dataset
        existing_product_names <- reactiveVal(value = NULL)
        
        # Function to save uploaded dataset
        observeEvent(input$product_names, {
                existing_product_names(read.csv(input$product_names$datapath, stringsAsFactors = FALSE))
        })
        
        observeEvent(input$search, {
                req(existing_product_names())
                if (!is.null(existing_product_names())) {
                        # Get the search product name and its phonetic code
                        search_product <- input$search_product
                        search_phonetic <- cologne(search_product, maxCodeLen = 8, clean = FALSE)
                        
                        # Compute phonetic codes for existing product names
                        soundalike <- transform(existing_product_names(), 
                                                sound = cologne(existing_product_names()[,1], maxCodeLen = 8, clean = FALSE))
                        
                        # Find phonetic matches
                        phonetic_matches <- which(soundalike$sound == search_phonetic)
                        
                        # Use Jaro-Winkler distance for additional matching
                        jw_distances <- stringdist(existing_product_names()[,1], search_product, method = "jw")
                        jw_threshold <- 0.3  # Adjust this threshold as needed
                        jw_matches <- which(jw_distances <= jw_threshold)
                        
                        # Combine matches
                        combined_matches <- unique(c(phonetic_matches, jw_matches))
                        
                        if (length(combined_matches) > 0) {
                                output$soundalike_output <- renderPrint({
                                        sound_alike_products <- existing_product_names()[,1][combined_matches]
                                        sound_alike_products
                                })
                        } else {
                                output$soundalike_output <- renderPrint({
                                        "No sound-alike product names found."
                                })
                        }
                } else {
                        output$soundalike_output <- renderPrint(NULL)
                }
        })
        
        observeEvent(input$reset, {
                updateTextInput(session, "search_product", value = "")
                output$soundalike_output <- renderPrint(NULL)
        })
        
        
        # Function to read and resize images to a consistent dimension
        read_and_resize_image <- function(file_path, target_width, target_height) {
                ext <- tools::file_ext(file_path)
                if (ext == "png") {
                        image <- readPNG(file_path)
                } else if (ext %in% c("jpg", "jpeg")) {
                        image <- readJPEG(file_path)
                } else if (ext %in% c("tif", "tiff")) {
                        image <- readTIFF(file_path)
                } else {
                        stop("Unsupported image format")
                }
                
                image_resized <- resize(Image(image), w = target_width, h = target_height)
                image_gray <- channel(image_resized, "gray")
                return(image_gray)
        }
        
        # Ensure the image has three channels
        ensure_three_channels <- function(image) {
                if (dim(image)[3] == 4) {
                        image <- drop(image[,,1:3])  # Drop the alpha channel
                }
                return(image)
        }
        
        
        target_width <- 100  # Set the target width for resizing
        target_height <- 100 # Set the target height for resizing
        
        # Function to read and process ZIP file containing image data
        read_and_process_zip <- function(zip_file_path) {
                unzip(zip_file_path, exdir = "temp_images")
                image_files <- list.files("temp_images", full.names = TRUE)
                processed_data <- lapply(image_files, function(image_file) {
                        ext <- tools::file_ext(image_file)
                        if (ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
                                image_gray <- read_and_resize_image(image_file, target_width, target_height)
                                image_gray <- ensure_three_channels(image_gray)
                                return(list(name = basename(image_file), image = image_gray))
                        } else {
                                return(NULL)
                        }
                })
                unlink("temp_images", recursive = TRUE)
                processed_data <- processed_data[!sapply(processed_data, is.null)]
                return(processed_data)
        }
        
        # Function to compare images
        compare_images <- function(new_image, existing_images) {
                distances <- sapply(existing_images, function(existing_image) {
                        sum((new_image - existing_image$image)^2)
                })
                return(distances)
        }
        
        # Reactive value to store existing labels
        existing_labels <- reactiveVal(NULL)
        
        # Load and process existing labels when a ZIP file is uploaded
        observeEvent(input$existing_labels, {
                existing_labels(read_and_process_zip(input$existing_labels$datapath))
        })
        
        # Search for lookalike labels when the search button is clicked
        observeEvent(input$search_button, {
                req(existing_labels())
                new_label_file <- input$new_label
                if (is.null(new_label_file)) return()
                
                ext <- tools::file_ext(new_label_file$name)
                if (ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
                        new_label <- read_and_resize_image(new_label_file$datapath, target_width, target_height)
                        new_label <- ensure_three_channels(new_label)
                        
                        distances <- compare_images(new_label, existing_labels())
                        closest_label_index <- which.min(distances)
                        closest_label <- existing_labels()[[closest_label_index]]$name
                        output$lookalike_output <- renderPrint({
                                if (distances[closest_label_index] < 5000) {  # You can adjust the threshold
                                        paste("Closest lookalike label:", closest_label)
                                } else {
                                        "No lookalike labels found."
                                }
                        })
                }
        })
        
        # Reset the input and output when the reset button is clicked
        observeEvent(input$reset_button, {
                session$sendCustomMessage("resetFileInput", "existing_labels")
                session$sendCustomMessage("resetFileInput", "new_label")
                output$lookalike_output <- renderPrint(NULL)
        })
})