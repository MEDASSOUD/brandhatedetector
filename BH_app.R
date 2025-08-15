library(shiny)
library(shinydashboard)
library(rvest)
library(stringr)
library(dplyr)
library(DT)
library(sentimentr)
library(tidytext)
library(stopwords)
library(ggplot2)
library(tidyr)
library(purrr)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(base64enc)
library(textdata)

# --- Helper Functions for Scraping ---

# Function for Step 1: Scrape Brand Information (rvest)
scrape_brand_info <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # 1. Brand Name
    brand_name_raw <- page %>%
      html_element("h1.prf-hdr__cpy-nm") %>%
      html_text(trim = TRUE)
    brand_name <- str_replace(brand_name_raw, " Reviews", "")
    
    # 2. Total Number of Reviews
    total_reviews_raw <- page %>%
      html_element("a.rvws-strs-rtng-v2__rvws") %>%
      html_text(trim = TRUE)
    
    total_reviews <- total_reviews_raw %>%
      str_replace_all("[^0-9]", "") %>%
      as.numeric()
    
    # 3. Brand Logo URL
    logo_url_raw <- page %>%
      html_element("div.prf-lg.prf-hdr__logo img") %>%
      html_attr("src")
    
    base_url <- "https://www.consumeraffairs.com"
    if (!str_starts(logo_url_raw, "http")) {
      logo_url <- paste0(base_url, logo_url_raw)
    } else {
      logo_url <- logo_url_raw
    }
    
    logo_display_html <- ""
    if (!is.null(logo_url) && nchar(logo_url) > 0) {
      logo_display_html <- paste0('<img src="', logo_url, '" style="max-width: 150px; height: auto;">')
    }
    
    return(list(
      brand_name = brand_name,
      total_reviews = total_reviews,
      logo_url = logo_url,
      logo_display_html = logo_display_html
    ))
    
  }, error = function(e) {
    message("Error in scrape_brand_info: ", e$message)
    return(NULL)
  })
}


# Function for Step 2: Scrape Reviews (rvest only)
scrape_reviews <- function(url, session_status_updater) {
  all_data <- list()
  base_url <- gsub("\\?.*|#.*", "", url) # Clean base URL from params/fragments
  max_pages_to_scrape <- 15 # Keep max pages for now, adjust if throttling persists
  
  for (i in 1:max_pages_to_scrape) {
    page_url <- paste0(base_url, "?page=", i, "#sort=recent")
    session_status_updater(paste0("Scraping page ", i, " (URL: ", page_url, ")..."))
    
    page <- tryCatch(read_html(page_url), error = function(e) {
      message(paste("Error reading page", i, ":", e$message))
      session_status_updater(paste0("Error reading page ", i, ": ", e$message, " Stopping scraping."))
      return(NULL) # Return NULL on error to break the loop
    })
    
    if (!is.null(page)) {
      reviews_on_page <- page %>% html_elements(".rvw__top-text") %>% html_text(trim = TRUE)
      reviewers_on_page <- page %>% html_elements(".rvw__inf-nm") %>% html_text(trim = TRUE)
      tags_on_page <- page %>% html_elements(".rvw__tag-cntr") %>%
        lapply(function(x) {
          x %>% html_elements(".rvw__tag") %>% html_text(trim = TRUE) %>% paste(collapse = ", ")
        }) %>%
        unlist()
      
      # If no review elements are found, it might indicate the end of available pages
      if (length(reviews_on_page) == 0 && length(reviewers_on_page) == 0 && length(tags_on_page) == 0) {
        session_status_updater(paste0("No new content found on page ", i, ". Stopping scraping."))
        break
      }
      
      # Handle cases where some elements might be missing on a page (e.g., only reviews, no reviewers)
      max_len_current_page <- max(length(reviews_on_page), length(reviewers_on_page), length(tags_on_page))
      if (max_len_current_page == 0) {
        df_current_page <- tibble(reviewer = character(), review = character(), reasons = character())
      } else {
        df_current_page <- tibble(
          reviewer = c(reviewers_on_page, rep(NA_character_, max_len_current_page - length(reviewers_on_page))),
          review = c(reviews_on_page, rep(NA_character_, max_len_current_page - length(reviews_on_page))),
          reasons = c(tags_on_page, rep(NA_character_, max_len_current_page - length(tags_on_page)))
        )
      }
      all_data[[i]] <- df_current_page
    } else {
      # If read_html failed, the loop is already broken by the tryCatch returning NULL
      break
    }
    Sys.sleep(2) # Increased sleep duration to 2 seconds
  }
  
  if (length(all_data) == 0) {
    return(tibble(reviewer = character(), review = character(), reasons = character(), review_id = integer()))
  } else {
    # Combine all scraped data, filter out empty reviews, remove duplicates, and then add review_id
    combined_data <- bind_rows(all_data) %>%
      filter(!is.na(review) & review != "") %>%
      distinct(review, reviewer, .keep_all = TRUE) %>% # Remove duplicates based on review and reviewer
      mutate(review_id = row_number()) # Ensure review_id is created here for use in DT
    return(combined_data)
  }
}

# --- Shiny UI with Dashboard (Including Welcome Step) ---
ui <- dashboardPage(
  skin = "black", # Set the dashboard skin to "black"
  dashboardHeader(
    # Use tags$a for a clickable logo that goes to the dashboard start,
    # and tags$img for the actual image.
    title = tags$a(href = "javascript:void(0);", # Prevents page reload, just changes tab
                   onclick = "Shiny.setInputValue('tabs', 'splash', {priority: 'event'});", # Navigate to splash on click
                   tags$img(src = "https://mohamedassoud.com/wp-content/uploads/2025/07/Asset-4.svg",
                            height = "50px", # Adjust height as needed
                            style = "margin-top: -10px; margin-left: -10px;")) # Adjust position if necessary
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      selected = "splash", # NEW: Default to the splash screen
      menuItem("Splash Screen", tabName = "splash", icon = icon("picture-o")), # NEW: Splash screen menu item
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Step 1: Brand Information", tabName = "step1_brand_info", icon = icon("info-circle")),
      menuItem("Step 2: Scrape Reviews", tabName = "step2_scrape_reviews", icon = icon("comments")),
      menuItem("Reviews Table", tabName = "review_table_view", icon = icon("table")),
      menuItem("Step 3: Negative Reviews", tabName = "step3_negative_reviews", icon = icon("angry")),
      menuItem("Step 4: Stopword-Free Neg. Reviews", tabName = "step4_stopwords", icon = icon("eraser")),
      menuItem("Step 5: NRC Emotion Analysis", tabName = "step5_nrc_emotions", icon = icon("heart-broken")),
      menuItem("Step 6: Emotion Visualizations", tabName = "step6_plots", icon = icon("chart-bar")),
      menuItem("Step 7: Hate Intensification", tabName = "step7_intensification", icon = icon("fire")),
      menuItem("Step 8: Final Classification", tabName = "step8_final_classification", icon = icon("star")),
      menuItem("Step 9: Summary Plots", tabName = "step9_summary_plots", icon = icon("chart-pie")),
      menuItem("Step 10: Reasons Analysis", tabName = "step10_reasons_plots", icon = icon("lightbulb")),
      menuItem("Step 11: Emotion Word Clouds", tabName = "step11_word_clouds", icon = icon("cloud")),
      menuItem("Step 12: Hate Intensity Bubble Plot", tabName = "step12_bubble_plot", icon = icon("circle")),
      menuItem(actionButton("quit_app", "Quit App", icon = icon("power-off")), icon = NULL)
    )
  ),
  dashboardBody(
    tags$head( # Custom CSS for a more modern look
      tags$style(HTML("
        /* Overall page background */
        .content-wrapper, .right-side {
          background-color: #f2f3f7; /* Light gray background */
        }
        /* Dashboard header */
        .main-header .navbar {
          background-color: #eace22; /* ACCENT COLOR */
        }
        .main-header .logo {
          background-color: #d6b81d; /* Slightly darker for logo */
        }
        /* Box styling */
        .box {
          border-radius: 8px; /* Rounded corners for boxes */
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); /* Subtle shadow */
          border-top: 3px solid #eace22; /* ACCENT COLOR */
        }
        .box-header.with-border {
          border-bottom: 1px solid #eee;
        }
        .box-title {
          font-weight: 600;
          color: #333;
        }
        /* Input fields */
        .form-control {
          border-radius: 5px;
          border: 1px solid #ccc;
        }
        /* Buttons */
        .btn {
          border-radius: 5px;
          padding: 8px 15px;
          font-weight: 500;
        }
        .btn-primary {
          background-color: #eace22; /* ACCENT COLOR */
          border-color: #eace22; /* ACCENT COLOR */
          color: black; /* Text color for accent buttons */
        }
        .btn-primary:hover {
          background-color: #d6b81d; /* Slightly darker accent */
          border-color: #d6b81d; /* Slightly darker accent */
        }
        .btn-default {
          background-color: #f8f9fa;
          border-color: #ccc;
          color: #333;
        }
        .btn-default:hover {
          background-color: #e2e6ea;
        }
        /* DataTables */
        .dataTables_wrapper .dataTables_paginate .paginate_button.current, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background-color: #eace22 !important; /* ACCENT COLOR */
          color: black !important; /* Text color for accent pagination */
          border-color: #eace22 !important; /* ACCENT COLOR */
        }
        /* Sidebar menu active item (for black skin, this section needs adjustment to ensure visibility) */
        /* Note: For black skin, shinydashboard might override some of these.
                   If you see issues, you might need more specific CSS. */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #eace22; /* ACCENT COLOR */
          border-left-color: #d6b81d; /* Darker accent line */
          color: black; /* Text color for active sidebar item */
        }
        .skin-black .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #eace22; /* Accent on hover */
          color: black; /* Text color on hover */
        }
        /* Override default black skin sidebar background to ensure custom colors work */
        .skin-black .main-sidebar {
            background-color: #222d32; /* Keep original black skin sidebar background */
        }
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
            background-color: #222d32;
        }
        /* Custom styles for the splash screen */
        .splash-container {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          height: 100vh; /* Full viewport height */
          text-align: center;
          background-color: #222d32; /* Match sidebar background for seamless transition */
          color: white; /* White text for black background */
        }
        .splash-logo {
          max-width: 50%; /* Max width for the logo */
          height: auto;
          margin-bottom: 30px; /* Space between logo and button */
        }
        .splash-button .btn {
            font-size: 1.5em; /* Larger font size for the button */
            padding: 15px 30px;
        }
      "))
    ),
    tabItems(
      # --- NEW: Splash Screen Tab ---
      tabItem(tabName = "splash",
              div(class = "splash-container",
                  tags$img(src = "https://mohamedassoud.com/wp-content/uploads/2025/07/white.svg",
                           class = "splash-logo",
                           alt = "App Logo"),
                  div(class = "splash-button",
                      actionButton("start_app_btn", "Start", class = "btn-primary") # Use btn-primary for accent color
                  )
              )
      ),
      
      # --- Welcome Tab ---
      tabItem(tabName = "welcome",
              h2("Welcome to the Brand Hate Detector !"), # Changed title
              box(width = 12,
                  h4("Purpose"),
                  p(HTML("This Shiny application is developed for <strong>purely academic purposes</strong> to detect and analyze different levels of 'brand hate' from online consumer reviews. The scraping of data from ConsumerAffairs.com is solely for this research and adheres to responsible data collection and privacy policies.")),
                  p("Developed by: Mohamed ASSOUD"),
                  
                  h4("How it Works"),
                  tags$ul(
                    tags$li("Begin by navigating through the steps in the sidebar menu on the left."),
                    tags$li("Each step performs a specific task, from data collection to advanced visualization."),
                    tags$li("Click 'Generate' or 'Perform' buttons within each step to process data and view results."),
                    tags$li("Plots in later steps are reactive and will update based on the data processed in preceding steps."),
                    tags$li("You can download all plots as PNG or TIFF images for inclusion in your reports or papers.")
                  ),
                  
                  h4("References:"),
                  tags$ul(
                    tags$li("Zhang, C., & Laroche, M. (2020). Brand hate: a multidimensional construct. ", tags$a(href="https://doi.org/10.1108/JPBM-11-2018-2103", "Journal of Product & Brand Management, 29(5), 589-601.", target="_blank")),
                    tags$li("Kucuk, S. U. (2019). Consumer brand hate: steamrolling whatever I see. ", tags$a(href="https://doi.org/10.1007/978-3-030-00380-7", "Psychology & Marketing, 36(5), 431-443.", target="_blank")),
                    tags$li("Sternberg, R. J. (2005). ", tags$a(href="https://www.cambridge.org/core/books/nature-of-hate/51197F6AC4AB66A490032383AD7D9595", "The Nature of Hate", target="_blank"), ". Cambridge University Press."),
                    tags$li("Find more of my academic work on ", tags$a(href="https://scholar.google.com/citations?user=CnxJI1cAAAAJ&hl=en&oi=ao", "Google Scholar", target="_blank"), " and ", tags$a(href="https://orcid.org/0000-0003-3425-2639", "ORCID", target="_blank"), "."),
                    tags$li(HTML("<strong>R Packages Used & Their CRAN Pages:</strong>")),
                    tags$ul(
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/shiny/index.html", "shiny", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/shinydashboard/index.html", "shinydashboard", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/rvest/index.html", "rvest", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/stringr/index.html", "stringr", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/dplyr/index.html", "dplyr", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/DT/index.html", "DT", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/sentimentr/index.html", "sentimentr", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/tidytext/index.html", "tidytext", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/stopwords/index.html", "stopwords", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/ggplot2/index.html", "ggplot2", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/tidyr/index.html", "tidyr", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/purrr/index.html", "purrr", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/scales/index.html", "scales", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/wordcloud/index.html", "wordcloud", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/RColorBrewer/index.html", "RColorBrewer", target="_blank")),
                      tags$li(tags$a(href="https://cran.r-project.org/web/packages/base64enc/index.html", "base64enc", target="_blank"))
                    )
                  ),
                  br(),
                  fluidRow(
                    column(width = 12, align = "right",
                           actionButton("start_analysis_btn", "Go to Step 1: Brand Information", icon = icon("play-circle")) # Updated button text
                    )
                  )
              )
      ),
      
      # --- Step 1 Tab ---
      tabItem(tabName = "step1_brand_info",
              h2("Step 1: Brand Information"),
              box(width = 12,
                  p("Please enter the full URL of the brand's ConsumerAffairs.com page you wish to analyze. For example: ", tags$code("https://www.consumeraffairs.com/cell_phones/cricket_cellular.html")),
                  textInput("brand_url", "Brand URL:",
                            value = "",
                            placeholder = "e.g., https://www.consumeraffairs.com/cell_phones/cricket_cellular.html"),
                  actionButton("scrape_brand_btn", "Fetch Brand Information"),
                  hr(),
                  h4("Brand Information Preview:"),
                  htmlOutput("brand_logo_output"),
                  h4(textOutput("brand_name_output")),
                  uiOutput("total_reviews_display"), # UPDATED: Use uiOutput
                  br(),
                  fluidRow(
                    column(width = 6, align = "left",
                           actionButton("prev_step1", "Previous: Welcome", icon = icon("arrow-left"))), # Nav to Welcome
                    column(width = 6, align = "right",
                           actionButton("next_step1", "Next: Scrape Reviews", icon = icon("arrow-right"))
                    )
                  )
              )
      ),
      
      # --- Step 2 Tab ---
      tabItem(tabName = "step2_scrape_reviews",
              h2("Step 2: Scrape Reviews"),
              box(width = 12,
                  h4("Start Review Scraping"),
                  p("This step initiates the web scraping process to collect consumer reviews for the specified brand from ConsumerAffairs.com. This may take some time depending on the number of reviews."),
                  actionButton("scrape_reviews_btn", "Start Review Scraping"),
                  hr(),
                  uiOutput("status_message_display") # UPDATED: Use uiOutput
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step2", "Previous: Brand Information", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step2", "Next: View Reviews Table", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Reviews Table View Tab ---
      tabItem(tabName = "review_table_view",
              h2("Scraped Reviews Data"),
              box(width = 12,
                  downloadButton("download_reviews_table", "Download Table as CSV"),
                  DTOutput("reviews_table")
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step3_from_all", "Previous: Scrape Reviews", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step3_to_negative", "Next: View Negative Reviews", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 3 Tab ---
      tabItem(tabName = "step3_negative_reviews",
              h2("Step 3: Negative Reviews Analysis"),
              box(width = 12,
                  h4("Filtered Negative Reviews"),
                  p("This table displays reviews identified as negative based on sentiment analysis."),
                  actionButton("analyze_sentiment_btn", "Analyze and Filter Negative Reviews"),
                  downloadButton("download_negative_reviews_table", "Download Table as CSV"),
                  hr(),
                  uiOutput("negative_sentiment_status_display"), # UPDATED: Use uiOutput
                  DTOutput("negative_reviews_table")
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step4_from_neg", "Previous: Reviews Table", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step4_to_stopwords", "Next: Stopword-Free Neg. Reviews", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 4 Tab ---
      tabItem(tabName = "step4_stopwords",
              h2("Step 4: Stopword-Free Negative Reviews"),
              box(width = 12,
                  h4("Negative Reviews with Stopwords Removed"),
                  p("This table displays the negative reviews with common English stopwords removed."),
                  actionButton("remove_stopwords_btn", "Remove Stopwords from Negative Reviews"),
                  downloadButton("download_stopword_free_reviews_table", "Download Table as CSV"),
                  hr(),
                  uiOutput("stopwords_status_display"), # UPDATED: Use uiOutput
                  DTOutput("stopword_free_reviews_table")
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step5_from_stopwords", "Previous: Negative Reviews", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step5_to_nrc", "Next: NRC Emotion Analysis", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 5 Tab ---
      tabItem(tabName = "step5_nrc_emotions",
              h2("Step 5: NRC Emotion Analysis of Negative Reviews"),
              box(width = 12,
                  h4("Emotion Counts in Negative Reviews (NRC Lexicon)"),
                  p("This table shows the counts of anger, sadness, surprise, fear, and disgust words in the stopword-free negative reviews, based on the NRC Word-Emotion Association Lexicon and its theoretical foundations."),
                  actionButton("analyze_nrc_emotions_btn", "Perform NRC Emotion Analysis"),
                  downloadButton("download_nrc_emotions_table", "Download Table as CSV"),
                  hr(),
                  uiOutput("nrc_status_display"), # UPDATED: Use uiOutput
                  DTOutput("nrc_emotions_table")
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step6_from_nrc", "Previous: Stopword-Free Neg. Reviews", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step6_to_plots", "Next: Emotion Visualizations", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 6 Tab ---
      tabItem(tabName = "step6_plots",
              h2("Step 6: Visualizations of Brand Hate Emotions"),
              fluidRow(
                box(width = 12,
                    h4("Overall Emotion Distribution (Bar Plot)"),
                    p("This bar plot shows the total count of words associated with anger, sadness, surprise, fear, and disgust across all negative reviews."),
                    actionButton("generate_plots_btn", "Generate Emotion Plots"),
                    downloadButton("download_emotion_bar_plot", "Download Bar Plot (PNG)"),
                    downloadButton("download_emotion_bar_plot_tiff", "Download Bar Plot (TIFF)"),
                    hr(),
                    plotOutput("emotion_bar_plot")
                )
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step7_from_plots", "Previous: NRC Emotion Analysis", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step7_to_intensification", "Next: Hate Intensification", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 7 Tab ---
      tabItem(tabName = "step7_intensification",
              h2("Step 7: Brand Hate Classification"),
              box(width = 12,
                  h4("Classified Brand Hate Levels"),
                  p("Reviews are classified into Mild, Moderate, or Strong Brand Hate levels, or remain Unclassified. This classification uses a hybrid approach, combining the presence of specific emotion words (anger, sadness, surprise, fear, disgust) from the NRC lexicon with the overall sentiment score from previous analysis. This method aims to capture both the qualitative emotional profile and the quantitative intensity of hate."),
                  tags$ul(
                    tags$li(HTML("<strong>Strong Hate:</strong> Contains Anger, Sadness, Fear, AND Disgust emotion words; OR has a Sentiment Score below (Mean - 2*SD) of the dataset.")),
                    tags$li(HTML("<strong>Moderate Hate:</strong> Contains Anger, Sadness, Fear, AND Surprise words (and not Strong Hate); OR has a Sentiment Score below (Mean - 1*SD) but not below (Mean - 2*SD) of the dataset.")),
                    tags$li(HTML("<strong>Mild Hate:</strong> Contains only Anger AND Sadness words (and not Moderate/Strong Hate); OR has a Sentiment Score below the Mean but not below (Mean - 1*SD) of the dataset.")),
                    tags$li(HTML("<strong>Unclassified:</strong> Any other combination of emotion words or sentiment score not fitting the above criteria; these will be further re-classified in Step 8."))
                  ),
                  p(HTML("<strong>Current Sentiment Score Statistics (from all negative reviews in Step 5):</strong>")),
                  htmlOutput("sentiment_stats_output"),
                  actionButton("classify_hate_btn", "Classify Brand Hate"),
                  downloadButton("download_hate_intensification_table", "Download Table as CSV"),
                  hr(),
                  uiOutput("intensification_status_display"), # UPDATED: Use uiOutput
                  DTOutput("hate_intensification_table")
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step8_from_intensification", "Previous: Emotion Visualizations", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step8_to_final_classification", "Next: Final Classification", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 8 Tab ---
      tabItem(tabName = "step8_final_classification",
              h2("Step 8: Final Brand Hate Classification"),
              box(width = 12,
                  h4("Re-classified 'Unclassified' Reviews"),
                  p("This step attempts to re-classify reviews that were previously marked 'Unclassified' in Step 7, based on a new set of rules focusing on the presence of specific emotional components:"),
                  tags$ul(
                    tags$li(HTML("<strong>Reclassify to Strong:</strong> If it contains at least Anger AND Sadness words.")),
                    tags$li(HTML("<strong>Reclassify to Moderate:</strong> If it contains at least Disgust AND Sadness words (and not Strong).")),
                    tags$li(HTML("<strong>Reclassify to Mild:</strong> If it contains at least Surprise AND Fear words (and not Moderate or Strong).")),
                    tags$li(HTML("<strong>Remain 'Unclassified (No Emotional Pattern)':</strong> If it still doesn't fit any of the above criteria."))
                  ),
                  actionButton("reclassify_unclassified_btn", "Perform Final Classification"),
                  downloadButton("download_final_classification_table", "Download Table as CSV"),
                  hr(),
                  uiOutput("final_classification_status_display"), # UPDATED: Use uiOutput
                  DTOutput("final_classification_table")
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step9_from_final", "Previous: Hate Intensification", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step9_to_summary", "Next: Summary Plots", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 9 Tab ---
      tabItem(tabName = "step9_summary_plots",
              h2("Step 9: Brand Hate Summary Visualizations"),
              box(width = 12,
                  h4("Distribution of Final Brand Hate Levels"),
                  p("This plot shows the number of reviews classified into each brand hate level (Strong, Moderate, Mild, Hybrid) after the final classification step, with reclassified categories combined."),
                  actionButton("generate_summary_plots_btn", "Generate Summary Plots"),
                  downloadButton("download_hate_level_distribution_plot", "Download Hate Level Distribution Plot (PNG)"),
                  downloadButton("download_hate_level_distribution_plot_tiff", "Download Hate Level Distribution Plot (TIFF)"),
                  hr(),
                  plotOutput("hate_level_distribution_plot")
              ),
              fluidRow(
                box(width = 6,
                    h4("Brand Hate Reviews out of All Negative Reviews"),
                    downloadButton("download_hate_from_negative_plot", "Download Hate vs Negative Plot (PNG)"),
                    downloadButton("download_hate_from_negative_plot_tiff", "Download Hate vs Negative Plot (TIFF)"),
                    p("This plot shows the proportion of total negative reviews (from Step 3) that were ultimately classified into any brand hate level (Mild, Moderate, Strong, or Reclassified categories). The percentage of each category is shown within the pie slices."),
                    plotOutput("hate_from_negative_plot")
                ),
                box(width = 6,
                    h4("Negative Reviews out of All Scraped Reviews"),
                    downloadButton("download_negative_from_all_plot", "Download Negative vs All Plot (PNG)"),
                    downloadButton("download_negative_from_all_plot_tiff", "Download Negative vs All Plot (TIFF)"),
                    p("This plot shows the proportion of all scraped reviews (from Step 2) that were identified as negative (in Step 3). The percentage of each category is shown within the pie slices."),
                    plotOutput("negative_from_all_plot")
                )
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step10_from_summary", "Previous: Final Classification", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step10_to_reasons", "Next: Reasons Analysis", icon = icon("arrow-right"))
                )
              )
      ),
      
      # --- Step 10 Tab ---
      tabItem(tabName = "step10_reasons_plots",
              h2("Step 10: Reasons per Brand Hate Type Analysis"),
              box(width = 12,
                  h4("Top Reasons by Brand Hate Level"),
                  p("These plots show the most frequent reasons associated with reviews within each final brand hate classification level. This helps to understand specific drivers for different intensities of hate."),
                  actionButton("generate_reasons_plots_btn", "Generate Reasons Plots"),
                  hr(),
                  fluidRow(
                    column(width = 6, tags$h5("Strong Hate"), downloadButton("download_reasons_strong_plot", "Download Strong Hate Reasons (PNG)"), downloadButton("download_reasons_strong_plot_tiff", "Download Strong Hate Reasons (TIFF)"), plotOutput("reasons_strong_plot")),
                    column(width = 6, tags$h5("Moderate Hate"), downloadButton("download_reasons_moderate_plot", "Download Moderate Hate Reasons (PNG)"), downloadButton("download_reasons_moderate_plot_tiff", "Download Moderate Hate Reasons (TIFF)"), plotOutput("reasons_moderate_plot"))
                  ),
                  fluidRow(
                    column(width = 6, tags$h5("Mild Hate"), downloadButton("download_reasons_mild_plot", "Download Mild Hate Reasons (PNG)"), downloadButton("download_reasons_mild_plot_tiff", "Download Mild Hate Reasons (TIFF)"), plotOutput("reasons_mild_plot")),
                    column(width = 6, tags$h5("Hybrid Hate"), downloadButton("download_reasons_hybrid_plot", "Download Hybrid Hate Reasons (PNG)"), downloadButton("download_reasons_hybrid_plot_tiff", "Download Hybrid Hate Reasons (TIFF)"), plotOutput("reasons_unclassified_plot"))
                  )
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step11_from_reasons", "Previous: Summary Plots", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step11_to_wordclouds", "Next: Emotion Word Clouds", icon = icon("arrow-right"))
                )
              )
      ),
      # --- Step 11 Tab ---
      tabItem(tabName = "step11_word_clouds",
              h2("Step 11: Emotion Word Clouds by Brand Hate Type"),
              box(width = 12,
                  h4("Emotion Word Clouds"),
                  p("These word clouds visualize the most frequent emotion-laden words for each type of brand hate (Strong, Moderate, Mild, Hybrid) based on words detected in Step 8. Larger words indicate higher frequency."),
                  actionButton("generate_wordclouds_btn", "Generate Word Clouds"),
                  hr(),
                  fluidRow(
                    column(width = 6, tags$h5("Strong Hate"), downloadButton("download_wordcloud_strong", "Download Strong Word Cloud (PNG)"), downloadButton("download_wordcloud_strong_tiff", "Download Strong Word Cloud (TIFF)"), imageOutput("wordcloud_strong")),
                    column(width = 6, tags$h5("Moderate Hate"), downloadButton("download_wordcloud_moderate", "Download Moderate Word Cloud (PNG)"), downloadButton("download_wordcloud_moderate_tiff", "Download Moderate Word Cloud (TIFF)"), imageOutput("wordcloud_moderate"))
                  ),
                  fluidRow(
                    column(width = 6, tags$h5("Mild Hate"), downloadButton("download_wordcloud_mild", "Download Mild Word Cloud (PNG)"), downloadButton("download_wordcloud_mild_tiff", "Download Mild Word Cloud (TIFF)"), imageOutput("wordcloud_mild")),
                    column(width = 6, tags$h5("Hybrid Hate"), downloadButton("download_wordcloud_hybrid", "Download Hybrid Word Cloud (PNG)"), downloadButton("download_wordcloud_hybrid_tiff", "Download Hybrid Word Cloud (TIFF)"), imageOutput("wordcloud_unclassified"))
                  )
              ),
              fluidRow(
                column(width = 6, align = "left",
                       actionButton("prev_step12_from_wordclouds", "Previous: Reasons Analysis", icon = icon("arrow-left"))),
                column(width = 6, align = "right",
                       actionButton("next_step12_to_bubble_plot", "Next: Hate Intensity Bubble Plot", icon = icon("arrow-right"))
                )
              )
      ),
      # --- Step 12 Tab ---
      tabItem(tabName = "step12_bubble_plot",
              h2("Step 12: Hate Intensity Bubble Plot"),
              box(width = 12,
                  h4("Distribution of Reviews by Hate Type and Sentiment Score"),
                  p("This bubble plot visualizes each review's final hate classification and its associated sentiment score. Each bubble represents a review, colored by its hate level. Jitter is applied to prevent overplotting."),
                  actionButton("generate_bubble_plot_btn", "Generate Bubble Plot"),
                  downloadButton("download_bubble_plot", "Download Bubble Plot (PNG)"),
                  downloadButton("download_bubble_plot_tiff", "Download Bubble Plot (TIFF)"),
                  hr(),
                  plotOutput("hate_bubble_plot")
              ),
              fluidRow(
                column(width = 12, align = "left",
                       actionButton("prev_step13_from_bubble_plot", "Previous: Emotion Word Clouds", icon = icon("arrow-left"))
                )
              )
      )
    )
  )
)

# --- Shiny Server ---
server <- function(input, output, session) {
  
  # Reactive value to store brand info
  brand_info <- reactiveVal(NULL)
  
  # Reactive value to store all scraped reviews data
  reviews_data <- reactiveVal(NULL)
  
  # Reactive value to store filtered negative reviews data
  negative_reviews_data <- reactiveVal(NULL)
  
  # Reactive value to store stopword-filtered negative reviews
  stopword_free_negative_reviews <- reactiveVal(NULL)
  
  # Reactive value to store NRC emotion analysis results
  nrc_emotion_data <- reactiveVal(NULL)
  
  # Reactive value to store brand hate intensification results from Step 7
  hate_intensification_data <- reactiveVal(NULL)
  
  # Reactive value to store final classification results from Step 8
  final_classification_data <- reactiveVal(NULL)
  
  
  # Reactive value for general status messages
  status_message_text <- reactiveVal("Ready to fetch brand information.")
  
  # Reactive value for negative sentiment analysis status messages
  negative_sentiment_status_text <- reactiveVal("Click 'Analyze and Filter Negative Reviews' to start.")
  
  # Reactive value for stopword removal status messages
  stopwords_status_text <- reactiveVal("Click 'Remove Stopwords from Negative Reviews' to start.")
  
  # Reactive value for NRC analysis status messages
  nrc_status_text <- reactiveVal("Click 'Perform NRC Emotion Analysis' to start.")
  
  # Reactive value for hate intensification status messages (Step 7)
  intensification_status_text <- reactiveVal("Click 'Classify Brand Hate' to start.")
  
  # Reactive value for final classification status messages (Step 8)
  final_classification_status_text <- reactiveVal("Click 'Perform Final Classification' to start.")
  
  
  # --- Display Outputs for Status Messages (using renderUI) ---
  # These new output observers handle embedding the reactive text within HTML for display
  output$total_reviews_display <- renderUI({
    info <- brand_info()
    if (!is.null(info$total_reviews)) {
      HTML(paste0("Total Reviews: <strong>", as.character(info$total_reviews), "</strong>"))
    } else {
      HTML("Total Reviews: <strong>N/A</strong>")
    }
  })
  
  output$status_message_display <- renderUI({
    HTML(paste0("Status: <strong>", status_message_text(), "</strong>"))
  })
  
  output$negative_sentiment_status_display <- renderUI({
    HTML(paste0("Negative Reviews Status: <strong>", negative_sentiment_status_text(), "</strong>"))
  })
  
  output$stopwords_status_display <- renderUI({
    HTML(paste0("Status: <strong>", stopwords_status_text(), "</strong>"))
  })
  
  output$nrc_status_display <- renderUI({
    HTML(paste0("NRC Analysis Status: <strong>", nrc_status_text(), "</strong>"))
  })
  
  output$intensification_status_display <- renderUI({
    HTML(paste0("Classification Status: <strong>", intensification_status_text(), "</strong>"))
  })
  
  output$final_classification_status_display <- renderUI({
    HTML(paste0("Final Classification Status: <strong>", final_classification_status_text(), "</strong>"))
  })
  
  # Original renderText outputs (can be removed as the _display versions now handle it)
  # output$status_message <- renderText({ status_message_text() })
  # output$negative_sentiment_status <- renderText({ negative_sentiment_status_text() })
  # output$stopwords_status <- renderText({ stopwords_status_text() })
  # output$nrc_status <- renderText({ nrc_status_text() })
  # output$intensification_status <- renderText({ intensification_status_text() })
  # output$final_classification_status <- renderText({ final_classification_status_text() })
  
  
  # Output for sentiment statistics for Step 7 (and reference in Step 8)
  output$sentiment_stats_output <- renderUI({
    dat <- nrc_emotion_data()
    if (!is.null(dat) && nrow(dat) > 0) {
      mean_score <- mean(dat$Sentiment_Score, na.rm = TRUE)
      sd_score <- sd(dat$Sentiment_Score, na.rm = TRUE)
      
      # Define thresholds for display purposes
      threshold_mild_upper_display <- round(mean_score, 3) # Equivalent to mean
      threshold_mild_lower_display <- round(mean_score - 1 * sd_score, 3)
      threshold_moderate_lower_display <- round(mean_score - 2 * sd_score, 3)
      
      HTML(paste0("Mean Sentiment Score: ", round(mean_score, 3), "<br/>",
                  "Standard Deviation: ", round(sd_score, 3), "<br/>",
                  "Calculated Thresholds:<br/>",
                  "&nbsp;&nbsp;&nbsp;&nbsp;Mild (Score > ", threshold_mild_lower_display, " to <= ", threshold_mild_upper_display, ")<br/>",
                  "&nbsp;&nbsp;&nbsp;&nbsp;Moderate (Score > ", threshold_moderate_lower_display, " to <= ", threshold_mild_lower_display, ")<br/>",
                  "&nbsp;&nbsp;&nbsp;&nbsp;Strong (Score <= ", threshold_moderate_lower_display, ")"))
    } else {
      HTML("Calculate NRC Emotion Analysis (Step 5) to see statistics and thresholds.")
    }
  })
  
  
  # --- Navigation Logic ---
  # NEW: Observe event for the splash screen "Start" button
  observeEvent(input$start_app_btn, {
    updateTabItems(session, "tabs", selected = "welcome")
  })
  
  observeEvent(input$start_analysis_btn, { # Nav from Welcome to Step 1
    updateTabItems(session, "tabs", selected = "step1_brand_info")
  })
  
  observeEvent(input$prev_step1, { # Nav from Step 1 to Welcome (original)
    updateTabItems(session, "tabs", selected = "welcome")
  })
  
  observeEvent(input$next_step1, { # Nav from Step 1 to Step 2
    updateTabItems(session, "tabs", selected = "step2_scrape_reviews")
  })
  
  observeEvent(input$prev_step2, { # Nav from Step 2 to Step 1
    updateTabItems(session, "tabs", selected = "step1_brand_info")
  })
  
  observeEvent(input$next_step2, { # Nav from Step 2 to Reviews Table
    updateTabItems(session, "tabs", selected = "review_table_view")
  })
  
  observeEvent(input$prev_step3_from_all, { # Nav from Reviews Table to Step 2
    updateTabItems(session, "tabs", selected = "step2_scrape_reviews")
  })
  
  observeEvent(input$next_step3_to_negative, { # Nav from Reviews Table to Step 3
    updateTabItems(session, "tabs", selected = "step3_negative_reviews")
  })
  
  observeEvent(input$prev_step4_from_neg, { # Nav from Step 3 to Reviews Table
    updateTabItems(session, "tabs", selected = "review_table_view")
  })
  
  observeEvent(input$next_step4_to_stopwords, { # Nav from Step 3 to Step 4
    updateTabItems(session, "tabs", selected = "step4_stopwords")
  })
  
  observeEvent(input$prev_step5_from_stopwords, { # Nav from Step 4 to Step 3
    updateTabItems(session, "tabs", selected = "step3_negative_reviews")
  })
  
  observeEvent(input$next_step5_to_nrc, { # Nav from Step 4 to Step 5
    updateTabItems(session, "tabs", selected = "step5_nrc_emotions")
  })
  
  observeEvent(input$prev_step6_from_nrc, { # Nav from Step 5 to Step 4
    updateTabItems(session, "tabs", selected = "step4_stopwords")
  })
  
  observeEvent(input$next_step6_to_plots, { # Nav from Step 5 to Step 6
    updateTabItems(session, "tabs", selected = "step6_plots")
  })
  
  observeEvent(input$prev_step7_from_plots, { # Nav from Step 6 to Step 5
    updateTabItems(session, "tabs", selected = "step5_nrc_emotions")
  })
  
  observeEvent(input$next_step7_to_intensification, { # Nav from Step 6 to Step 7
    updateTabItems(session, "tabs", selected = "step7_intensification")
  })
  
  observeEvent(input$prev_step8_from_intensification, { # Nav from Step 7 to Step 6
    updateTabItems(session, "tabs", selected = "step6_plots")
  })
  
  observeEvent(input$next_step8_to_final_classification, { # Nav from Step 7 to Step 8
    updateTabItems(session, "tabs", selected = "step8_final_classification")
  })
  
  observeEvent(input$prev_step9_from_final, { # Nav from Step 8 to Step 7
    updateTabItems(session, "tabs", selected = "step7_intensification")
  })
  
  observeEvent(input$next_step9_to_summary, { # Nav from Step 8 to Step 9
    updateTabItems(session, "tabs", selected = "step9_summary_plots")
  })
  
  observeEvent(input$prev_step10_from_summary, { # Nav from Step 9 to Step 8
    updateTabItems(session, "tabs", selected = "step8_final_classification")
  })
  
  observeEvent(input$next_step10_to_reasons, { # Nav from Step 9 to Step 10
    updateTabItems(session, "tabs", selected = "step10_reasons_plots")
  })
  
  observeEvent(input$prev_step11_from_reasons, { # Nav from Step 10 to Step 9
    updateTabItems(session, "tabs", selected = "step9_summary_plots")
  })
  
  observeEvent(input$next_step11_to_wordclouds, { # Nav from Step 10 to Step 11
    updateTabItems(session, "tabs", selected = "step11_word_clouds")
  })
  
  observeEvent(input$prev_step12_from_wordclouds, { # Nav from Step 11 to Step 10
    updateTabItems(session, "tabs", selected = "step10_reasons_plots")
  })
  
  observeEvent(input$next_step12_to_bubble_plot, { # Nav from Step 11 to Step 12
    updateTabItems(session, "tabs", selected = "step12_bubble_plot")
  })
  
  observeEvent(input$prev_step13_from_bubble_plot, { # Nav from Step 12 to Step 11
    updateTabItems(session, "tabs", selected = "step11_word_clouds")
  })
  
  # Quit button to stop the app
  observeEvent(input$quit_app, {
    stopApp()
  })
  
  # --- Step 1: Brand Info ---
  observeEvent(input$scrape_brand_btn, {
    # Ensure URL is provided before attempting to scrape
    if (input$brand_url == "") {
      status_message_text("Please enter a brand URL in the text box above.") # More specific message
      return()
    }
    
    status_message_text("Scraping brand information...")
    info <- scrape_brand_info(input$brand_url)
    brand_info(info)
    if (!is.null(info) && !is.null(info$brand_name)) {
      status_message_text(paste0("Brand information for '", info$brand_name, "' fetched successfully! Total reviews: ", info$total_reviews, "."))
    } else {
      status_message_text("Failed to fetch brand information. Please check the URL and your internet connection. (Example: https://www.consumeraffairs.com/cell_phones/cricket_cellular.html)")
    }
  })
  
  output$brand_logo_output <- renderUI({
    info <- brand_info()
    if (!is.null(info$logo_display_html)) {
      HTML(info$logo_display_html)
    } else {
      HTML("<p>No brand logo available or could not be loaded.</p>") # More informative message
    }
  })
  
  output$brand_name_output <- renderText({
    info <- brand_info()
    if (!is.null(info$brand_name)) {
      info$brand_name
    } else {
      "Brand Name: N/A" # More explicit label
    }
  })
  
  
  # --- Step 2: Scrape Reviews ---
  observeEvent(input$scrape_reviews_btn, {
    current_brand_info <- brand_info()
    if (is.null(current_brand_info) || is.null(current_brand_info$brand_name)) {
      status_message_text("Please fetch brand information first in Step 1 to proceed with scraping.")
      return()
    }
    
    # Use withProgress for a progress bar
    withProgress(message = "Scraping reviews...", value = 0, {
      reviews <- scrape_reviews(input$brand_url, session_status_updater = function(msg) {
        incProgress(1/15, detail = msg) # Assuming max_pages_to_scrape is 15
        status_message_text(msg) # Update general status message
      })
      reviews_data(reviews)
      
      if (!is.null(reviews) && nrow(reviews) > 0) {
        status_message_text(paste0("Scraped ", nrow(reviews), " unique reviews successfully! Ready for sentiment analysis."))
      } else {
        status_message_text("Failed to scrape reviews or no reviews found. This may happen if review content is loaded by JavaScript or no reviews exist. Please check the provided URL in Step 1.")
      }
    })
  })
  
  output$reviews_table <- renderDT({
    dat <- reviews_data()
    if (!is.null(dat) && nrow(dat) > 0) {
      datatable(dat %>% select(review_id, reviewer, review, reasons),
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    } else {
      data.frame(Message = "No reviews scraped yet. Click 'Start Review Scraping' in Step 2 to populate this table.")
    }
  })
  
  # Download handler for reviews_table
  output$download_reviews_table <- downloadHandler(
    filename = function() {
      paste("scraped_reviews_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reviews_data(), file, row.names = FALSE)
    }
  )
  
  
  # --- Step 3: Negative Reviews Analysis ---
  observeEvent(input$analyze_sentiment_btn, {
    if (is.null(reviews_data()) || nrow(reviews_data()) == 0) {
      negative_sentiment_status_text("No reviews available to process. Please scrape reviews first in Step 2.")
      negative_reviews_data(NULL)
      return()
    }
    
    negative_sentiment_status_text("Analyzing sentiment and filtering negative reviews...")
    
    # Use withProgress for this step too
    withProgress(message = "Analyzing sentiment...", value = 0, {
      # Get sentences from reviews (sentimentr processes sentence by sentence)
      reviews_for_sentiment <- reviews_data() %>%
        mutate(review_char = as.character(review)) %>%
        get_sentences(text.field = "review_char")
      
      incProgress(0.5, detail = "Calculating sentiment scores...")
      sentiment_scores <- sentiment(reviews_for_sentiment)
      
      # Aggregate sentiment scores by original review_id (element_id corresponds to row in reviews_data)
      review_sentiment_summary <- sentiment_scores %>%
        group_by(element_id) %>%
        summarise(avg_sentiment = mean(sentiment, na.rm = TRUE), .groups = 'drop') %>%
        # Join back with original reviews to get review_id and other details
        mutate(review_id_original = reviews_data()$review_id[element_id])
      
      
      incProgress(0.9, detail = "Filtering negative reviews...")
      # Filter for negative reviews (you can adjust the threshold, e.g., < -0.1)
      negative_filtered_reviews <- review_sentiment_summary %>%
        filter(avg_sentiment < 0) %>%
        left_join(reviews_data() %>% select(review_id, reviewer, review, reasons),
                  by = c("review_id_original" = "review_id")) %>%
        select(review_id = review_id_original, reviewer, review, reasons, sentiment_score = avg_sentiment) %>%
        arrange(sentiment_score) # Order by most negative first
      
      negative_reviews_data(negative_filtered_reviews)
      
      if (!is.null(negative_filtered_reviews) && nrow(negative_filtered_reviews) > 0) {
        negative_sentiment_status_text(paste0("Found ", nrow(negative_filtered_reviews), " negative reviews. Ready for stopword removal."))
      } else {
        negative_sentiment_status_text("No negative reviews found based on the analysis criteria (average sentiment < 0). You may need to adjust the sentiment threshold if you expect negative reviews but none were found.")
      }
    })
  })
  
  output$negative_reviews_table <- renderDT({
    dat <- negative_reviews_data()
    if (!is.null(dat) && nrow(dat) > 0) {
      datatable(dat %>% select(review_id, reviewer, review, reasons, sentiment_score),
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        formatRound('sentiment_score', 3) # Format sentiment score to 3 decimal places
    } else {
      data.frame(Message = "No negative reviews found yet. Click 'Analyze and Filter Negative Reviews' in Step 3.")
    }
  })
  
  # Download handler for negative_reviews_table
  output$download_negative_reviews_table <- downloadHandler(
    filename = function() {
      paste("negative_reviews_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(negative_reviews_data(), file, row.names = FALSE)
    }
  )
  
  
  # --- Step 4: Stopword-Free Negative Reviews ---
  observeEvent(input$remove_stopwords_btn, {
    if (is.null(negative_reviews_data()) || nrow(negative_reviews_data()) == 0) {
      stopwords_status_text("No negative reviews available to process. Please analyze negative reviews first in Step 3.")
      stopword_free_negative_reviews(NULL)
      return()
    }
    
    stopwords_status_text("Removing stopwords from negative reviews...")
    
    withProgress(message = "Removing stopwords...", value = 0, {
      custom_stopwords <- stopwords("en", source = "snowball")
      
      stopword_filtered <- negative_reviews_data() %>%
        mutate(original_full_review = review) %>%
        unnest_tokens(word, review, token = "words") %>%
        anti_join(tibble(word = custom_stopwords), by = "word") %>%
        filter(str_detect(word, "^[[:alpha:]]+$")) %>%
        group_by(review_id) %>%
        summarise(
          reviewer = first(reviewer),
          original_review_snippet = first(original_full_review),
          stopword_free_review = paste(word, collapse = " "),
          reasons = first(reasons),
          sentiment_score = first(sentiment_score),
          .groups = 'drop'
        ) %>%
        ungroup() %>%
        select(review_id, reviewer, stopword_free_review, reasons, sentiment_score, original_review_snippet)
      
      stopword_free_negative_reviews(stopword_filtered)
      
      if (!is.null(stopword_filtered) && nrow(stopword_filtered) > 0) {
        stopwords_status_text(paste0("Stopwords removed from ", nrow(stopword_filtered), " negative reviews. Ready for NRC emotion analysis."))
      } else {
        stopwords_status_text("No reviews remaining after stopword removal. This might happen if all reviews were very short or contained only stopwords, or if no negative reviews were found in Step 3.")
      }
    })
  })
  
  output$stopword_free_reviews_table <- renderDT({
    dat <- stopword_free_negative_reviews()
    if (!is.null(dat) && nrow(dat) > 0) {
      datatable(dat,
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        formatRound('sentiment_score', 3)
    } else {
      data.frame(Message = "No stopword-free negative reviews found yet. Click 'Remove Stopwords from Negative Reviews' in Step 4.")
    }
  })
  
  # Download handler for stopword_free_reviews_table
  output$download_stopword_free_reviews_table <- downloadHandler(
    filename = function() {
      paste("stopword_free_reviews_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(stopword_free_negative_reviews(), file, row.names = FALSE)
    }
  )
  
  
  # --- Step 5: NRC Emotion Analysis ---
  observeEvent(input$analyze_nrc_emotions_btn, {
    if (is.null(stopword_free_negative_reviews()) || nrow(stopword_free_negative_reviews()) == 0) {
      nrc_status_text("No stopword-free negative reviews available to analyze. Please complete Step 4 first.")
      nrc_emotion_data(NULL)
      return()
    }
    
    nrc_status_text("Performing NRC emotion analysis on negative reviews...")
    
    withProgress(message = "Performing NRC analysis...", value = 0, {
      nrc_lexicon <- get_sentiments("nrc") %>%
        filter(sentiment %in% c("anger", "sadness", "surprise", "fear", "disgust"))
      
      # Check if nrc_lexicon has any rows
      if (nrow(nrc_lexicon) == 0) {
        nrc_status_text("NRC lexicon is empty for selected emotions. This is unexpected. Please check NRC setup.")
        nrc_emotion_data(NULL)
        return()
      }
      
      review_emotions <- stopword_free_negative_reviews() %>%
        unnest_tokens(word, stopword_free_review) %>%
        inner_join(nrc_lexicon, by = "word", relationship = "many-to-many") %>%
        group_by(review_id, reviewer, original_review_snippet, sentiment_score, reasons) %>%
        summarise(
          anger_words = sum(sentiment == "anger", na.rm = TRUE),
          sadness_words = sum(sentiment == "sadness", na.rm = TRUE),
          surprise_words = sum(sentiment == "surprise", na.rm = TRUE),
          fear_words = sum(sentiment == "fear", na.rm = TRUE),
          disgust_words = sum(sentiment == "disgust", na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          anger_words = tidyr::replace_na(anger_words, 0),
          sadness_words = tidyr::replace_na(sadness_words, 0),
          surprise_words = tidyr::replace_na(surprise_words, 0),
          fear_words = tidyr::replace_na(fear_words, 0),
          disgust_words = tidyr::replace_na(disgust_words, 0)
        ) %>%
        mutate(
          total_emotion_words = anger_words + sadness_words + surprise_words + fear_words + disgust_words
        ) %>%
        select(
          review_id,
          reviewer,
          Anger = anger_words,
          Sadness = sadness_words,
          Surprise = surprise_words,
          Fear = fear_words,
          Disgust = disgust_words,
          `Total Emotion Words` = total_emotion_words,
          Original_Review = original_review_snippet,
          Sentiment_Score = sentiment_score,
          Reasons = reasons
        ) %>%
        arrange(review_id)
      
      nrc_emotion_data(review_emotions)
      
      if (!is.null(review_emotions) && nrow(review_emotions) > 0) {
        nrc_status_text(paste0("NRC emotion analysis complete for ", nrow(review_emotions), " negative reviews. Ready for emotion visualizations."))
      } else {
        nrc_status_text("No emotion words found in the negative reviews based on the NRC lexicon. This may occur if reviews are very short or do not contain strong emotional language.")
      }
    })
  })
  
  output$nrc_emotions_table <- renderDT({
    dat <- nrc_emotion_data()
    if (!is.null(dat) && nrow(dat) > 0) {
      datatable(dat,
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        formatRound('Sentiment_Score', 3)
    } else {
      data.frame(Message = "No NRC emotion analysis performed yet. Click 'Perform NRC Emotion Analysis' in Step 5.")
    }
  })
  
  # Download handler for nrc_emotions_table
  output$download_nrc_emotions_table <- downloadHandler(
    filename = function() {
      paste("nrc_emotion_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nrc_emotion_data(), file, row.names = FALSE)
    }
  )
  
  # --- Step 6: Emotion Visualizations ---
  
  # Reactive expression to hold the emotion bar plot (for rendering and downloading)
  emotion_bar_plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$generate_plots_btn, {
    if (is.null(nrc_emotion_data()) || nrow(nrc_emotion_data()) == 0) {
      # No specific error message output for plots, but plots will be empty.
      emotion_bar_plot_obj(NULL) # Clear plot object
      return()
    }
    
    withProgress(message = "Generating emotion plots...", value = 0, {
      emotion_columns <- c("Anger", "Sadness", "Surprise", "Fear", "Disgust")
      emotion_totals <- nrc_emotion_data() %>%
        select(all_of(emotion_columns)) %>%
        summarise(across(everything(), sum)) %>%
        pivot_longer(everything(), names_to = "Emotion", values_to = "Count")
      
      plot <- ggplot(emotion_totals, aes(x = reorder(Emotion, -Count), y = Count, fill = Emotion)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = Count), vjust = -0.5, size = 5) +
        labs(title = "Total Word Counts per Emotion in Negative Reviews",
             x = "Emotion",
             y = "Total Word Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12)) +
        scale_fill_manual(values = c("Anger" = "#FF0000", "Sadness" = "#4682B4",
                                     "Surprise" = "#FFD700", "Fear" = "#800080",
                                     "Disgust" = "#006400"))
      emotion_bar_plot_obj(plot)
      incProgress(1, detail = "Plots generated! You can now proceed to Brand Hate Classification.")
    })
  })
  
  output$emotion_bar_plot <- renderPlot({
    plot_obj <- emotion_bar_plot_obj()
    if (is.null(plot_obj)) {
      ggplot() + annotate("text", x=0.5, y=0.5, label="Click 'Generate Emotion Plots' to see the plot.", size=5, color="gray") + theme_void()
    } else {
      plot_obj
    }
  })
  
  output$download_emotion_bar_plot <- downloadHandler(
    filename = function() {
      paste("emotion_bar_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Re-generate the plot for saving to ensure it's fresh if reactive value was cleared
      emotion_columns <- c("Anger", "Sadness", "Surprise", "Fear", "Disgust")
      emotion_totals <- nrc_emotion_data() %>%
        select(all_of(emotion_columns)) %>%
        summarise(across(everything(), sum)) %>%
        pivot_longer(everything(), names_to = "Emotion", values_to = "Count")
      
      plot_to_save <- ggplot(emotion_totals, aes(x = reorder(Emotion, -Count), y = Count, fill = Emotion)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = Count), vjust = -0.5, size = 5) +
        labs(title = "Total Word Counts per Emotion in Negative Reviews",
             x = "Emotion",
             y = "Total Word Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12)) +
        scale_fill_manual(values = c("Anger" = "#FF0000", "Sadness" = "#4682B4",
                                     "Surprise" = "#FFD700", "Fear" = "#800080",
                                     "Disgust" = "#006400"))
      ggsave(file, plot = plot_to_save, device = "png", width = 10, height = 6)
    }
  )
  
  output$download_emotion_bar_plot_tiff <- downloadHandler(
    filename = function() {
      paste("emotion_bar_plot_", Sys.Date(), ".tiff", sep = "")
    },
    content = function(file) {
      # Re-generate the plot for saving to ensure it's fresh if reactive value was cleared
      emotion_columns <- c("Anger", "Sadness", "Surprise", "Fear", "Disgust")
      emotion_totals <- nrc_emotion_data() %>%
        select(all_of(emotion_columns)) %>%
        summarise(across(everything(), sum)) %>%
        pivot_longer(everything(), names_to = "Emotion", values_to = "Count")
      
      plot_to_save <- ggplot(emotion_totals, aes(x = reorder(Emotion, -Count), y = Count, fill = Emotion)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = Count), vjust = -0.5, size = 5) +
        labs(title = "Total Word Counts per Emotion in Negative Reviews",
             x = "Emotion",
             y = "Total Word Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12)) +
        scale_fill_manual(values = c("Anger" = "#FF0000", "Sadness" = "#4682B4",
                                     "Surprise" = "#FFD700", "Fear" = "#800080",
                                     "Disgust" = "#006400"))
      ggsave(file, plot = plot_to_save, device = "tiff", width = 10, height = 6, dpi = 600)
    }
  )
  
  
  # Heatmap removed, now rendering a placeholder plot
  output$emotion_heatmap <- renderPlot({
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Heatmap visualization removed as per request.",
               size = 6, color = "darkgray") +
      theme_void()
  })
  
  
  # --- Step 7: Brand Hate Intensification ---
  observeEvent(input$classify_hate_btn, {
    if (is.null(nrc_emotion_data()) || nrow(nrc_emotion_data()) == 0) {
      intensification_status_text("No NRC emotion data available to classify. Please complete Step 5 first.")
      hate_intensification_data(NULL)
      return()
    }
    
    intensification_status_text("Classifying brand hate levels based on emotion presence and sentiment score distribution...")
    
    withProgress(message = "Classifying hate levels...", value = 0, {
      current_mean_sentiment <- mean(nrc_emotion_data()$Sentiment_Score, na.rm = TRUE)
      current_sd_sentiment <- sd(nrc_emotion_data()$Sentiment_Score, na.rm = TRUE)
      
      sentiment_threshold_strong <- current_mean_sentiment - 2 * current_sd_sentiment
      sentiment_threshold_moderate <- current_mean_sentiment - 1 * current_sd_sentiment
      sentiment_threshold_mild <- current_mean_sentiment
      
      nrc_lexicon_full <- get_sentiments("nrc") %>%
        filter(sentiment %in% c("anger", "sadness", "surprise", "fear", "disgust"))
      
      initial_classified_data <- nrc_emotion_data() %>%
        rowwise() %>%
        mutate(
          has_anger = Anger > 0,
          has_sadness = Sadness > 0,
          has_surprise = Surprise > 0,
          has_fear = Fear > 0,
          has_disgust = Disgust > 0
        ) %>%
        mutate(
          Hate_Level = case_when(
            (has_anger & has_sadness & has_fear & has_disgust) ~ "Strong",
            (has_anger & has_sadness & has_fear & has_surprise) ~ "Moderate",
            (has_anger & has_sadness & !has_fear & !has_surprise & !has_disgust) ~ "Mild",
            TRUE ~ "Unclassified"
          )
        ) %>%
        ungroup()
      
      classified_data_with_words <- initial_classified_data %>%
        rowwise() %>%
        mutate(
          review_words_with_emotions = list(
            Original_Review %>%
              tibble(text = .) %>%
              unnest_tokens(word, text) %>%
              inner_join(nrc_lexicon_full, by = "word", relationship = "many-to-many") %>%
              filter(str_detect(word, "^[[:alpha:]]+$")) %>%
              group_by(sentiment) %>%
              summarise(words = paste(unique(word), collapse = ", "), .groups = 'drop')
          )
        ) %>%
        ungroup() %>%
        mutate(
          Words_Anger = map_chr(review_words_with_emotions, ~{
            .x %>% filter(sentiment == "anger") %>% pull(words) %>% paste(collapse = ", ")
          }),
          Words_Sadness = map_chr(review_words_with_emotions, ~{
            .x %>% filter(sentiment == "sadness") %>% pull(words) %>% paste(collapse = ", ")
          }),
          Words_Surprise = map_chr(review_words_with_emotions, ~{
            .x %>% filter(sentiment == "surprise") %>% pull(words) %>% paste(collapse = ", ")
          }),
          Words_Fear = map_chr(review_words_with_emotions, ~{
            .x %>% filter(sentiment == "fear") %>% pull(words) %>% paste(collapse = ", ")
          }),
          Words_Disgust = map_chr(review_words_with_emotions, ~{
            .x %>% filter(sentiment == "disgust") %>% pull(words) %>% paste(collapse = ", ")
          })
        ) %>%
        mutate(across(starts_with("Words_"), ~na_if(., ""))) %>%
        select(
          review_id,
          reviewer,
          Hate_Level,
          Words_Anger,
          Words_Sadness,
          Words_Surprise,
          Words_Fear,
          Words_Disgust,
          Reasons = Reasons,
          Original_Review,
          Sentiment_Score
        ) %>%
        arrange(review_id)
      
      hate_intensification_data(classified_data_with_words)
      
      if (!is.null(classified_data_with_words) && nrow(classified_data_with_words) > 0) {
        intensification_status_text(paste0("Brand hate levels classified for ", nrow(classified_data_with_words), " reviews. (Note: 'Unclassified' means emotion combination or sentiment score did not fit initial emotion rules. These will be further re-classified in Step 8)."))
      } else {
        intensification_status_text("No reviews classified based on the defined criteria in Step 7.")
      }
    })
  })
  
  output$hate_intensification_table <- renderDT({
    dat <- hate_intensification_data()
    if (!is.null(dat) && nrow(dat) > 0) {
      datatable(dat,
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        formatRound('Sentiment_Score', 3)
    } else {
      data.frame(Message = "No brand hate classification data available yet. Click 'Classify Brand Hate' in Step 7.")
    }
  })
  
  # Download handler for hate_intensification_table
  output$download_hate_intensification_table <- downloadHandler(
    filename = function() {
      paste("hate_intensification_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hate_intensification_data(), file, row.names = FALSE)
    }
  )
  
  # --- Step 8: Final Brand Hate Classification ---
  observeEvent(input$reclassify_unclassified_btn, {
    if (is.null(hate_intensification_data()) || nrow(hate_intensification_data()) == 0) {
      final_classification_status_text("No data from Step 7 available to re-classify. Please complete Step 7 first.")
      final_classification_data(NULL)
      return()
    }
    
    final_classification_status_text("Re-classifying 'Unclassified' reviews based on emotional patterns and sentiment score proximity...")
    
    withProgress(message = "Performing final classification...", value = 0, {
      data_for_reclassification <- hate_intensification_data() %>%
        left_join(
          nrc_emotion_data() %>%
            select(review_id, Anger_orig = Anger, Sadness_orig = Sadness,
                   Surprise_orig = Surprise, Fear_orig = Fear, Disgust_orig = Disgust),
          by = "review_id"
        )
      
      current_mean_sentiment <- mean(nrc_emotion_data()$Sentiment_Score, na.rm = TRUE)
      current_sd_sentiment <- sd(nrc_emotion_data()$Sentiment_Score, na.rm = TRUE)
      
      sentiment_threshold_strong <- current_mean_sentiment - 2 * current_sd_sentiment
      sentiment_threshold_moderate <- current_mean_sentiment - 1 * current_sd_sentiment
      sentiment_threshold_mild <- current_mean_sentiment
      
      reclassified_reviews <- data_for_reclassification %>%
        rowwise() %>%
        mutate(
          has_anger_re = Anger_orig > 0,
          has_sadness_re = Sadness_orig > 0,
          has_surprise_re = Surprise_orig > 0,
          has_fear_re = Fear_orig > 0,
          has_disgust_re = Disgust_orig > 0
        ) %>%
        mutate(
          Final_Hate_Level = if_else(
            Hate_Level == "Unclassified",
            case_when(
              (has_anger_re & has_sadness_re) ~ "Strong (Reclassified by Emotion)",
              (has_disgust_re & has_sadness_re) ~ "Moderate (Reclassified by Emotion)",
              (has_surprise_re & has_fear_re) ~ "Mild (Reclassified by Emotion)",
              
              Sentiment_Score <= sentiment_threshold_strong ~ "Strong (Reclassified by Score)",
              (Sentiment_Score > sentiment_threshold_strong & Sentiment_Score <= sentiment_threshold_moderate) ~ "Moderate (Reclassified by Score)",
              (Sentiment_Score > sentiment_threshold_moderate & Sentiment_Score <= sentiment_threshold_mild) ~ "Mild (Reclassified by Score)",
              
              TRUE ~ "Hybrid Hate"
            ),
            Hate_Level
          )
        ) %>%
        ungroup() %>%
        select(
          review_id,
          reviewer,
          Final_Hate_Level,
          Words_Anger,
          Words_Sadness,
          Words_Surprise,
          Words_Fear,
          Words_Disgust,
          Reasons,
          Original_Review,
          Sentiment_Score,
          `Initial_Hate_Level (Step 7)` = Hate_Level
        ) %>%
        arrange(review_id)
      
      final_classification_data(reclassified_reviews)
      
      if (!is.null(reclassified_reviews) && nrow(reclassified_reviews) > 0) {
        final_classification_status_text(paste0("Final classification complete for ", nrow(reclassified_reviews), " reviews. Ready for summary plots."))
      } else {
        final_classification_status_text("No reviews classified in final step.")
      }
    })
  })
  
  output$final_classification_table <- renderDT({
    dat <- final_classification_data()
    if (!is.null(dat) && nrow(dat) > 0) {
      datatable(dat,
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        formatRound('Sentiment_Score', 3)
    } else {
      data.frame(Message = "No final classification data available yet. Click 'Perform Final Classification' in Step 8.")
    }
  })
  
  # Download handler for final_classification_table
  output$download_final_classification_table <- downloadHandler(
    filename = function() {
      paste("final_classification_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(final_classification_data(), file, row.names = FALSE)
    }
  )
  
  # --- Step 9: Summary Plots ---
  
  # Reactive values to hold plot objects for downloading
  hate_level_dist_plot_obj <- reactiveVal(NULL)
  hate_from_negative_plot_obj <- reactiveVal(NULL)
  negative_from_all_plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$generate_summary_plots_btn, {
    if (is.null(final_classification_data()) || nrow(final_classification_data()) == 0 ||
        is.null(negative_reviews_data()) || nrow(negative_reviews_data()) == 0 ||
        is.null(reviews_data()) || nrow(reviews_data()) == 0) {
      hate_level_dist_plot_obj(NULL)
      hate_from_negative_plot_obj(NULL)
      negative_from_all_plot_obj(NULL)
      return()
    }
    
    withProgress(message = "Generating summary plots...", value = 0, {
      # Plot 1: Distribution of Final Brand Hate Levels (Combined Categories)
      hate_level_counts_combined <- final_classification_data() %>%
        mutate(
          Plot_Hate_Level = case_when(
            str_detect(Final_Hate_Level, "Strong") ~ "Strong",
            str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
            str_detect(Final_Hate_Level, "Mild") ~ "Mild",
            TRUE ~ "Hybrid"
          )
        ) %>%
        count(Plot_Hate_Level, name = "Count") %>%
        mutate(Percentage = Count / sum(Count))
      
      hate_level_order <- c("Strong", "Moderate", "Mild", "Hybrid")
      hate_level_colors <- c("Strong" = "#DC3912", "Moderate" = "#FF9900", "Mild" = "#3366CC", "Hybrid" = "#999999")
      
      plot_hate_level_distribution <- ggplot(hate_level_counts_combined, aes(x = factor(Plot_Hate_Level, levels = hate_level_order), y = Count, fill = Plot_Hate_Level)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(Count, " (", percent(Percentage, accuracy = 0.1), ")")), vjust = -0.5, size = 4) +
        labs(title = "Distribution of Final Brand Hate Levels",
             x = "Brand Hate Level",
             y = "Number of Reviews") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12)) +
        scale_fill_manual(values = hate_level_colors)
      hate_level_dist_plot_obj(plot_hate_level_distribution)
      
      output$hate_level_distribution_plot <- renderPlot({
        plot_hate_level_distribution
      })
      
      # Download handlers for hate_level_distribution_plot (PNG)
      output$download_hate_level_distribution_plot <- downloadHandler(
        filename = function() { paste("hate_level_distribution_", Sys.Date(), ".png", sep = "") },
        content = function(file) { ggsave(file, plot = hate_level_dist_plot_obj(), device = "png", width = 10, height = 6, dpi = 300) }
      )
      
      # Download handlers for hate_level_distribution_plot (TIFF)
      output$download_hate_level_distribution_plot_tiff <- downloadHandler(
        filename = function() { paste("hate_level_distribution_", Sys.Date(), ".tiff", sep = "") },
        content = function(file) { ggsave(file, plot = hate_level_dist_plot_obj(), device = "tiff", width = 10, height = 6, dpi = 600) }
      )
      
      # Plot 2: Brand Hate Reviews out of All Negative Reviews (Pie Chart)
      total_negative_reviews <- nrow(negative_reviews_data())
      total_classified_hate_reviews <- final_classification_data() %>%
        filter(Final_Hate_Level != "Hybrid Hate") %>%
        nrow()
      
      remaining_negative_reviews <- total_negative_reviews - total_classified_hate_reviews
      
      hate_from_negative_data_pie <- tibble(
        Category = c("Classified Hate Reviews", "Other Negative Reviews"),
        Count = c(total_classified_hate_reviews, remaining_negative_reviews)
      ) %>%
        mutate(Percentage = Count / sum(Count))
      
      plot_hate_from_negative <- if (sum(hate_from_negative_data_pie$Count) == 0) {
        ggplot() + annotate("text", x=0.5, y=0.5, label="No negative reviews to classify.", size=5, color="gray") + theme_void()
      } else {
        ggplot(hate_from_negative_data_pie, aes(x = "", y = Count, fill = Category)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = 0) +
          geom_text(aes(label = percent(Percentage, accuracy = 0.1)), position = position_stack(vjust = 0.5), color = "black", size = 4) +
          labs(title = "Brand Hate Reviews out of All Negative Reviews", x = NULL, y = NULL, fill = "Category") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                legend.position = "bottom") +
          scale_fill_manual(values = c("Classified Hate Reviews" = "#FF8C00", "Other Negative Reviews" = "#A9D9D9"))
      }
      hate_from_negative_plot_obj(plot_hate_from_negative)
      
      output$hate_from_negative_plot <- renderPlot({
        plot_hate_from_negative
      })
      
      # Download handlers for hate_from_negative_plot (PNG)
      output$download_hate_from_negative_plot <- downloadHandler(
        filename = function() { paste("hate_from_negative_plot_", Sys.Date(), ".png", sep = "") },
        content = function(file) { ggsave(file, plot = hate_from_negative_plot_obj(), device = "png", width = 10, height = 7) }
      )
      
      # Download handlers for hate_from_negative_plot (TIFF)
      output$download_hate_from_negative_plot_tiff <- downloadHandler(
        filename = function() { paste("hate_from_negative_plot_", Sys.Date(), ".tiff", sep = "") },
        content = function(file) { ggsave(file, plot = hate_from_negative_plot_obj(), device = "tiff", width = 10, height = 7, dpi = 600) }
      )
      
      # Plot 3: Negative Reviews out of All Scraped Reviews (Pie Chart)
      total_scraped_reviews <- nrow(reviews_data())
      
      remaining_all_reviews <- total_scraped_reviews - total_negative_reviews
      
      negative_from_all_data_pie <- tibble(
        Category = c("Total Negative Reviews", "Other Scraped Reviews"),
        Count = c(total_negative_reviews, remaining_all_reviews)
      ) %>%
        mutate(Percentage = Count / sum(Count))
      
      plot_negative_from_all <- if (sum(negative_from_all_data_pie$Count) == 0) {
        ggplot() + annotate("text", x=0.5, y=0.5, label="No scraped reviews found.", size=5, color="gray") + theme_void()
      } else {
        ggplot(negative_from_all_data_pie, aes(x = "", y = Count, fill = Category)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = 0) +
          geom_text(aes(label = percent(Percentage, accuracy = 0.1)), position = position_stack(vjust = 0.5), color = "black", size = 4) +
          labs(title = "Negative Reviews out of All Scraped Reviews", x = NULL, y = NULL, fill = "Category") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                legend.position = "bottom") +
          scale_fill_manual(values = c("Total Negative Reviews" = "#FF4500", "Other Scraped Reviews" = "#B0E0E6"))
      }
      negative_from_all_plot_obj(plot_negative_from_all)
      
      output$negative_from_all_plot <- renderPlot({
        plot_negative_from_all
      })
      
      # Download handlers for negative_from_all_plot (PNG)
      output$download_negative_from_all_plot <- downloadHandler(
        filename = function() { paste("negative_from_all_plot_", Sys.Date(), ".png", sep = "") },
        content = function(file) { ggsave(file, plot = negative_from_all_plot_obj(), device = "png", width = 10, height = 7) }
      )
      
      # Download handlers for negative_from_all_plot (TIFF)
      output$download_negative_from_all_plot_tiff <- downloadHandler(
        filename = function() { paste("negative_from_all_plot_", Sys.Date(), ".tiff", sep = "") },
        content = function(file) { ggsave(file, plot = negative_from_all_plot_obj(), device = "tiff", width = 10, height = 7, dpi = 600) }
      )
      incProgress(1, detail = "Summary plots generated!")
    })
  })
  
  # --- Step 10: Reasons per Brand Hate Type Plots ---
  
  # Reactive values for reasons plots
  reasons_strong_plot_obj <- reactiveVal(NULL)
  reasons_moderate_plot_obj <- reactiveVal(NULL)
  reasons_mild_plot_obj <- reactiveVal(NULL)
  reasons_unclassified_plot_obj <- reactiveVal(NULL) # This will now be Hybrid
  
  observeEvent(input$generate_reasons_plots_btn, {
    if (is.null(final_classification_data()) || nrow(final_classification_data()) == 0) {
      reasons_strong_plot_obj(NULL)
      reasons_moderate_plot_obj(NULL)
      reasons_mild_plot_obj(NULL)
      reasons_unclassified_plot_obj(NULL)
      return()
    }
    
    withProgress(message = "Generating reasons plots...", value = 0, {
      reasons_data_processed <- final_classification_data() %>%
        # Ensure Plot_Hate_Level is available (re-derive as done in Step 9 plots for consistency)
        mutate(
          Plot_Hate_Level = case_when(
            str_detect(Final_Hate_Level, "Strong") ~ "Strong",
            str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
            str_detect(Final_Hate_Level, "Mild") ~ "Mild",
            TRUE ~ "Hybrid" # Renamed Unclassified to Hybrid
          )
        ) %>%
        filter(!is.na(Reasons) & Reasons != "") %>% # Remove reviews without specified reasons
        separate_rows(Reasons, sep = ",\\s*") %>% # Split comma-separated reasons into separate rows
        mutate(Reasons = trimws(Reasons)) # Trim whitespace
      
      # Get unique hate levels to loop through for plotting
      unique_hate_levels <- unique(reasons_data_processed$Plot_Hate_Level)
      
      # Define custom plotting function to avoid repetition
      plot_reasons <- function(data_subset, level_name, top_n = 10) {
        if(nrow(data_subset) == 0) {
          return(ggplot() + annotate("text", x=0.5, y=0.5, label=paste0("No data for ", level_name, " Hate."), size=5, color="gray") + theme_void())
        }
        
        reason_counts <- data_subset %>%
          count(Reasons, sort = TRUE, name = "Count") %>%
          head(top_n) # Get top N reasons
        
        ggplot(reason_counts, aes(x = reorder(Reasons, Count), y = Count, fill = Reasons)) +
          geom_col(show.legend = FALSE) +
          geom_text(aes(label = Count), hjust = -0.1, size = 4) +
          labs(title = paste0("Top Reasons for ", level_name, " Hate"),
               x = "Reason",
               y = "Number of Times Mentioned") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10, face = "bold")) +
          coord_flip() # Flip coordinates for horizontal bars
      }
      
      # Generate plots for each hate level and store them
      plot_strong <- plot_reasons(filter(reasons_data_processed, Plot_Hate_Level == "Strong"), "Strong")
      reasons_strong_plot_obj(plot_strong)
      output$reasons_strong_plot <- renderPlot({ plot_strong })
      
      plot_moderate <- plot_reasons(filter(reasons_data_processed, Plot_Hate_Level == "Moderate"), "Moderate")
      reasons_moderate_plot_obj(plot_moderate)
      output$reasons_moderate_plot <- renderPlot({ plot_moderate })
      
      plot_mild <- plot_reasons(filter(reasons_data_processed, Plot_Hate_Level == "Mild"), "Mild")
      reasons_mild_plot_obj(plot_mild)
      output$reasons_mild_plot <- renderPlot({ plot_mild })
      
      plot_unclassified <- plot_reasons(filter(reasons_data_processed, Plot_Hate_Level == "Hybrid"), "Hybrid") # Updated here to Hybrid
      reasons_unclassified_plot_obj(plot_unclassified)
      output$reasons_unclassified_plot <- renderPlot({ plot_unclassified })
      incProgress(1, detail = "Reasons plots generated!")
    })
  })
  
  # Download handlers for Reasons Plots (PNG and TIFF)
  output$download_reasons_strong_plot <- downloadHandler(filename = function() { paste0("reasons_strong_plot_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = reasons_strong_plot_obj(), device = "png", width = 8, height = 6, dpi = 300) })
  output$download_reasons_strong_plot_tiff <- downloadHandler(filename = function() { paste0("reasons_strong_plot_", Sys.Date(), ".tiff") }, content = function(file) { ggsave(file, plot = reasons_strong_plot_obj(), device = "tiff", width = 8, height = 6, dpi = 600) })
  
  output$download_reasons_moderate_plot <- downloadHandler(filename = function() { paste0("reasons_moderate_plot_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = reasons_moderate_plot_obj(), device = "png", width = 8, height = 6, dpi = 300) })
  output$download_reasons_moderate_plot_tiff <- downloadHandler(filename = function() { paste0("reasons_moderate_plot_", Sys.Date(), ".tiff") }, content = function(file) { ggsave(file, plot = reasons_moderate_plot_obj(), device = "tiff", width = 8, height = 6, dpi = 600) })
  
  output$download_reasons_mild_plot <- downloadHandler(filename = function() { paste0("reasons_mild_plot_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = reasons_mild_plot_obj(), device = "png", width = 8, height = 6, dpi = 300) })
  output$download_reasons_mild_plot_tiff <- downloadHandler(filename = function() { paste0("reasons_mild_plot_", Sys.Date(), ".tiff") }, content = function(file) { ggsave(file, plot = reasons_mild_plot_obj(), device = "tiff", width = 8, height = 6, dpi = 600) })
  
  output$download_reasons_hybrid_plot <- downloadHandler(filename = function() { paste0("reasons_hybrid_plot_", Sys.Date(), ".png") }, content = function(file) { ggsave(file, plot = reasons_unclassified_plot_obj(), device = "png", width = 8, height = 6, dpi = 300) }) # Renamed for Hybrid
  output$download_reasons_hybrid_plot_tiff <- downloadHandler(filename = function() { paste0("reasons_hybrid_plot_", Sys.Date(), ".tiff") }, content = function(file) { ggsave(file, plot = reasons_unclassified_plot_obj(), device = "tiff", width = 8, height = 6, dpi = 600) }) # Renamed for Hybrid
  
  
  # --- Step 11: Emotion Word Clouds ---
  
  # Reactive values for word cloud plots
  wordcloud_strong_obj <- reactiveVal(NULL)
  wordcloud_moderate_obj <- reactiveVal(NULL)
  wordcloud_mild_obj <- reactiveVal(NULL)
  wordcloud_unclassified_obj <- reactiveVal(NULL) # This will now be Hybrid
  
  observeEvent(input$generate_wordclouds_btn, {
    if (is.null(final_classification_data()) || nrow(final_classification_data()) == 0) {
      # Clear plot objects
      wordcloud_strong_obj(NULL)
      wordcloud_moderate_obj(NULL)
      wordcloud_mild_obj(NULL)
      wordcloud_unclassified_obj(NULL)
      return()
    }
    
    withProgress(message = "Generating word clouds...", value = 0, {
      wordcloud_data_processed <- final_classification_data() %>%
        mutate(
          Plot_Hate_Level = case_when(
            str_detect(Final_Hate_Level, "Strong") ~ "Strong",
            str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
            str_detect(Final_Hate_Level, "Mild") ~ "Mild",
            TRUE ~ "Hybrid" # Renamed Unclassified to Hybrid
          )
        ) %>%
        # Combine all emotion word columns into a single column
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        filter(Combined_Emotion_Words != "") %>% # Remove rows with no emotion words
        # Now, unnest into individual words
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>% # Corrected: Reference Combined_Emotion_Words
        filter(word != "") # Remove any empty strings from splitting
      
      
      # Define custom word cloud plotting function
      # This function will now return a temporary file path for renderImage,
      # and the download handlers will re-generate the clouds.
      plot_wordcloud_to_file <- function(data_subset, level_name, file_type = "png", dpi = 100) {
        word_counts <- data_subset %>%
          count(word, sort = TRUE, name = "Freq")
        
        if(nrow(word_counts) == 0 || all(word_counts$Freq == 0)) { # Added check for all freqs being 0
          # For a lack of data, return a placeholder image file
          temp_message_file <- tempfile(fileext = ".png")
          png(temp_message_file, width = 800, height = 600, res = 100)
          plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
          text(0.5, 0.5, paste0("No emotion words found for ", level_name, " Hate."), cex=1.2, col="gray")
          dev.off()
          return(temp_message_file)
        }
        
        if (nrow(word_counts) > 200) {
          word_counts <- head(word_counts, 200)
        }
        
        temp_file <- tempfile(fileext = paste0(".", file_type))
        if (file_type == "png") {
          png(temp_file, width = 800, height = 600, res = dpi)
        } else if (file_type == "tiff") {
          tiff(temp_file, width = 800, height = 600, units = "px", res = dpi, compression = "lzw")
        }
        
        wordcloud(words = word_counts$word,
                  freq = word_counts$Freq,
                  min.freq = 1,
                  max.words = 100,
                  random.order = FALSE,
                  colors = brewer.pal(8,"Dark2"))
        dev.off()
        return(temp_file)
      }
      
      # Generate word clouds for each hate level and store the PNG paths for rendering
      # Set height explicitly in UI's imageOutput for better control
      wordcloud_strong_obj(plot_wordcloud_to_file(filter(wordcloud_data_processed, Plot_Hate_Level == "Strong"), "Strong", "png", 100))
      wordcloud_moderate_obj(plot_wordcloud_to_file(filter(wordcloud_data_processed, Plot_Hate_Level == "Moderate"), "Moderate", "png", 100))
      wordcloud_mild_obj(plot_wordcloud_to_file(filter(wordcloud_data_processed, Plot_Hate_Level == "Mild"), "Mild", "png", 100))
      wordcloud_unclassified_obj(plot_wordcloud_to_file(filter(wordcloud_data_processed, Plot_Hate_Level == "Hybrid"), "Hybrid", "png", 100)) # Updated here to Hybrid
      incProgress(1, detail = "Word clouds generated! You can now proceed to the Bubble Plot.")
    })
  })
  
  # Render Image outputs for Word Clouds
  # These are now `imageOutput` in the UI and rendered with `renderImage`
  output$wordcloud_strong <- renderImage({
    file_path <- wordcloud_strong_obj() # This is the path to the temp PNG
    list(src = file_path, contentType = "image/png", width = "100%", height = 400) # Ensure height matches output area
  }, deleteFile = TRUE) # Delete the temp file after serving
  
  output$wordcloud_moderate <- renderImage({
    file_path <- wordcloud_moderate_obj()
    list(src = file_path, contentType = "image/png", width = "100%", height = 400)
  }, deleteFile = TRUE)
  
  output$wordcloud_mild <- renderImage({
    file_path <- wordcloud_mild_obj()
    list(src = file_path, contentType = "image/png", width = "100%", height = 400)
  }, deleteFile = TRUE)
  
  output$wordcloud_unclassified <- renderImage({ # Renamed from `plotOutput` to `imageOutput` in UI
    file_path <- wordcloud_unclassified_obj()
    list(src = file_path, contentType = "image/png", width = "100%", height = 400)
  }, deleteFile = TRUE)
  
  # Download Handlers for Word Clouds (PNG) - these re-generate the cloud at higher res
  output$download_wordcloud_strong <- downloadHandler(
    filename = function() { paste0("wordcloud_strong_", Sys.Date(), ".png") },
    content = function(file) {
      # ENSURE DATA IS AVAILABLE FOR DOWNLOAD
      req(final_classification_data())
      
      # Re-prepare the data needed for the wordcloud (same logic as in generate_wordclouds_btn)
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Strong") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      # Handle case where no words are found for this category (to prevent error)
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        # Create a placeholder empty plot
        png(file, width = 800, height = 600, res = 300)
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Strong Hate to download."), cex=1.2, col="gray")
        dev.off()
        return() # Exit content function
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      png(file, width = 800, height = 600, res = 300) # Increased resolution for download
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  output$download_wordcloud_moderate <- downloadHandler(
    filename = function() { paste0("wordcloud_moderate_", Sys.Date(), ".png") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Moderate") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        png(file, width = 800, height = 600, res = 300)
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Moderate Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      png(file, width = 800, height = 600, res = 300)
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  output$download_wordcloud_mild <- downloadHandler(
    filename = function() { paste0("wordcloud_mild_", Sys.Date(), ".png") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Mild") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        png(file, width = 800, height = 600, res = 300)
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Mild Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      png(file, width = 800, height = 600, res = 300)
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  output$download_wordcloud_hybrid <- downloadHandler(
    filename = function() { paste0("wordcloud_hybrid_", Sys.Date(), ".png") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Hybrid") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        png(file, width = 800, height = 600, res = 300)
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Hybrid Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      png(file, width = 800, height = 600, res = 300)
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  # Download Handlers for Word Clouds (TIFF) - these re-generate the cloud at higher res
  output$download_wordcloud_strong_tiff <- downloadHandler(
    filename = function() { paste0("wordcloud_strong_", Sys.Date(), ".tiff") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Strong") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Strong Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  output$download_wordcloud_moderate_tiff <- downloadHandler(
    filename = function() { paste0("wordcloud_moderate_", Sys.Date(), ".tiff") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Moderate") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Moderate Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  output$download_wordcloud_mild_tiff <- downloadHandler(
    filename = function() { paste0("wordcloud_mild_", Sys.Date(), ".tiff") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Mild") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Mild Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  output$download_wordcloud_hybrid_tiff <- downloadHandler(
    filename = function() { paste0("wordcloud_hybrid_", Sys.Date(), ".tiff") },
    content = function(file) {
      req(final_classification_data())
      data_subset_for_download <- final_classification_data() %>%
        mutate(Plot_Hate_Level = case_when(
          str_detect(Final_Hate_Level, "Strong") ~ "Strong",
          str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
          str_detect(Final_Hate_Level, "Mild") ~ "Mild",
          TRUE ~ "Hybrid"
        )) %>%
        filter(Plot_Hate_Level == "Hybrid") %>%
        unite("Combined_Emotion_Words", c(Words_Anger, Words_Sadness, Words_Surprise, Words_Fear, Words_Disgust), sep = ", ", na.rm = TRUE) %>%
        separate_rows(Combined_Emotion_Words, sep = ",\\s*") %>%
        mutate(word = trimws(Combined_Emotion_Words)) %>%
        filter(word != "") %>%
        count(word, sort = TRUE, name = "Freq")
      
      if (nrow(data_subset_for_download) == 0 || all(data_subset_for_download$Freq == 0)) {
        tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='n', xlab='', ylab='')
        text(0.5, 0.5, paste0("No emotion words found for Hybrid Hate to download."), cex=1.2, col="gray")
        dev.off()
        return()
      }
      
      if (nrow(data_subset_for_download) > 200) { data_subset_for_download <- head(data_subset_for_download, 200) }
      tiff(file, width = 800, height = 600, units = "px", res = 600, compression = "lzw")
      wordcloud(words = data_subset_for_download$word, freq = data_subset_for_download$Freq, min.freq = 1, max.words = 100, random.order = FALSE, colors = brewer.pal(8,"Dark2"))
      dev.off()
    }
  )
  
  # --- Step 12: Bubble Plot ---
  
  # Reactive value for the bubble plot object
  bubble_plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$generate_bubble_plot_btn, {
    if (is.null(final_classification_data()) || nrow(final_classification_data()) == 0) {
      bubble_plot_obj(NULL)
      return()
    }
    
    withProgress(message = "Generating bubble plot...", value = 0, {
      plot_data <- final_classification_data() %>%
        mutate(
          Plot_Hate_Level = case_when(
            str_detect(Final_Hate_Level, "Strong") ~ "Strong",
            str_detect(Final_Hate_Level, "Moderate") ~ "Moderate",
            str_detect(Final_Hate_Level, "Mild") ~ "Mild",
            TRUE ~ "Hybrid"
          )
        )
      
      hate_level_colors <- c("Strong" = "#DC3912", "Moderate" = "#FF9900", "Mild" = "#3366CC", "Hybrid" = "#999999")
      hate_level_order <- c("Strong", "Moderate", "Mild", "Hybrid")
      
      plot <- ggplot(plot_data, aes(x = factor(Plot_Hate_Level, levels = hate_level_order), y = Sentiment_Score, color = Plot_Hate_Level)) +
        geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.7) +
        labs(title = "Hate Intensity: Sentiment Score by Classified Type",
             x = "Brand Hate Level",
             y = "Sentiment Score (from sentimentr)",
             color = "Hate Level") +
        scale_color_manual(values = hate_level_colors) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12),
              legend.position = "bottom")
      
      bubble_plot_obj(plot)
      incProgress(1, detail = "Bubble plot generated!")
    })
  })
  
  output$hate_bubble_plot <- renderPlot({
    plot_obj <- bubble_plot_obj()
    if (is.null(plot_obj)) {
      ggplot() + annotate("text", x=0.5, y=0.5, label="Click 'Generate Bubble Plot' to see the plot.", size=5, color="gray") + theme_void()
    } else {
      plot_obj
    }
  })
  
  output$download_bubble_plot <- downloadHandler(
    filename = function() {
      paste0("hate_bubble_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = bubble_plot_obj(), device = "png", width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_bubble_plot_tiff <- downloadHandler(
    filename = function() {
      paste0("hate_bubble_plot_", Sys.Date(), ".tiff")
    },
    content = function(file) {
      ggsave(file, plot = bubble_plot_obj(), device = "tiff", width = 10, height = 7, dpi = 600)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)