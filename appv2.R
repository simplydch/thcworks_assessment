library(shiny)
library(bslib)

library(shiny)

### Required for functional shinylive
if (FALSE) {
  library("munsell")
}

library(RColorBrewer)
library(bslib)
library(ggplot2)

# TODO - Fix Use of custom font for ggplot - not currently working
# library(extrafont)
#
# font_import(paths = "docs/fonts", pattern = "Sans", prompt = FALSE)
# loadfonts()
# print(fonts())


# data_file <- "https://raw.githubusercontent.com/thcworks/dashboard_assessment/refs/heads/main/data/honeycomb_test_data.csv"
data_file <- "./data/honeycomb_test_data.csv"


# Set point symbols
# TODO find fix to unicode symbols not working in shinylive

# small_plot_point <- "\u2B23"  # Set points as hexagon
# point_size_small <- 3


## Set up common values so they can be easily altered
small_plot_point <- 16
point_size_small <- 2
point_size_large <- point_size_small * 1.5
graph_font_size <- 14
graph_font <- "sans"

## Load Data
test_data_csv <- read.csv(data_file)


# Get current date - set to 01/01/2022 for demonstration purposes
# cur_date <- Sys.Date()
cur_date <- as.Date("2022-01-01")


### Set up dataframe for recoding relationship -
### after converting to lowercase and removing whitespace

relationship_recode_info <- data.frame(
  original = c(
    "colleagueinanotherteam",
    "teammate",
    "colleaguefromadifferentteam",
    "colleaguewithinmyteam",
    "iamtheirlinemanager",
    "theyaremylinemanager",
    "self"
  ),
  replacement = c(
    "Peer",
    "Peer",
    "Peer",
    "Peer",
    "Manager",
    "Staff",
    "Self"
  )
)

## Vectors of characteristics column names for simplicity later

characteristic_names <- c(
  "u.sexuality",
  "u.ethnicity",
  "u.gender_identity",
  "u.age",
  "r.sexuality",
  "r.ethnicity",
  "r.gender_identity",
  "r.age"
)

user_characteristic_names <- characteristic_names[1:4]

### Verify data is in expected format
### This should be extended to all required columns

verify_data <- function(df) {
  # TODO Check all used columns are in this list
  # Check that required columns are all present
  if (any(!c("value", "u.population_id", "relationship", "user_id") %in% names(df))) {
    stop("Column names are not as expected - check data file")
  }
  
  # Check review sources
  
  df[which(df[, "self_review"]), "relationship"] <- "self"
  
  df[, "relationship"] <- gsub("\\s+", "", tolower(df[, "relationship"]))
  
  df[, "relationship"] <- relationship_recode_info[
    match(
      df[, "relationship"],
      relationship_recode_info[, "original"]
    ),
    "replacement"
  ]
  
  df[!df[, "relationship"] %in% c("Peer", "Manager", "Staff", "Self"), ] <- "Unknown"
  
  if (any(df[, "relationship"] == "Unknown")) {
    message("Not all review sources could be processed - marked as Unknown")
  }
  
  # Check for duplicate entries
  
  int_entries <- nrow(df)
  df <- df[!duplicated(df), ]
  # 
  # if (int_entries > nrow(df)) {
  #   message("Duplicate entries found and removed")
  # }
  # 
  # Convert Date to correct format
  # NOTE - this assumes that time can be dismissed
  df[, "completed_at"] <- as.Date(df[, "completed_at"], format = "%Y-%m-%d %H:%M:%S")
  
  
  # Replace Missing or NA character values with 'Unknown'
  
  char_subset <- df[, characteristic_names]
  
  char_subset[is.na(char_subset) | char_subset == "" | char_subset == "Prefer not to say"] <- "Unknown"
  
  df[, characteristic_names] <- unname(char_subset)
  
  
  # Convert values to numeric and flag if any values are in unexpected format
  null_values_idx <- which(df[, "value"] == "null")
  df[null_values_idx, "value"] <- NA
  
  withCallingHandlers(
    {
      df[, "value"] <- as.numeric(df[, "value"])
    },
    warning = function(w) {
      if (grepl("NAs introduced by coercion", w$message)) {
        message("Values that were not numeric or 'null' were unexpected and converted to NA")
        invokeRestart("muffleWarning")
      } else {
        message(w$message)
      }
    }
  )
  
  
  return(df)
}


## Run verified function

test_data_csv_verified <- verify_data(df = test_data_csv)

rm(test_data_csv)


test_data_csv_verified$behaviour_title_short <- paste0(substr(test_data_csv_verified$behaviour_title, 
                                                              1, 27), "...(",test_data_csv_verified$behaviour_id, ")")

behaviour_key <- unique(test_data_csv_verified[, c("behaviour_id", 
                                                   "behaviour_title_short", 
                                                   "behaviour_title")])


### Average values for multiple values of the same behaviour score in same review

av_value_behave_review <- aggregate(value ~ review_id + behaviour_id,  
                                    test_data_csv_verified, 
                                    mean, 
                                    na.rm=TRUE)

#  And add other columns back

test_data_csv_verified <- data.frame(
  av_value_behave_review,
  test_data_csv_verified[
    match(
      paste0(av_value_behave_review$review_id, av_value_behave_review$behaviour_id, sep="_"),
      paste0(test_data_csv_verified$review_id, test_data_csv_verified$behaviour_id, sep="_")
    ),
    !names(test_data_csv_verified) %in% c(
      "behaviour_id",
      "value",
      "review_id"
    )
  ]
)


all_users <- unique(test_data_csv_verified[, c("user_id", "u.population_id")])

## Calculate the average score per review ID

average_score_per_review <- aggregate(value ~ review_id, test_data_csv_verified, mean)

### Add back information about review session
### NOTE: This assumes that the same reviewer always does the full review

average_score_per_review <- data.frame(
  average_score_per_review,
  test_data_csv_verified[
    match(
      average_score_per_review$review_id,
      test_data_csv_verified$review_id
    ),
    !names(test_data_csv_verified) %in% c(
      "behaviour_title",
      "behaviour_id",
      "value",
      "review_id"
    )
  ]
)


## Create user info dataframe

user_info <- unique(average_score_per_review[, c("user_id")])
user_info <- data.frame(
  user_info,
  average_score_per_review[
    match(
      user_info,
      average_score_per_review$user_id
    ),
    user_characteristic_names
  ]
)

## Order based on date

average_score_per_review <- average_score_per_review[order(average_score_per_review$completed_at,
                                                           decreasing = TRUE
), ]


latest_average_scores <- average_score_per_review[!duplicated(average_score_per_review$user_id), ]




## Get ordered list of current populations

current_population_ids <- sort(unique(average_score_per_review[, "u.population_id"]))

num_ind_pops <- length(current_population_ids)

# Select colours from colour-blind friendly palette
# NOTE: This might need adjusted if number of population is over 12
# minimum number of colours is three so [1:num_ind_pops] ensures correct number
# if less than 3

col_pal <- brewer.pal(n = num_ind_pops, name = "Paired")[1:num_ind_pops]


#### Create html code so that populations are coloured in drop down loist

current_population_ids <- setNames(
  current_population_ids,
  paste0(
    "<span style='color:", col_pal,
    "';>",
    current_population_ids, "</span>"
  )
)

# TODO - This doesn't necessarily scale well as users increase, might require more thought or limiting
# selected number of populations!


### Function below creates dummy data for stacked dotplot
### This is necessary to allow the reactive plot

# assign value groups
create_stacked_dot_plot_data <- function(data){
  value_groups <- cut(
    data$value,
    seq(0, 10, 0.25),
    seq(0, 9.75, 0.25)
  )
  
  # extract actual value for each group (not factor)
  data[, "value_group"] <- as.numeric(as.character(value_groups))
  
  
  # blank data frame for when a group has no data
  blank_df <- data.frame(matrix(ncol = ncol(data) + 1, nrow = 0))
  colnames(blank_df) <- c(names(data), "plot_height")
  
  
  # split into groups, order and assign cumulative value
  data_ls <- lapply(split(data, value_groups), function(x) {
    x <- x[order(x[["u.population_id"]], x[["value"]], decreasing = TRUE), ]
    if (nrow(x) > 0) {
      x["plot_height"] <- seq(1, nrow(x), 1)
      return(x)
    } else {
      return(blank_df)
    }
  })
  
  return(do.call(rbind, data_ls))
}


### Compare Users Initial and Latest Review Values

average_score_per_review_no_self <- average_score_per_review[average_score_per_review$relationship != "Self", ]

user_list <- split(average_score_per_review_no_self, average_score_per_review_no_self$user_id)

first_last_list <- lapply(user_list, function(x) {
  x <- x[order(x[, "completed_at"]), c("user_id", "completed_at", "value")]
  list(x[1, ], x[nrow(x), ])
})

first_review <- do.call(rbind, lapply(first_last_list, "[[", 1))
second_review <- do.call(rbind, lapply(first_last_list, "[[", 2))

first_last_df <- data.frame(first_review, second_review[, c("completed_at", "value")])

names(first_last_df) <- c(
  "user_id",
  "inital_review_date",
  "initial_review_value",
  "last_review_date",
  "last_review_value"
)


# Keep only users with multiple dates
first_last_df <- first_last_df[first_last_df[, "inital_review_date"] != first_last_df[, "last_review_date"], ]
first_last_df[, "score_diff"] <- first_last_df$last_review_value - first_last_df$initial_review_value


### Static values for data overview

prop_reduced_review_score <- round(prop.table(table(first_last_df$score_diff < 0))[2] * 100, 1)

num_users <- nrow(all_users)
num_self_reviews <- sum(average_score_per_review[, "relationship"] == "Self")
num_reviews <- nrow(average_score_per_review)

# Select only users for which the full set of characteristics is available
characteristics_known <- rowSums(user_info[, user_characteristic_names] == "Unknown") == 0

user_info_char_known <- user_info[characteristics_known, user_characteristic_names]

count_character_sets <- as.data.frame(table(user_info_char_known))
count_character_sets <- count_character_sets[count_character_sets$Freq > 0, ]
count_character_sets <- count_character_sets[order(count_character_sets$Freq,
                                                   decreasing = TRUE
), ]

count_character_sets[, "idx"] <- 1:nrow(count_character_sets)

num_char_set <- nrow(count_character_sets)

num_unique_char_set <- sum(count_character_sets[, "Freq"] == 1)

# Set up initial values



# Default is that self reviews are not shown
exclude_self <- TRUE

sel_pop_id <- current_population_ids[[1]]

relevant_reviews <- average_score_per_review[average_score_per_review$u.population_id == sel_pop_id, ]


if (exclude_self) {
  relevant_reviews <- relevant_reviews[relevant_reviews$relationship != "Self", ]
  init_average_score_per_review <- average_score_per_review[average_score_per_review$relationship != "Self",]
} else {
  init_average_score_per_review <- average_score_per_review
  
}



init_ids <- sort(unique(relevant_reviews$user_id))

init_id <- init_ids[[1]]

id_reviews <- relevant_reviews[relevant_reviews$user_id == init_id, c("completed_at","review_id", "relationship")]
id_reviews <- id_reviews[order(id_reviews$completed_at, decreasing = TRUE), ]

review_types <- sort(unique(id_reviews[, "relationship"]))

review_type_sel <- id_reviews[1, "relationship"]

format_review_date <- function(df){
  paste0(df[, "review_id"], " (", df[, "completed_at"], ")")
}

extract_review_id <- function(entry){
  gsub("([0-9]+) .*", "\\1", entry)
}


review_dates <- format_review_date(id_reviews[id_reviews[,"relationship"]==review_type_sel,])

review_date_sel <- review_dates[1]



latest_average_scores_sel_id <- latest_average_scores[latest_average_scores$u.population_id  == sel_pop_id, ]


# Initialize a reactive values
selected_id <- reactiveVal(init_id)
full_data_set <- reactiveVal(test_data_csv_verified)
av_review_set <- reactiveVal(init_average_score_per_review)
cur_data_set <- reactiveVal(create_stacked_dot_plot_data(latest_average_scores_sel_id ))
selected_review <- reactiveVal(review_date_sel)
selected_type <- reactiveVal(review_type_sel)
time_series_click <- reactiveVal(FALSE)
ignore_update <- reactiveVal(FALSE)
# Define UI
ui <- page_sidebar(
  title = "The HoneyComb Works - Example",
  
  
  # TODO This should be moved to a separate file - but can't get it to link properly
  # When using shiny live
  ###### Custom CSS and theme setup #####
  tags$style(HTML("
    .card-title {
      font-size: 13px;
      font-weight: bold;
      margin-bottom: -10px;
    }
    .card-content {
      font-size: 13px; 
      margin-bottom: -10px;
    }
    .card-full-page {
      width: 100%; /* Set your desired width */
      max-height: 200px; /* Set height to auto or a specific value */
      min-height: 200px;
    }
    .card {
      margin-bottom: 0px;  /* Adjust this value to control space between cards */
      margin-top: 0px;
    }
    .tab-content {
      max-height: 200px; 
      overflow-y: auto;
          }
    .card-body {
      padding: 5px;  /* Adjust this value to control inner padding of the cards */
    }
    .navbar {
      height: 40px;  /* You can adjust this value to make the navbar smaller */
      padding: 0;    /* Remove extra padding */
    }
    
    /* Vertically center the title */
    .navbar-brand {
      display: flex;
      align-items: center;
      height: 100%;
    }
    .nav-tabs > li > a {
      padding: 5px 10px; /* Adjust padding for smaller tabs */
      font-size: 12px;   /* Adjust font size for smaller text */
    }
    body, title, .content-wrapper, .main-header, .sidebar, .navbar, h1, h2, h3, h4, h5, h6, p, div,
    .nav-link.active, .table, .table th, .table td { 
      font-family: 'Work Sans', sans-serif !important; 
      color: #000000 !important; 
    } 
    .table {
      min-height: 200px;
      width: 300px;  /* Set table width to 100% */
      }
      th:nth-child(1) {
        width: 100px;  
      }
      th:nth-child(2) {
        width: 100px;  
      }
      th:nth-child(3) {
        width: 100px;  
      }
  ")),
  ######
  
  theme = bs_theme(
    bslib_spacer = "10px",
    preset = "flatly",
    fg = "rgb(255,180, 0)",
    bg = "rgb(255,255,255)",
    primary = "rgb(255, 190, 0)",
    success = "rgb(137, 145, 151)"
  ),
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Work+Sans:wght@400&display=swap"
  ),
  sidebar = sidebar(
    checkboxInput(
      inputId = "exclude_self",
      label = "Exclude Self Reviews",
      value = TRUE
    ),
    
    # Set up Population Selection
    selectizeInput(
      inputId = "pop_sel",
      label = "Population:",
      choices = current_population_ids,
      selected = sel_pop_id,
      multiple = TRUE,
      options = list(render = I("
  {
    item: function(item, escape) { return '<div>' + item.label + '</div>'; },
    option: function(item, escape) { return '<div>' + item.label + '</div>'; }
  }"))
    ),
    # Set up ID Selection
    selectizeInput(
      inputId = "id_sel",
      label = "User (Type to search):",
      choices = init_ids,
      selected = init_id,
      multiple = FALSE
    ),
    # Set up Review Type Selection
    selectInput(
      inputId = "review_type_sel",
      label = "Review Type:",
      choices = review_types,
      selected = review_type_sel,
      multiple = FALSE
    ),
    # Set up Review Date Selection
    selectInput(
      inputId = "review_date_sel",
      label = "Review:",
      choices = review_dates,
      selected = review_date_sel,
      multiple = FALSE
    ),
    # Radio buttons for page selection
    radioButtons("page_selection", "Select Page:",
                 choices = c(
                   "Interactive Dashboard" = "int_dash",
                   "Data Overview" = "data_overview"
                 ),
                 selected = "int_dash"
    )
  ),
  
  # Placeholder for dynamic content
  uiOutput("dynamic_content")
)

# Define server logic
server <- function(input, output, session) {
  # Render the dynamic content based on the selected page
  output$dynamic_content <- renderUI({
    if (input$page_selection == "int_dash") {
      tagList(
        tabsetPanel(
          id = "tabs", # Optional: give an ID to the tabset
          tabPanel(
            "Most Recent Values",
            # Content for Tab 1
            card(
              class = "card-full-page",
              plotOutput(outputId = "stack_pop", click = "dot_plot_click")
            )
          ),
          tabPanel(
            "Users Without Reviews",
            # Content for Tab 2
            card(
              class = "table",
              tableOutput("missing_user_table")
            )
          )
        ),
        layout_columns(
          col_widths = c(5, 7),
          layout_columns(
            col_widths = c(12, 12), # Full width for the layout
            row_heights = c(3, 1), # Height proportions
            card(
              style = "max-height: 300px; height: auto",
              plotOutput(outputId = "behave_bar", hover = "behave_bar_hover")
            ),
            card(
              style = "max-height: 100px; height: auto",
              tags$h3("Behaviour:", class = "card-title"),
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              tags$div(
                class = "card-content",
                textOutput("hover_info")
              )
            )
          ),
          layout_columns(
            col_widths = c(7, 5, 12),
            card(
              style = "max-height: 200px; height: auto",
              plotOutput(outputId = "overall_graphic")
            ),
            card(
              style = "max-height: 200px; height: auto",
              width = 12,
              status = "primary",
              tags$h3("User ID:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_id")),
              tags$h3("Review ID:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_review")),
              tags$h3("Reviewer:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_reviewer")),
              tags$h3("Days Since Review:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_days"))
            ),
            card(
              style = "max-height: 200px; height: auto",
              plotOutput(outputId = "time_series", click = "time_series_click")
            )
          ),
        )
      )
    } else {
      tagList(
        layout_columns(
          col_widths = c(6, 6, 12, 6, 6, 12, 8, 4),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "users_per_pop")
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "reviews_per_pop")
          ),
          card(
            class = "card-content",
            (paste(
              "There are", num_users, "users across", num_ind_pops, "populations. A total of",
              num_reviews, "reviews have been completed.", num_self_reviews, "of these are 'Self' reviews."
            ))
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "review_relationship_dist")
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "review_diff")
          ),
          card(
            class = "card-content",
            (paste(
              "The average behavioural value for 'Self' reviews is higher than any other review type.", "
                              Reviews by staff of a line manager are lowest.  Of users who have more than one review, ignoring self reviews, many do not show an improvement in average behavioural values",
              "between their initial and latest review,", paste0(prop_reduced_review_score, "%"), "show a decrease."
            ))
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "character_set_dist")
          ),
          card(
            class = "card-content",
            (paste(
              "For people with a complete set of known characteristics there are", num_char_set, "different characteristic sets with",
              num_unique_char_set, "people having a combination of characters unique to them."
            ))
          )
        )
      )
    }
  })
  
  ### reactive data
  
  ### Population level Reactive Data
  stack_plot_data <- reactive({
    list(
      new_point = cur_data_set()[cur_data_set()$user_id == selected_id(), ],
      cur_data = cur_data_set()
    )
  })
  
  ind_plot_data <- reactive({
    
    sel_id <- selected_id()
    sel_type <- selected_type()
    sel_review <- selected_review()
    current_full_data <- full_data_set()
    current_selected_id <- selected_id()
    
    review_type_sel <- {
      if (sel_type == "All") {
        c("Peer", "Manager", "Staff", "Self")
      } else {
        sel_type
      }
    }
    

    
    # Subset dataset based on currently selected values
    
    data_subset_all_reviews <- current_full_data[
      current_full_data$user_id == current_selected_id,
    ]
    
    data_subset <- current_full_data[
      current_full_data$user_id == current_selected_id &
        current_full_data$relationship %in% review_type_sel &
        current_full_data$review_id == sel_review,
    ]
    
    ind_data_subset <- average_score_per_review[average_score_per_review$user_id == current_selected_id &
                                                  average_score_per_review$review_id %in% current_full_data$review_id, ]
    
    #av_score_per_behave <- aggregate(value ~ behaviour_id + behaviour_title, data_subset, mean, na.rm = TRUE)
    
    #av_score_per_behave <- av_score_per_behave[order(av_score_per_behave$value), ]
    
    
    pop_col <- col_pal[match(data_subset$u.population_id[1], current_population_ids)]
    
    # Darker color is used for self reviews
    
    dark_pop_col <- adjustcolor(pop_col,
                                alpha.f = 1,
                                red.f = 0.75,
                                green.f = 0.75,
                                blue.f = 0.75
    )
    
    list(
      ind_data_subset = ind_data_subset,
      data_subset = data_subset,
      data_subset_all_reviews = data_subset_all_reviews,
      pop_col = c(light = pop_col, dark = dark_pop_col)
      #av_score_per_behave = av_score_per_behave
    )
  })
  
  
  
  observeEvent(input$exclude_self, {
    if (input$exclude_self) {
      full_data_set(test_data_csv_verified[test_data_csv_verified$relationship != "Self", ])
      av_review_set(average_score_per_review[average_score_per_review$relationship != "Self", ])
    } else {
      full_data_set(test_data_csv_verified)
      av_review_set(average_score_per_review)
    }
  })
  
  observeEvent(c(input$pop_sel, input$exclude_self), {
    data_subset <- latest_average_scores[latest_average_scores$user_id %in% full_data_set()$user_id, ]
    data_subset <- data_subset[data_subset$u.population_id %in% input$pop_sel, ]
    
    users_sub <- sort(unique(data_subset$user_id))
    
    
    
    # convert list back into dataframe
    cur_data_set(create_stacked_dot_plot_data(data_subset))
    
    updateSelectizeInput(session, "id_sel",
                         choices = users_sub,
                         selected = users_sub[1], server = TRUE
    ) # Reset selection
  })
  
  
  # When new ID is selected update review_type_sel & review_date_sel to latest
  observeEvent(input$id_sel, {
    if (!is.null(input$id_sel) && input$id_sel != "") {
      
      data_subset_all_reviews <- av_review_set()
      
      review_date_sub <- unique(data_subset_all_reviews[data_subset_all_reviews$user_id == input$id_sel,  
                                                        c("relationship", "completed_at", "review_id")])
      
      review_date_sub <- review_date_sub[order(review_date_sub$completed_at, decreasing = TRUE), ]
      
      updateSelectInput(session, "review_type_sel",
                        choices = c(review_date_sub$relationship, "All"),
                        selected = review_date_sub$relationship[1]
      )
      
      review_dates <- review_date_sub[review_date_sub[,"relationship"]==review_date_sub$relationship[1], 
                                      c("completed_at", "review_id")]
      cur_reviews <- format_review_date(review_dates)
      
      updateSelectInput(session, "review_date_sel",
                        choices =  cur_reviews,
                        selected =  cur_reviews[1]
      ) # Reset selection
      time_series_click(TRUE)
      selected_type(review_date_sub$relationship[1])
      selected_review(review_dates[1, "review_id"])
      selected_id(input$id_sel)
    }
  })
  
  # When new review type is selected update review_date_sel to latest
  observeEvent(input$review_type_sel, {
    # TODO this is bit of a hack to stop updating if you click on the time series plot
    # Need to improve reactivity so that this sort of thing doesn't happen
    if(!time_series_click()){
      if (input$review_type_sel == "All" | input$review_type_sel == "") {
        review_type_sel <- c("Peer", "Manager", "Staff", "Self")
      } else {
        review_type_sel <- input$review_type_sel
      }
      
      data_subset_all_reviews <- isolate(av_review_set())
      review_date_sub <- unique(data_subset_all_reviews[data_subset_all_reviews$user_id == input$id_sel &
                                                          data_subset_all_reviews$relationship  %in% review_type_sel,  
                                                        c("relationship", "completed_at", "review_id")])
      
      cur_review_list <- format_review_date(review_date_sub)
      updateSelectInput(session, "review_date_sel",
                        choices = cur_review_list,
                        selected = cur_review_list[1]
      ) 
      
      # Reset selection
      selected_review(review_date_sub[1, "review_id"])
      selected_type(input$review_type_sel)
    }
  })
  
  
  # When new review date is selected updated selected_date() value
  observeEvent(input$review_date_sel, {
    if(time_series_click()){
      time_series_click(FALSE)
      
    } else {
      selected_review(extract_review_id(input$review_date_sel))
    }
  })
  
  # When when point is clicked on dot plot change to the relevant ID
  # Also save point value so it can be enlarged
  observeEvent(input$dot_plot_click, {
    new_point <- nearPoints(cur_data_set(),
                            input$dot_plot_click,
                            threshold = 5,
                            maxpoints = 1
    )
    
    if (nrow(new_point) > 0 && "user_id" %in% colnames(new_point)) {
      # Update the SelectizeInput only if the point is valid
      updateSelectizeInput(session, "id_sel",
                           choices = cur_data_set()$user_id,
                           selected = new_point$user_id, server = TRUE
      )
    }
  })
  
  # When when point is clicked on time-series change to correct review date & type
  # Also save point value so it can be enlarged
  observeEvent(input$time_series_click, {
    ind_data_subset <- ind_plot_data()[["ind_data_subset"]]
    new_point <- nearPoints(ind_data_subset,
                            input$time_series_click,
                            threshold = 5,
                            maxpoints = 1
    )
    
    if (nrow(new_point) > 0 && "completed_at" %in% colnames(new_point)) {
      updateSelectInput(session, "review_type_sel",
                        selected = new_point$relationship[1]
      )
      
      
      data_subset_all_reviews <- isolate(av_review_set())
      
      review_date_sub <- unique(data_subset_all_reviews[data_subset_all_reviews$review_id == new_point$review_id,  
                                                        c("relationship", "completed_at", "review_id")])
      cur_reviews <-  format_review_date(review_date_sub)
      
      sel_review <- new_point[,c("completed_at", "review_id")]
      
      updateSelectInput(session, "review_date_sel",
                        choices =  cur_reviews,
                        selected = cur_reviews[1]
      ) # Reset selection
      
      selected_type(new_point$relationship[1])
      selected_review(sel_review[, "review_id"])
      time_series_click(TRUE)
    }
  })
  
  
  
  ### Create Basic ID Summary Graphic
  output$overall_graphic <- renderPlot({
    data_subset <- ind_plot_data()[["data_subset"]]
    pop_col <- ind_plot_data()[["pop_col"]][["light"]]
    dark_pop_col <-  ind_plot_data()[["pop_col"]][["dark"]]
    
    average_value <- round(mean(data_subset$value, na.rm = TRUE), 2)
    
    # Create dummy data
    data <- data.frame(
      category = c("A", "B"),
      count = c(10 - average_value, average_value)
    )
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n = -1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, "\n value: ", data$count)
    
    point_col <- if (data_subset$relationship[1] == "Self") dark_pop_col else pop_col
    
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(
        size = 5, family = graph_font,
        x = 2, y = 0.25, label = paste0("Overall:\n ", average_value),
        label.padding = unit(0.1, "lines"),
        lineheight = 0.75
      ) +
      scale_fill_manual(values = c(
        "B" = point_col,
        "A" = alpha(point_col, 0.5)
      )) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(
        text = element_text(family = graph_font),
        legend.position = "none"
      )
  })
  
  ### Get Text Summaries
  output$info_box_id <- renderText({
    return(as.numeric(selected_id()))
  })
  
  output$info_box_review <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    return(as.numeric(data_subset[1, c("review_id")]))
  })
  
  output$info_box_reviewer <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    return(as.numeric(data_subset[1, c("reviewer_id")]))
  })
  
  output$info_box_days <- renderText({
    data_subset <- isolate(ind_plot_data()[["data_subset"]])
    sel_review <- selected_review()
    review_date <- data_subset[data_subset$review_id == sel_review, "completed_at"]
    return(as.numeric(cur_date - as.Date(review_date[1])))
  })
  
  
  ### Create Time Series Plot
  output$time_series <- renderPlot({
    cur_data <- stack_plot_data()[["cur_data"]]
    ind_data_subset <- ind_plot_data()[["ind_data_subset"]]
    pop_col <- ind_plot_data()[["pop_col"]][["light"]]
    pop_col_dark <- ind_plot_data()[["pop_col"]][["dark"]]
    data_subset <- ind_plot_data()[["data_subset"]]
    
    
    
    # Subset the data
    ind_data_subset_self <- ind_data_subset[ind_data_subset$relationship == "Self", ]
    ind_data_subset_other <- ind_data_subset[ind_data_subset$relationship != "Self", ]
    
    # Create a sequence of x values for predictions
    
    date_range <- seq(min(ind_data_subset$completed_at),
                      max(ind_data_subset$completed_at),
                      length.out = 100
    )
    
    
    #  Function to get best attempt at a spline depending on the number of points
    create_spline_data <- function(df, date_range) {
      num_dates <- length(unique(df$completed_at))
      if (num_dates > 3) {
        spline_predict <- smooth.spline(df$completed_at,
                                        df$value,
                                        all.knots = TRUE,
                                        spar = 0.5
        )
        return(data.frame(
          predict(spline_predict, newdata = data.frame(completed_at = date_range))
        ))
      } else if (num_dates > 1) {
        spline_predict <- lm(value ~ completed_at, df[, c("completed_at", "value")])
        return(data.frame(
          x = date_range,
          y = predict(spline_predict,
                      newdata = data.frame(completed_at = date_range, value = NA)
          )
        ))
      } else if (num_dates == 1) {
        return(data.frame(x = date_range, y = df$value))
      } else {
        blank_df <- data.frame(matrix(ncol = 2, nrow = 0))
        names(blank_df) <- c("x", "y")
        return(blank_df)
      }
    }
    
    # We want to plot the self review and other reviews separately
    
    spline_int_self <- create_spline_data(ind_data_subset_self, date_range)
    spline_int_other <- create_spline_data(ind_data_subset_other, date_range)
    
    review_displayed <- data_subset[1, c("review_id")]
    
    new_point <- ind_data_subset[ind_data_subset$review_id == review_displayed, ]
    
    high_point_col <- if (new_point$relationship == "Self") pop_col_dark else pop_col
    
    
    ggplot() +
      geom_point(data = ind_data_subset_self, aes(x = completed_at, y = value), color = pop_col_dark, size = point_size_small, shape = small_plot_point) +
      geom_point(data = ind_data_subset_other, aes(x = completed_at, y = value), color = pop_col, size = point_size_small, shape = small_plot_point) +
      geom_point(data = new_point, aes(x = completed_at, y = value), color = high_point_col, size = point_size_large, shape = small_plot_point) +
      geom_line(data = spline_int_other, aes(x = as.Date(x), y = y), color = pop_col) +
      geom_line(data = spline_int_self, aes(x = as.Date(x), y = y), color = pop_col_dark) +
      labs(x = "Review Date", y = "Value") +
      xlim(min(date_range) - 30, max(date_range) + 30) +
      ylim(0, 10) +
      theme_minimal() +
      theme(text = element_text(family = graph_font, size = graph_font_size))
  })
  
  ### Create The Behavioural Bar plot
  output$behave_bar <- renderPlot({
    data_subset <- ind_plot_data()[["data_subset"]]
    pop_col <- ind_plot_data()[["pop_col"]][["light"]]
    dark_pop_col <- ind_plot_data()[["pop_col"]][["dark"]]
    #av_score_per_behave <- ind_plot_data()[["av_score_per_behave"]]
    point_col <- if (data_subset$relationship[1] == "Self") dark_pop_col else pop_col
    ggplot(data = data_subset , aes(
      x = factor(behaviour_id,
                 levels = behaviour_id
      ),
      y = value
    ), ) +
      geom_bar(stat = "identity", fill = point_col) +
      coord_flip() +
      ylim(c(0, 10)) +
      theme_minimal() +
      labs(y = "Value", x = "Behaviour") +
      theme(
        text = element_text(family = graph_font, size = graph_font_size),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  ### Set up display of behaviour on hover
  output$hover_info <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    pop_col <- ind_plot_data()[["pop_col"]]
    #av_score_per_behave <- ind_plot_data()[["av_score_per_behave"]]
    
    hover <- input$behave_bar_hover
    if (is.null(hover)) {
      return("Hover over a bar to see the scored behaviour.")
    }
    
    # Calculate which bar is being hovered over
    bar_index <- round(hover$y)
    if (bar_index >= 1 && bar_index <= nrow(data_subset)) {
      value <- data_subset$value[bar_index]
      category <- data_subset$behaviour_title[bar_index]
      return(category)
    } else {
      return("Hover over a bar to see the scored behaviour.")
    }
  })
  
  
  ### Create The Stacked Dot Plot
  output$stack_pop <- renderPlot({
    new_point <- stack_plot_data()[["new_point"]]
    isolate({
      cur_data <- stack_plot_data()[["cur_data"]]
    })
    sel_col <- col_pal[match(new_point$u.population_id[1], current_population_ids)]
    
    
    ggplot(cur_data, aes(x = value_group, y = plot_height, color = factor(u.population_id, levels = input$pop_sel))) +
      geom_point(size = point_size_small, shape = small_plot_point) +
      geom_point(data = new_point, aes(x = value_group, y = plot_height), colour = sel_col, size = point_size_large, shape = small_plot_point) +
      labs(x = "Values", y = "") +
      xlim(0, 10) +
      ylim(0, 30) +
      scale_color_manual(values = col_pal[match(input$pop_sel, current_population_ids)]) +
      theme_minimal() +
      theme(
        text = element_text(family = graph_font, size = graph_font_size),
        legend.position = "none",
        axis.text.y = element_blank(), # Remove y-axis text
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.title.y = element_blank(), # Remove y-axis title
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        plot.title = element_text(size = 14)
      )
  })
  
  ## Create Users Per Pop Barchart
  output$users_per_pop <- renderPlot({
    ggplot(data = all_users, aes(x = factor(u.population_id))) +
      geom_bar(fill = rgb(1, 0.5, 0, 0.7)) +
      #  geom_bar(data = average_score_per_review, aes(x = factor(u.population_id), fill = factor(relationship)), position = "stack") +  # Stacked bar plot
      theme_minimal() +
      labs(y = "Number of Users", x = "Population")
  })
  
  ## Create Reviews Per Pop Barchart
  output$reviews_per_pop <- renderPlot({
    ggplot(data = average_score_per_review, aes(
      x = factor(u.population_id),
      fill = factor(relationship)
    )) +
      geom_bar(position = "stack") + # Stacked bar plot
      scale_fill_brewer(palette = "YlOrRd") +
      labs(
        x = "Population",
        y = "Number of Reviews",
        fill = "Review Type"
      ) + # Label for the fill legend
      theme_minimal() +
      guides(fill = guide_legend(ncol = 2)) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.79, 0.55),
        legend.title = element_text(size = 12), # Increase legend title size
        legend.text = element_text(size = 10)
      )
  })
  
  ## Create Review Diff Histogram
  output$review_diff <- renderPlot({
    ggplot(first_last_df, aes(x = score_diff)) +
      geom_histogram(binwidth = 0.5, fill = rgb(1, 0.5, 0, 0.7)) +
      labs(
        title = "Increase from Initial Reviews",
        x = "Average Value Difference",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  ## Create Review Relationship Histogram
  output$review_relationship_dist <- renderPlot({
    custom_colors <- scales::alpha(brewer.pal(n = 4, name = "YlOrRd"), alpha = 0.4)
    
    ggplot(average_score_per_review, aes(x = value, fill = relationship)) +
      geom_histogram(binwidth = 0.5, position = "identity") +
      scale_fill_manual(values = custom_colors) +
      labs(
        x = "Average Review Value",
        y = "Count",
        fill = "Relationship"
      ) +
      guides(fill = guide_legend(ncol = 2)) +
      theme_minimal() +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.60, 0.85),
        legend.title = element_text(size = 12), # Increase legend title size
        legend.text = element_text(size = 10)
      )
  })
  
  
  ## Create Character Set Barchart
  output$character_set_dist <- renderPlot({
    custom_colors <- scales::alpha(brewer.pal(n = 4, name = "YlOrRd"), alpha = 0.8)
    
    custom_colors <- c(custom_colors[4:2], rep(custom_colors[1], nrow(count_character_sets) - 3))
    
    ggplot(count_character_sets, aes(x = idx, y = Freq, fill = factor(idx))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = custom_colors,
        breaks = c("1", "2", "3"),
        labels = c(
          "1" = "Heterosexual White Man, 25-34",
          "2" = "Heterosexual White Man, 35-44",
          "3" = "Heterosexual White Woman, 25-34"
        )
      ) +
      theme_minimal() +
      labs(y = "Number of Users", x = "Character Set", fill = "Character Set") +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.65, 0.65),
        axis.text.x = element_blank(), # Remove x-axis text
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        legend.title = element_text(size = 12), # Increase legend title size
        legend.text = element_text(size = 10) # Increase legend text size
      )
  })
  
  # Find any users that are in the selected populations that don't have any review data
  output$missing_user_table <- renderTable({
    cur_data <- stack_plot_data()[["cur_data"]]
    
    selected_pops <- unique(cur_data[, "u.population_id"])
    
    all_users_sel_pops <- all_users[all_users[, "u.population_id"] %in% selected_pops, ]
    all_users_sel_pops[, "Value"] <- NA
    missing_table <- all_users_sel_pops[!all_users_sel_pops[, "user_id"] %in% cur_data[, "user_id"], , drop = FALSE]
    names(missing_table) <- c("User", "Population", "Value")
    
    return(missing_table)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
