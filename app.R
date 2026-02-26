# CREDIBLE Data Wrangler - Shiny App
# A tool for K-12 students to prepare data for analysis

library(shiny)
library(tidyverse)
library(DT)
library(httr)
library(jsonlite)
library(readxl)     # ensure readxl is available for .xlsx
library(stringr)    # robust regex for code extraction
library(markdown)   # for rendering markdown in chat

# Get API key from environment variable (for deployment)
# Set this in your .Renviron file or Shiny server settings
CLAUDE_API_KEY <- Sys.getenv("CLAUDE_API_KEY")

# Add resource path for logo
addResourcePath("images", ".")

# UI Definition
ui <- fluidPage(
  titlePanel("CREDIBLE Data Wrangler"),
  
  tags$head(
    tags$style(HTML("
      /* Still Water palette — cohesive teal color family
         Primary:   #3B7A8C  (lake teal)
         Dark:      #2A5F70  (deep water, hovers)
         Muted:     #6A9AA6  (secondary elements)
         Accent:    #4A9BAA  (highlights)
      */

      /* Main theme */
      body {
        background-color: #f8f9fa;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      h3, h4, h5 { color: #2A5F70; }

      /* Chat container */
      .chat-container {
        height: 500px;
        overflow-y: auto;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 15px;
        margin-bottom: 10px;
        background-color: #ffffff;
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      }

      /* Chat messages */
      .user-message {
        background-color: #3B7A8C;
        color: white;
        padding: 10px 15px;
        margin: 8px 0;
        border-radius: 12px 12px 4px 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .assistant-message {
        background-color: #f8f9fa;
        color: #333;
        padding: 10px 15px;
        margin: 8px 0;
        border-radius: 12px 12px 12px 4px;
        border: 1px solid #dee2e6;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }

      /* Data preview section */
      .data-preview {
        margin: 20px 0;
        background-color: white;
        padding: 15px;
        border-radius: 6px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      }

      /* Sidebar styling */
      .well {
        background-color: #ffffff;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      }

      /* Button styling */
      .btn-primary {
        background-color: #3B7A8C !important;
        border-color: #2A5F70 !important;
        border-radius: 5px;
        padding: 10px 16px;
        font-weight: 500;
        transition: background-color 0.2s;
      }
      .btn-primary:hover, .btn-primary:focus {
        background-color: #2A5F70 !important;
        border-color: #1F4A58 !important;
      }

      /* Input styling */
      .form-control {
        border: 1px solid #dee2e6;
        border-radius: 5px;
        transition: border-color 0.2s;
      }
      .form-control:focus {
        border-color: #3B7A8C;
        box-shadow: 0 0 0 0.2rem rgba(59, 122, 140, 0.25);
      }

      /* Step history cards */
      .step-card {
        border: 1px solid #dee2e6;
        padding: 15px;
        margin: 15px 0;
        border-radius: 6px;
        background-color: white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
        border-left: 4px solid #3B7A8C;
      }

      /* Status text */
      pre {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-left: 4px solid #4A9BAA;
        border-radius: 4px;
        padding: 12px;
        font-family: monospace;
      }

      /* Prompt suggestions */
      .prompt-suggestion {
        display: inline-block;
        background-color: #e9ecef;
        color: #2A5F70;
        padding: 6px 12px;
        margin: 4px;
        border-radius: 15px;
        border: 1px solid #dee2e6;
        font-size: 13px;
        cursor: pointer;
        transition: all 0.2s;
        text-decoration: none;
      }
      .prompt-suggestion:hover {
        background-color: #6A9AA6;
        color: white;
        border-color: #6A9AA6;
      }
      .suggestions-label {
        font-size: 12px;
        color: #6c757d;
        font-style: italic;
        margin-bottom: 5px;
      }

      /* Links */
      a { color: #3B7A8C; }
      a:hover { color: #2A5F70; }

      /* Title */
      .title-panel h2 {
        color: #2A5F70;
        font-weight: 600;
      }
    ")),
    # CODAP Integration: IFramePhone library for communication
    tags$script(src="https://unpkg.com/iframe-phone@1.4.0/dist/iframe-phone.js"),
    tags$script(HTML("
      // Initialize CODAP connection using IFramePhone
      var codapPhone = null;
      var codapConnectionInitialized = false;

      function initCodapConnection() {
        if (codapConnectionInitialized) return;

        try {
          console.log('Initializing CODAP connection with IFramePhone...');

          // Check if IFramePhone is available
          if (typeof iframePhone === 'undefined') {
            console.error('IFramePhone library not loaded');
            return;
          }

          // Create phone connection to CODAP
          codapPhone = new iframePhone.IframePhoneRpcEndpoint(
            function(command, callback) {
              // Handler for messages FROM CODAP (we don't expect any in this simple case)
              console.log('Received message from CODAP:', command);
              if (callback) callback({success: true});
            },
            'data-interactive',
            window.parent
          );

          codapConnectionInitialized = true;
          console.log('CODAP connection established successfully');
        } catch (e) {
          console.error('Error initializing CODAP connection:', e);
        }
      }

      // Call init when page loads
      window.addEventListener('load', function() {
        initCodapConnection();
      });

      // CODAP Interface Helper Function
      // This function sends messages to CODAP using the Data Interactive Plugin API via IFramePhone
      function codapInterface(action, resource, values) {
        return new Promise(function(resolve, reject) {
          // Check if running inside CODAP (has a parent frame different from self)
          if (window === window.parent) {
            console.warn('Not running inside CODAP - no parent frame detected');
            reject({
              error: 'Not running in CODAP',
              message: 'This app must be embedded in CODAP to use the Send to CODAP feature. Please open CODAP at codap.concord.org and add this app as a Data Interactive plugin.',
              helpUrl: 'https://codap.concord.org/'
            });
            return;
          }

          // Initialize connection if not already done
          if (!codapConnectionInitialized) {
            initCodapConnection();
          }

          // Check if phone is available
          if (!codapPhone) {
            reject({
              error: 'CODAP connection not established',
              message: 'Unable to establish connection with CODAP. Make sure you are using the latest CODAP version.',
              helpUrl: 'https://codap.concord.org/'
            });
            return;
          }

          var message = {
            action: action,
            resource: resource,
            values: values
          };

          console.log('Sending to CODAP via IFramePhone:', message);

          // Send via IFramePhone
          codapPhone.call(message, function(response) {
            console.log('CODAP Response:', response);
            if (response && response.success) {
              resolve(response);
            } else {
              reject(response || {error: 'Unknown error', message: 'CODAP returned an error'});
            }
          });
        });
      }

      // Custom Shiny Message Handler: sendToCODAP
      // This receives data from R/Shiny and sends it to CODAP
      Shiny.addCustomMessageHandler('sendToCODAP', function(payload) {
        console.log('Received sendToCODAP message from Shiny:', payload);
        console.log('Current window location:', window.location.href);
        console.log('Parent window exists:', window.parent !== window);

        var datasetName = payload.datasetName || 'MyData';
        var attributes = payload.attributes || [];
        var cases = payload.cases || [];

        console.log('Dataset name:', datasetName);
        console.log('Number of attributes:', attributes.length);
        console.log('Number of cases:', cases.length);

        // Step 1: Create CODAP dataContext with attributes
        codapInterface('create', 'dataContext', {
          name: datasetName,
          title: datasetName,
          description: 'Data exported from CREDIBLE Data Wrangler Shiny App',
          collections: [{
            name: datasetName + '_collection',
            title: datasetName,
            attrs: attributes
          }]
        })
        .then(function(response) {
          console.log('DataContext created successfully:', response);

          // Step 2: Send data rows as cases to CODAP
          return codapInterface('create', 'dataContext[' + datasetName + '].item', cases);
        })
        .then(function(response) {
          console.log('Cases sent successfully:', response);
          console.log('Total cases sent:', cases.length);

          // Notify Shiny of success
          Shiny.setInputValue('codap_export_status', {
            success: true,
            message: 'Successfully sent ' + cases.length + ' rows to CODAP dataset: ' + datasetName,
            timestamp: new Date().getTime()
          }, {priority: 'event'});
        })
        .catch(function(error) {
          console.error('Error sending data to CODAP:', error);

          // Create helpful error message
          var errorMsg = error.message || error.error || 'Unknown error';
          if (error.helpUrl) {
            errorMsg += ' Visit: ' + error.helpUrl;
          }

          // Notify Shiny of error
          Shiny.setInputValue('codap_export_status', {
            success: false,
            message: errorMsg,
            timestamp: new Date().getTime()
          }, {priority: 'event'});
        });
      });

      // === CODAP IMPORT FUNCTIONS ===

      // Get list of all datasets available in CODAP
      function getAvailableDatasets() {
        return codapInterface('get', 'dataContextList', {});
      }

      // Import a specific dataset from CODAP
      function importDataFromCODAP(datasetName) {
        console.log('Importing dataset from CODAP:', datasetName);

        // First get the dataset structure to understand columns
        return codapInterface('get', 'dataContext[' + datasetName + ']', {})
          .then(function(contextResponse) {
            console.log('Dataset structure:', contextResponse);

            // Then get all the data rows
            return codapInterface('get', 'dataContext[' + datasetName + '].item', {})
              .then(function(casesResponse) {
                console.log('Retrieved', casesResponse.values.length, 'cases');

                // Send to R/Shiny
                Shiny.setInputValue('codap_imported_data', {
                  datasetName: datasetName,
                  cases: casesResponse.values,
                  timestamp: new Date().getTime()
                }, {priority: 'event'});

                return casesResponse.values;
              });
          });
      }

      // Handler: Request available datasets from CODAP
      Shiny.addCustomMessageHandler('requestCODAPDatasets', function(payload) {
        console.log('Requesting available datasets from CODAP...');

        getAvailableDatasets()
          .then(function(response) {
            console.log('Available datasets:', response.values);
            Shiny.setInputValue('codap_available_datasets', {
              datasets: response.values || [],
              timestamp: new Date().getTime()
            }, {priority: 'event'});
          })
          .catch(function(error) {
            console.error('Error getting datasets:', error);
            Shiny.setInputValue('codap_available_datasets', {
              datasets: [],
              error: error.message || 'Failed to get datasets',
              timestamp: new Date().getTime()
            }, {priority: 'event'});
          });
      });

      // Handler: Import a specific dataset from CODAP
      Shiny.addCustomMessageHandler('importFromCODAP', function(payload) {
        console.log('Import request for dataset:', payload.datasetName);

        importDataFromCODAP(payload.datasetName)
          .catch(function(error) {
            console.error('Error importing dataset:', error);
            Shiny.setInputValue('codap_import_error', {
              message: error.message || 'Failed to import dataset',
              timestamp: new Date().getTime()
            }, {priority: 'event'});
          });
      });

      console.log('CODAP interface initialized');
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Setup"),
      
      # File Upload
      fileInput("data_file", "Upload Data File",
                accept = c(".csv", ".xlsx", ".tsv", ".txt")),

      hr(),
      downloadButton("download_data", "Download Cleaned Data", class = "btn-primary"),

      # Advanced section (collapsible) - CODAP integration
      tags$details(
        style = "margin-top: 15px;",
        tags$summary(style = "cursor: pointer; color: #6c757d; font-size: 13px;", "CODAP Integration"),
        div(style = "padding: 10px 0;",
          # Import from CODAP
          h5("Import from CODAP", style = "font-size: 13px; margin-top: 5px;"),
          div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 8px;",
              actionButton("refresh_codap_datasets", "Refresh",
                           icon = icon("sync"),
                           class = "btn-sm",
                           style = "font-size: 11px;")
          ),
          selectInput("codap_dataset_select", NULL,
                      choices = c("(Click refresh)" = ""),
                      width = "100%"),
          actionButton("import_from_codap", "Import",
                       class = "btn-primary",
                       style = "width: 100%; font-size: 12px;",
                       icon = icon("download")),
          hr(),
          # Export to CODAP
          h5("Export to CODAP", style = "font-size: 13px;"),
          textInput("codap_dataset_name", "Dataset Name:", value = "WrangledData"),
          actionButton("send_to_codap", "Send to CODAP",
                       class = "btn-primary",
                       style = "width: 100%; font-size: 12px;",
                       icon = icon("share"))
        )
      ),

      # Logo footer
      div(style = "text-align: center; padding: 20px 0 10px 0; margin-top: 20px;",
          tags$img(src = "images/credible-logo.png", height = "80px",
                   style = "display: block; margin: 0 auto 8px auto; mix-blend-mode: multiply;"),
          tags$a(href = "https://projectcredible.com", target = "_blank",
                 style = "font-size: 12px; color: #3B7A8C;", "projectcredible.com")
      )
    ),
    
    mainPanel(
      width = 9,
      # Header with help button
      fluidRow(
        column(10, h3("Let's Prepare Your Data!")),
        column(2, actionButton("show_help", "Help", icon = icon("question-circle"),
                               class = "btn-default", style = "margin-top: 15px;"))
      ),
      p("Upload your data and chat with me about what you'd like to do."),

      # Chat section
      div(class = "chat-container", id = "chat_box", uiOutput("chat_display")),
      fluidRow(
        column(10, textAreaInput("user_input", NULL, placeholder = "Type your message here...", width = "100%", rows = 2, resize = "vertical")),
        column(2, actionButton("send_btn", "Send", class = "btn-primary", width = "100%"))
      ),
      div(style = "margin-top: 10px;",
          uiOutput("prompt_suggestions")
      ),

      # Data preview section
      div(style = "margin-top: 20px;",
          h4("Your Data"),
          DTOutput("data_table")
      ),

      # Step history (collapsible)
      tags$details(
        style = "margin-top: 20px;",
        tags$summary(style = "cursor: pointer; color: #6c757d;", "View completed steps"),
        div(style = "padding: 10px 0;",
            uiOutput("step_history")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Capture a stable environment to evaluate generated code
  server_env <- environment()
  
  # Reactive values to store state
  rv <- reactiveValues(
    current_data = NULL,
    original_data = NULL,
    chat_history = list(),
    conversation_history = list(), # For Claude API
    step_history = list(),
    current_step = "No data loaded",
    awaiting_confirmation = FALSE,
    pending_code = NULL,
    pending_step_name = NULL,
    pending_step_description = NULL
  )
  
  # Load data when file is uploaded
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      file_ext <- tools::file_ext(input$data_file$name)
      rv$original_data <- switch(
        file_ext,
        "csv" = readr::read_csv(input$data_file$datapath, show_col_types = FALSE),
        "xlsx" = readxl::read_excel(input$data_file$datapath),
        "tsv" = readr::read_tsv(input$data_file$datapath, show_col_types = FALSE),
        "txt" = readr::read_delim(input$data_file$datapath, show_col_types = FALSE),
        stop("Unsupported file type")
      )
      rv$current_data <- rv$original_data
      rv$current_step <- "Original data loaded"
      rv$chat_history <- append(rv$chat_history, list(
        list(role = "assistant", 
             content = paste0("Great! I've loaded your data. It has ",
                              nrow(rv$current_data), " rows and ",
                              ncol(rv$current_data), " columns. ",
                              "What would you like to do with it?"))
      ))
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Load example dataset (archived for now)
  # observeEvent(input$load_example, {
  #   tryCatch({
  #     rv$original_data <- readr::read_csv("plant_growth_experiment.csv", show_col_types = FALSE)
  #     rv$current_data <- rv$original_data
  #     rv$current_step <- "Example data loaded"
  #     rv$chat_history <- list(
  #       list(role = "assistant",
  #            content = paste0("Perfect! I've loaded the **Plant Growth Experiment** example dataset. ",
  #                             "This data tracks plant growth over 4 weeks for different treatment groups (Sunlight, Partial Shade, Full Shade).\n\n",
  #                             "The dataset has ", nrow(rv$current_data), " rows and ",
  #                             ncol(rv$current_data), " columns, including:\n",
  #                             "- Student names and groups\n",
  #                             "- Weekly height measurements\n",
  #                             "- Final leaf counts\n",
  #                             "- Treatment types\n\n",
  #                             "**Notice**: This is a messy dataset with typical issues like inconsistent formatting, ",
  #                             "different date formats, and missing values - perfect for learning data wrangling!\n\n",
  #                             "What would you like to do with it? For example, you could ask me to:\n",
  #                             "- Convert the weekly measurements to long format\n",
  #                             "- Clean up the inconsistent treatment names\n",
  #                             "- Summarize growth by treatment group"))
  #     )
  #     # Reset conversation history and step history
  #     rv$conversation_history <- list()
  #     rv$step_history <- list()
  #     rv$awaiting_confirmation <- FALSE
  #     rv$pending_code <- NULL
  #   }, error = function(e) {
  #     showNotification(paste("Error loading example data:", e$message), type = "error")
  #   })
  # })
  
  # Function to call Claude API (env-only key)
  call_claude <- function(user_message) {
    api_key <- CLAUDE_API_KEY
    if (nchar(api_key) == 0) {
      stop("Claude API key not found. Please set CLAUDE_API_KEY in your .Renviron file.")
    }
    
    # Prepare context about current data (improved schema)
    data_context <- ""
    if (!is.null(rv$current_data)) {
      col_types <- paste(
        paste0(
          names(rv$current_data), ":",
          vapply(rv$current_data, function(x) class(x)[1], "")
        ),
        collapse = ", "
      )
      data_context <- paste0(
        "\n\nCurrent data structure:\n",
        "- Rows: ", nrow(rv$current_data), "\n",
        "- Columns: ", paste(names(rv$current_data), collapse = ", "), "\n",
        "- Column types: ", col_types
      )
    }
    
    # System prompt
    system_prompt <- paste0(
      "You are a helpful data preparation assistant for K-12 students. ",
      "Your role is to help them clean and prepare data through three main operations:\n",
      "1. Pivoting data from wide to long format\n",
      "2. Grouping and summarizing (aggregating) data\n",
      "3. Joining multiple datasets\n\n",
      "You should:\n",
      "- Be educational and explain concepts in simple terms\n",
      "- Ask clarifying questions to understand their goals\n",
      "- Suggest appropriate data preparation steps\n",
      "- Explain why each step is needed\n",
      "- When ready to perform a step, provide R code using tidyverse wrapped in ```r ``` code blocks\n",
      "- The code should start with 'rv$current_data %>%' to transform the current data\n",
      "- Always explain what the code will do BEFORE showing the code\n",
      "- After providing code, the system will ask the user to confirm\n\n",
      "You can also help with simpler tasks like creating variables, filtering, or selecting columns.\n",
      "IMPORTANT: Always wrap R code in ```r and ``` markers.",
      data_context
    )
    
    # Build messages for API
    messages <- c(rv$conversation_history, list(list(role = "user", content = user_message)))
    
    # Call Claude API
    response <- httr::POST(
      url = "https://api.anthropic.com/v1/messages",
      httr::add_headers(
        "x-api-key" = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = "claude-sonnet-4-5-20250929",
        max_tokens = 2048,
        system = system_prompt,
        messages = messages
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, "parsed")
      if (is.null(result$content) || length(result$content) == 0 || is.null(result$content[[1]]$text)) {
        stop("API returned an unexpected structure: ", jsonlite::toJSON(result, auto_unbox = TRUE))
      }
      assistant_message <- result$content[[1]]$text
      rv$conversation_history <- append(rv$conversation_history, list(
        list(role = "user", content = user_message),
        list(role = "assistant", content = assistant_message)
      ))
      return(assistant_message)
    } else {
      stop("API call failed (", httr::status_code(response), "): ", httr::content(response, "text"))
    }
  }
  
  # Handle send button
  observeEvent(input$send_btn, {
    req(input$user_input)
    user_msg <- input$user_input
    updateTextInput(session, "user_input", value = "")
    
    # Confirmations
    if (rv$awaiting_confirmation && tolower(trimws(user_msg)) %in% c("yes", "y", "confirm", "ok", "proceed")) {
      tryCatch({
        new_data <- eval(parse(text = rv$pending_code), envir = server_env)
        if (!(is.data.frame(new_data) || tibble::is_tibble(new_data) || dplyr::is_grouped_df(new_data))) {
          stop("The generated code did not produce a data frame/tibble.")
        }
        previous_data <- rv$current_data
        rv$current_data <- new_data
        rv$step_history <- append(rv$step_history, list(list(
          name = rv$pending_step_name,
          description = rv$pending_step_description,
          code = rv$pending_code,
          timestamp = Sys.time()
        )))
        rv$current_step <- paste("Step", length(rv$step_history), ":", rv$pending_step_name)
        rv$chat_history <- append(rv$chat_history, list(
          list(role = "user", content = user_msg),
          list(role = "assistant", 
               content = paste0("✓ Step completed successfully! Your data now has ",
                                nrow(rv$current_data), " rows and ",
                                ncol(rv$current_data), " columns. ",
                                "What would you like to do next?"))
        ))
        rv$awaiting_confirmation <- FALSE
        rv$pending_code <- NULL
        rv$pending_step_name <- NULL
        rv$pending_step_description <- NULL
      }, error = function(e) {
        rv$chat_history <- append(rv$chat_history, list(
          list(role = "user", content = user_msg),
          list(role = "assistant", 
               content = paste0(
                 "Error executing code:\n\n```\n", rv$pending_code, "\n```\n\n",
                 "Details: ", e$message,
                 "\n\nWould you like me to propose a simpler step or a different approach?"
               ))
        ))
        rv$awaiting_confirmation <- FALSE
      })
      return()
    }
    
    # Rejections
    if (rv$awaiting_confirmation && tolower(trimws(user_msg)) %in% c("no", "n", "cancel", "stop")) {
      rv$chat_history <- append(rv$chat_history, list(
        list(role = "user", content = user_msg),
        list(role = "assistant", content = "No problem! Let's try something different. What would you like to do instead?")
      ))
      rv$awaiting_confirmation <- FALSE
      rv$pending_code <- NULL
      return()
    }
    
    # Normal message -> call Claude
    rv$chat_history <- append(rv$chat_history, list(list(role = "user", content = user_msg)))
    tryCatch({
      assistant_msg <- call_claude(user_msg)
      
      # Robust multi-line fenced code extraction (```r, ```R, ```{r}, or just ```)
      pattern <- "(?s)```\\s*\\{?[rR]?\\}?\\s*\\n(.*?)\\n```"
      m <- stringr::str_match(assistant_msg, pattern)
      if (!is.na(m[1, 2])) {
        code <- trimws(m[1, 2])
        rv$pending_code <- code
        rv$awaiting_confirmation <- TRUE
        rv$pending_step_name <- "Data Transformation"
        rv$pending_step_description <- "Suggested by assistant"
        assistant_msg <- paste0(
          assistant_msg,
          "\n\n**Ready to apply this change?** Type 'yes' to proceed or 'no' to try something else."
        )
      }
      
      rv$chat_history <- append(rv$chat_history, list(list(role = "assistant", content = assistant_msg)))
    }, error = function(e) {
      rv$chat_history <- append(rv$chat_history, list(list(role = "assistant", content = paste("Error:", e$message))))
    })
  })
  
  # Render chat display
  output$chat_display <- renderUI({
    messages <- lapply(rv$chat_history, function(msg) {
      class_name <- ifelse(msg$role == "user", "user-message", "assistant-message")
      # Convert markdown to HTML for proper formatting
      formatted_content <- HTML(markdown::markdownToHTML(
        text = msg$content, 
        fragment.only = TRUE,
        options = c("use_xhtml", "smartypants", "mathjax")
      ))
      div(class = class_name,
          tags$b(paste0(toupper(substr(msg$role, 1, 1)), substr(msg$role, 2, nchar(msg$role)), ":")),
          tags$br(),
          formatted_content)
    })
    tagList(messages)
  })
  
  # Render data table
  output$data_table <- renderDT({
    req(rv$current_data)
    datatable(rv$current_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Render step history
  output$step_history <- renderUI({
    if (length(rv$step_history) == 0) return(p("No steps completed yet."))
    step_items <- lapply(seq_along(rv$step_history), function(i) {
      step <- rv$step_history[[i]]
      div(
        class = "step-card",
        h4(paste("Step", i, ":", step$name)),
        p(tags$b("Description:"), step$description),
        p(tags$b("Completed:"), format(step$timestamp, "%Y-%m-%d %H:%M:%S")),
        tags$pre(step$code)
      )
    })
    tagList(step_items)
  })

  # Help modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "How to Use Data Wrangler",
      p("This tool helps you prepare messy data for analysis. Here's how:"),
      tags$ol(
        tags$li("Upload your data file"),
        tags$li("Chat with the assistant about what you want to do"),
        tags$li("Review your data as it updates"),
        tags$li("Type 'yes' to confirm changes, 'no' to try something else"),
        tags$li("Download your cleaned data when finished")
      ),
      hr(),
      p(tags$b("Questions?"), " Email us at ", tags$a(href = "mailto:projectcredible@utk.edu", "projectcredible@utk.edu")),
      easyClose = TRUE,
      footer = modalButton("Got it!")
    ))
  })
  
  # Render prompt suggestions
  output$prompt_suggestions <- renderUI({
    # Only show suggestions if data is loaded and fewer than 3 messages in chat
    if (is.null(rv$current_data) || length(rv$chat_history) > 3) {
      return(NULL)
    }
    
    suggestions <- list(
      "Help me understand my data",
      "Calculate averages by group"
    )
    
    suggestion_buttons <- lapply(seq_along(suggestions), function(i) {
      actionLink(
        inputId = paste0("suggestion_", i),
        label = suggestions[[i]],
        class = "prompt-suggestion"
      )
    })
    
    div(
      div(class = "suggestions-label", "Try these prompts:"),
      tagList(suggestion_buttons)
    )
  })
  
  # Handle suggestion clicks
  observe({
    suggestions <- c(
      "Help me understand my data",
      "Calculate averages by group"
    )

    lapply(seq_along(suggestions), function(i) {
      observeEvent(input[[paste0("suggestion_", i)]], {
        updateTextInput(session, "user_input", value = suggestions[[i]])
      })
    })
  })

  # CODAP Integration: Handle Send to CODAP button
  observeEvent(input$send_to_codap, {
    # Get current data
    data <- rv$current_data

    # Validate data exists
    if (is.null(data) || nrow(data) == 0) {
      showNotification("No data available to send to CODAP. Please load data first.", type = "error")
      return()
    }

    # Get dataset name (with fallback)
    dataset_name <- input$codap_dataset_name
    if (is.null(dataset_name) || dataset_name == "") {
      dataset_name <- "WrangledData"
    }

    # Show initial notification
    showNotification(
      paste0("Sending ", nrow(data), " rows to CODAP as dataset: ", dataset_name),
      type = "message",
      duration = 3
    )

    # Convert columns to CODAP attributes format
    attributes <- lapply(names(data), function(col_name) {
      list(name = col_name, title = col_name)
    })

    # Convert rows to CODAP cases format
    cases <- lapply(seq_len(nrow(data)), function(i) {
      row_data <- as.list(data[i, ])
      # Critical: Convert NA to NULL for JSON serialization
      row_data <- lapply(row_data, function(x) {
        if (is.na(x)) return(NULL) else return(x)
      })
      return(row_data)
    })

    # Send to JavaScript via custom message handler
    session$sendCustomMessage(
      type = "sendToCODAP",
      message = list(
        datasetName = dataset_name,
        attributes = attributes,
        cases = cases
      )
    )
  })

  # CODAP Integration: Handle export status feedback from JavaScript
  observeEvent(input$codap_export_status, {
    status <- input$codap_export_status
    if (!is.null(status) && !is.null(status$success)) {
      if (status$success) {
        showNotification(status$message, type = "message", duration = 5)
      } else {
        showNotification(
          HTML(paste0(
            "CODAP Export Error: ", status$message,
            "<br><br><strong>Tip:</strong> Make sure this app is embedded in CODAP as a Data Interactive plugin. ",
            "To do this, open CODAP at codap.concord.org, then drag a 'Data Interactive' plugin from the plugin menu ",
            "and enter this app's URL.",
            "<br><br>Alternatively, use the 'Download Cleaned Data' button to export as CSV."
          )),
          type = "error",
          duration = 15
        )
      }
    }
  })

  # CODAP Integration: Request available datasets from CODAP
  observeEvent(input$refresh_codap_datasets, {
    showNotification("Requesting datasets from CODAP...", type = "message", duration = 2)
    session$sendCustomMessage(type = "requestCODAPDatasets", message = list())
  })

  # CODAP Integration: Handle received dataset list from CODAP
  observeEvent(input$codap_available_datasets, {
    response <- input$codap_available_datasets

    if (!is.null(response$error)) {
      showNotification(
        paste("Could not get CODAP datasets:", response$error),
        type = "warning",
        duration = 5
      )
      updateSelectInput(session, "codap_dataset_select",
                        choices = c("(Not connected to CODAP)" = ""))
      return()
    }

    datasets <- response$datasets
    if (length(datasets) == 0) {
      showNotification("No datasets found in CODAP.", type = "warning", duration = 3)
      updateSelectInput(session, "codap_dataset_select",
                        choices = c("(No datasets available)" = ""))
      return()
    }

    # Build choices from dataset list
    choices <- setNames(
      sapply(datasets, function(d) d$name),
      sapply(datasets, function(d) d$title %||% d$name)
    )

    updateSelectInput(session, "codap_dataset_select", choices = choices)
    showNotification(
      paste("Found", length(datasets), "dataset(s) in CODAP"),
      type = "message",
      duration = 3
    )
  })

  # CODAP Integration: Import selected dataset from CODAP
  observeEvent(input$import_from_codap, {
    dataset_name <- input$codap_dataset_select

    if (is.null(dataset_name) || dataset_name == "") {
      showNotification("Please select a dataset first. Click refresh to load available datasets.",
                       type = "warning")
      return()
    }

    showNotification(
      paste("Importing dataset:", dataset_name),
      type = "message",
      duration = 2
    )

    session$sendCustomMessage(
      type = "importFromCODAP",
      message = list(datasetName = dataset_name)
    )
  })

  # CODAP Integration: Handle imported data from CODAP
  observeEvent(input$codap_imported_data, {
    data <- input$codap_imported_data

    if (is.null(data$cases) || length(data$cases) == 0) {
      showNotification("No data found in the selected dataset.", type = "warning")
      return()
    }

    tryCatch({
      # Convert list of cases to data frame
      # Each case has a 'values' field containing the actual data
      case_values <- lapply(data$cases, function(case) {
        if (!is.null(case$values)) {
          return(case$values)
        } else {
          return(case)
        }
      })

      df <- bind_rows(lapply(case_values, function(row) {
        # Convert NULL to NA for proper data frame handling
        row <- lapply(row, function(x) if (is.null(x)) NA else x)
        as.data.frame(row, stringsAsFactors = FALSE)
      }))

      rv$original_data <- df
      rv$current_data <- df
      rv$current_step <- paste("Imported from CODAP:", data$datasetName)
      rv$step_history <- list()
      rv$conversation_history <- list()
      rv$awaiting_confirmation <- FALSE
      rv$pending_code <- NULL

      rv$chat_history <- list(
        list(role = "assistant",
             content = paste0("I've imported the **", data$datasetName, "** dataset from CODAP. ",
                              "It has ", nrow(df), " rows and ", ncol(df), " columns.\n\n",
                              "**Columns:** ", paste(names(df), collapse = ", "), "\n\n",
                              "What would you like to do with this data?"))
      )

      showNotification(
        paste("Successfully imported", nrow(df), "rows from CODAP"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Error processing imported data:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # CODAP Integration: Handle import errors
  observeEvent(input$codap_import_error, {
    error <- input$codap_import_error
    showNotification(
      HTML(paste0(
        "CODAP Import Error: ", error$message,
        "<br><br><strong>Tip:</strong> Make sure this app is embedded in CODAP and ",
        "that the selected dataset exists."
      )),
      type = "error",
      duration = 10
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
