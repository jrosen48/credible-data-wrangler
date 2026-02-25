# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**CREDIBLE Data Wrangler** is an educational Shiny application designed for K-12 students to learn data preparation and cleaning techniques. The app provides an interactive chat interface powered by Claude AI to guide students through common data wrangling operations: pivoting, grouping/summarizing, and joining datasets.

## Architecture

### Single-File Shiny App
The entire application is contained in `app.R` (583 lines), combining UI and server logic in one file. This follows the standard single-file Shiny app pattern.

**Key Components:**
- **UI (lines 19-225)**: Sidebar layout with file upload, chat interface, data preview table, and help/history tabs
- **Server (lines 228-579)**: Reactive values manage state; Claude API integration handles conversational AI; code execution happens in a controlled server environment

### State Management via reactiveValues
The `rv` reactive values object (lines 233-244) stores the complete application state:
- `current_data` and `original_data`: Data frames at current and initial states
- `chat_history`: User-facing chat messages (displayed in UI)
- `conversation_history`: Full conversation sent to Claude API (includes system context)
- `step_history`: List of completed transformations with code and timestamps
- `awaiting_confirmation`: Boolean flag for pending code execution
- `pending_code`, `pending_step_name`, `pending_step_description`: Temporary storage for transformations awaiting user approval

### Claude API Integration
The `call_claude()` function (lines 307-386) manages API communication:
- Uses Claude Sonnet 4.5 model (`claude-sonnet-4-5-20250929`)
- System prompt instructs Claude to provide educational explanations and generate tidyverse R code
- Code must be wrapped in ```r ``` fenced blocks for extraction
- Data context (rows, columns, types) is automatically injected into each API call
- Conversation history is maintained for multi-turn interactions

### Code Execution Flow
1. User sends message via chat (line 389)
2. If awaiting confirmation: Check for yes/no response and execute or cancel pending code
3. Otherwise: Send message to Claude API
4. Extract R code from response using regex pattern (lines 454-466)
5. If code found: Set `awaiting_confirmation = TRUE` and prompt user
6. User confirms: Execute code with `eval(parse(text = rv$pending_code), envir = server_env)` (line 397)
7. Update `current_data`, add to `step_history`, reset confirmation state

**CRITICAL**: All generated code must start with `rv$current_data %>%` and return a data frame/tibble. Code is evaluated in the `server_env` environment which has access to all reactive values.

### Data Loading
Two entry points:
- **File upload** (lines 247-271): Supports CSV, XLSX, TSV, TXT via `readr` and `readxl`
- **Example dataset** (lines 274-304): Loads `plant_growth_experiment.csv`, a pre-built messy dataset for demonstration

## Key Files

- `app.R`: Main Shiny application (UI + server)
- `sample_student_data.R`: Script that generates example CSV datasets with intentional messiness
- `plant_growth_experiment.csv`: Example dataset loaded by the "Try Example Dataset" button
- `.Renviron`: Must contain `CLAUDE_API_KEY` environment variable (gitignored)
- `rsconnect/`: Deployment configuration for shinyapps.io

## Running the Application

### Local Development
```r
# Install dependencies
install.packages(c("shiny", "tidyverse", "DT", "httr", "jsonlite", "readxl", "stringr", "markdown", "skimr"))

# Set up API key in .Renviron file
CLAUDE_API_KEY=your_api_key_here

# Run the app from R console or RStudio
shiny::runApp("app.R")
```

### Deployment
The app is configured for deployment to shinyapps.io (see `rsconnect/shinyapps.io/ed-analytics/credible-data-wrangler.dcf`). The `CLAUDE_API_KEY` must be set in the deployment environment variables.

## Development Patterns

### Adding New Data Operations
To extend the app with new data wrangling capabilities:
1. Update the system prompt in `call_claude()` (lines 332-350) to describe the new operation
2. Ensure Claude generates code compatible with the `rv$current_data %>%` pattern
3. No changes needed to execution logic - it automatically handles any tidyverse transformation

### Modifying UI/Styling
All CSS is inline in the `tags$head()` section (lines 22-130). The design uses:
- Gradient backgrounds for chat messages and buttons
- Custom `.chat-container` with scrolling
- `.step-card` styling for step history
- Prompt suggestions as clickable `.prompt-suggestion` elements

### Debugging Code Execution Errors
When user-confirmed code fails (lines 422-433):
- Error message shows the attempted code and error details
- Conversation continues - user can request alternative approaches
- `current_data` remains unchanged (rollback is automatic)

## R Project Configuration

Standard RStudio project settings in `credible-data-wrangler.Rproj`:
- 2-space indentation
- UTF-8 encoding
- Code indexing enabled

## Security Considerations

- API key is stored in environment variable, never committed to git
- User-provided code is NOT executed - only Claude-generated code runs
- Code execution is sandboxed to `server_env` environment
- File uploads are processed with standard Shiny security (temporary files, automatic cleanup)
