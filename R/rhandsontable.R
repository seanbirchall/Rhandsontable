rhandsontable <- function(data, colHeaders = NULL, rowHeaders = TRUE,
                          width = NULL, height = NULL, elementId = NULL,
                          menuItems = NULL) {
  # Convert data to a list if it's a data frame
  if(is.data.frame(data)){
    # no headers
    if(is.null(colHeaders)){
      colHeaders <- excel_headers(n_cols = ncol(data))
    }
    data <- lapply(1:nrow(data), function(i) as.list(data[i,]))
  }

  # Default menu items if none provided
  if(is.null(menuItems)) {
    menuItems <- list(
      reorder = list(
        name = "Reorder Columns",
        submenu = list(
          list(
            key = "reorder:asc",
            name = "Ascending Order",
            value = "reorder:asc"
          ),
          list(
            key = "reorder:desc",
            name = "Descending Order",
            value = "reorder:desc"
          ),
          list(
            name = "---------"
          ),
          list(
            key = "reorder:manual",
            name = "✚",
            value = "reorder:manual"
          )
        )
      ),
      separator = list(
        name = "---------"
      ),
      rename = list(
        name = "Rename",
        value = "rename"
      )
    )
  }

  # Forward options using x
  x = list(
    data = data,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    menuItems = menuItems
  )

  # Create widget
  htmlwidgets::createWidget(
    name = 'rhandsontable',
    x,
    width = width,
    height = height,
    package = 'rhandsontable',
    elementId = elementId
  )
}

#' Shiny bindings for rhandsontable
#' @export
rHandsontableOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rhandsontable', width, height,
                                 package = 'rhandsontable')
}

#' @export
renderRHandsontable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, rHandsontableOutput, env, quoted = TRUE)
}

#' @export
handsontableToDF <- function(data) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, rHandsontableOutput, env, quoted = TRUE)
}

# Basic style functions
style_cell <- function(hot, row, col, style = list()) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  # Initialize or get existing style information
  if (is.null(hot$x$cellStyles)) hot$x$cellStyles <- list()

  # Create style entry
  style_entry <- list(
    row = row,
    col = col,
    style = style
  )

  hot$x$cellStyles <- c(hot$x$cellStyles, list(style_entry))
  hot
}

# Column styling
style_cols <- function(hot, cols, style = list()) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  if (is.null(hot$x$colStyles)) hot$x$colStyles <- list()

  # Create style entry
  style_entry <- list(
    cols = cols,  # Keep original column names/indices
    style = style
  )

  hot$x$colStyles <- c(hot$x$colStyles, list(style_entry))
  hot
}

# Row styling
style_rows <- function(hot, rows, style = list()) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  if (is.null(hot$x$rowStyles)) hot$x$rowStyles <- list()

  style_entry <- list(
    rows = rows,
    style = style
  )

  hot$x$rowStyles <- c(hot$x$rowStyles, list(style_entry))
  hot
}

readonly_cell <- function(hot, row, col, readonly = TRUE) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  if (is.null(hot$x$readOnlyCells)) hot$x$readOnlyCells <- list()

  readonly_entry <- list(
    row = row,
    col = col,
    readonly = readonly
  )

  hot$x$readOnlyCells <- c(hot$x$readOnlyCells, list(readonly_entry))
  hot
}

readonly_cols <- function(hot, cols, readonly = TRUE) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  if (is.null(hot$x$readOnlyCols)) hot$x$readOnlyCols <- list()

  readonly_entry <- list(
    cols = cols,
    readonly = readonly
  )

  hot$x$readOnlyCols <- c(hot$x$readOnlyCols, list(readonly_entry))
  hot
}

readonly_rows <- function(hot, rows, readonly = TRUE) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  if (is.null(hot$x$readOnlyRows)) hot$x$readOnlyRows <- list()

  readonly_entry <- list(
    rows = rows,
    readonly = readonly
  )

  hot$x$readOnlyRows <- c(hot$x$readOnlyRows, list(readonly_entry))
  hot
}

excel_headers <- function(n_cols) {
  # Helper function to convert number to Excel column name
  num_to_excel_col <- function(n) {
    result <- ""
    while (n > 0) {
      n <- n - 1  # Adjust for 0-based indexing
      remainder <- n %% 26
      result <- paste0(LETTERS[remainder + 1], result)
      n <- n %/% 26
    }
    return(result)
  }

  # Generate headers for specified number of columns
  headers <- sapply(seq_len(n_cols), num_to_excel_col)

  return(headers)
}

# Function to set up nested headers
nested_headers <- function(hot, headers, collapsible = FALSE) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  # Set the nested headers configuration
  hot$x$nestedHeaders <- headers

  # Handle collapsible option
  if (is.logical(collapsible) && collapsible) {
    # If TRUE, make all nested header groups collapsible
    collapsible_config <- lapply(seq_along(headers[[1]]), function(i) {
      if (is.list(headers[[1]][[i]]) && !is.null(headers[[1]][[i]]$colspan)) {
        list(row = -2, col = i, collapsible = TRUE)
      }
    })
    # Remove NULL entries
    collapsible_config <- Filter(Negate(is.null), collapsible_config)
    hot$x$collapsibleColumns <- collapsible_config
  } else if (is.list(collapsible)) {
    # Use custom collapsible configuration
    hot$x$collapsibleColumns <- collapsible
  }

  hot
}

# Function to add collapsible columns
collapsible_cols <- function(hot, collapsible = TRUE) {
  if (!"rhandsontable" %in% class(hot)) {
    stop("hot must be a rhandsontable object")
  }

  if (is.logical(collapsible)) {
    hot$x$collapsibleColumns <- collapsible
  } else if (is.list(collapsible)) {
    hot$x$collapsibleColumns <- collapsible
  }

  hot
}
