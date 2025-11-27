library(knitr)
library(bibtex)

# Load the .bib file
bib_file <- "inst/citations.bib"  # Replace with your .bib file path
bib <- read.bib(bib_file)

# Helper function to format authors
format_authors <- function(authors) {
  formatted_authors <- sapply(authors, function(author) {
    last_name <- author$family
    initials <- paste(paste0(substr(author$given, 1, 1), "."), collapse = " ")
    paste(last_name, initials, collapse = " ")
  })
  paste(formatted_authors, collapse = ", ")
}

# Sort the entries: Newest year first, then by the first author's last name alphabetically
sorted_bib <- bib[order(
  -as.numeric(sapply(bib, function(entry) entry$year)),  # Sort by year (descending)
  sapply(bib, function(entry) entry$author[[1]]$family) # Then by the first author's last name
)]

# Create a Markdown file
output_file <- "inst/citations.md"
cat("# Citations\n\n", file = output_file)

# Loop through sorted entries to create a numbered list
for (i in seq_along(sorted_bib)) {
  entry <- sorted_bib[[i]]
  
  # Format authors
  authors <- format_authors(entry$author)
  
  # Clean the title by removing curly brackets
  clean_title <- gsub("\\{|\\}", "", entry$title)
  
  # Check if the entry has a DOI
  doi <- if (!is.null(entry$doi)) {
    sprintf(" DOI: [%s](https://doi.org/%s)", entry$doi, entry$doi)
  } else {
    ""
  }
  
  # Write the entry to the Markdown file as a numbered list item
  cat(sprintf("%d. %s. %s. *%s*. **%s**.%s\n\n",
              i,               # Number
              authors,         # Unbolded authors
              clean_title,     # Cleaned title
              entry$journal,   # Journal or booktitle
              entry$year,      # Bolded year
              doi),            # DOI (if available)
      file = output_file, append = TRUE)
}

cat("\n", file = output_file, append = TRUE)
