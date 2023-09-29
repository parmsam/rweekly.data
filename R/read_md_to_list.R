#' Read and Parse Markdown File Content
#'
#' This function reads the content of a markdown file, extracts the YAML metadata
#' and sections denoted by "###" headers, and returns them in a list format.
#'
#' @param file_path Character string representing the path to the markdown file.
#'
#' @return A list containing two named elements:
#' \itemize{
#'   \item \code{yaml}: A list containing the parsed YAML metadata, where each name-value pair corresponds to an entry in the YAML.
#'   \item \code{content}: A list of sections from the markdown content, where the names of the list elements are the section headers and the values are lists of the lines (content) belonging to those sections.
#' }
#' 
#' @examples
#' \dontrun{
#'   md_data <- read_md_to_list("https://raw.githubusercontent.com/rweekly/rweekly.org/gh-pages/_posts/2023-09-25-2023-W39.md")
#'   print(md_data$yaml)
#'   print(md_data$content$'Section Name')
#' }
#'
#' @export
read_md_to_list <- function(file_path) {
  # Read the content of the markdown file
  lines <- readLines(file_path)
  
  # Initialize empty lists for each section
  yaml <- list()
  sections <- list()
  current_section <- NULL
  current_section_name <- NULL
  
  # Parsing YAML data
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]
  for (i in (yaml_start + 1):(yaml_end - 1)) {
    split_line <- strsplit(lines[i], ": ")[[1]]
    yaml[trimws(split_line[1])] <- trimws(split_line[2])
  }
  
  # Parse the rest of the markdown content
  for (i in (yaml_end + 1):length(lines)) {
    line <- lines[i]
    
    # Check for headers (sections)
    if (grepl("^### ", line)) {
      if (!is.null(current_section_name)) {
        sections[[current_section_name]] <- current_section
      }
      current_section_name <- sub("^### ", "", line)
      current_section <- list()
    } else if (grepl("^[+|-] ", line) | grepl("^!\\[.*\\]\\(.*\\)", line)) {
      # Check for list items or images
      # Seperate name from URL in bullet item
      line <- strsplit(line, "]\\(")[[1]]
      line[1] <- sub("^[+|-|\\*] +\\[", "", line[1])
      line[2] <- sub("\\)$", "", line[2])
      # Name elements of list
      names(line) <- c("name", "url")
      line <- list(line)
      current_section <- c(current_section, line)
    }
  }
  
  # Add the last section to the sections list
  if (!is.null(current_section_name)) {
    sections[[current_section_name]] <- current_section
  }
  
  # Combine YAML and sections data
  result <- list(yaml = yaml, content = sections)
  
  return(result)
}

