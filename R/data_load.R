#' Load Markdown Files from the RWeekly GitHub Repository
#'
#' This function fetches markdown files from a specified GitHub repository and branch,
#' filters out files based on given patterns, processes the files to extract YAML
#' metadata and content, and then reshapes the data into a wider format.
#'
#' @param repo_name Character. Name of the GitHub repository in the format "user/repo".
#'   Default is "rweekly/rweekly.org".
#' @param branch_name Character. Name of the branch in the GitHub repository to fetch
#'   the files from. Default is "gh-pages".
#' @param skip_pattern Character. A regular expression pattern specifying which files 
#'   to exclude based on their names. Default is "2016|2017", which excludes files 
#'   with names containing "2016" or "2017".
#'
#' @return A tibble with YAML metadata and content from the fetched markdown files, 
#'   where each section of content is a separate column.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- data_load()
#'   head(data)
#' }
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom janitor clean_names
data_load <- function(
    repo_name = "rweekly/rweekly.org", 
    branch_name = "gh-pages", 
    skip_pattern = "2016|2017") {
  # Fetch the markdown files from the specified GitHub repo and branch
  md_files <- get_gh_mdfiles(repo_name, branch_name) 
  # Filter out unwanted files
  filtered_files <- md_files %>%
    .[grepl("_posts", .)] %>%
    .[!grepl(skip_pattern, .)]
  # Convert to a list and then to a data frame
  converted_tbl <- filtered_files %>%
    purrr::map(read_md_to_list) %>% 
    purrr::map_df(~ tibble::tibble(yaml = list(.x$yaml), content = list(.x$content)))
  # Split sections into columns
  unnested_tbl <- converted_tbl %>% 
    tidyr::unnest_wider(yaml) %>%
    tidyr::unnest_wider(content)
  # Clean names
  cleaned_tbl <- unnested_tbl %>% 
    janitor::clean_names(allow_dupes = F)
  return(cleaned_tbl)
}
