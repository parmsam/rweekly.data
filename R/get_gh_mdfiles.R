#' @title Get list of markdown file Github URLs from Repo
#' @description get_gh_mdfiles() retrieves sourceable R code URLs from a Github repo
#' @param repo_name Github repository name with org or username included
#' @param branch_name Github repository brach name
#' @param filter_pattern regex pattern to filter file list by, '\\.md$' by default for markdown files
#' @return character vector of Github URLs
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # get_gh_mdfiles("rweekly/rweekly.org", "gh-pages")
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{GET}}, \code{\link[httr]{stop_for_status}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_subset}}, \code{\link[stringr]{str_replace}}
#' @rdname get_gh_mdfiles
#' @export
#' @importFrom httr GET stop_for_status content
#' @importFrom glue glue
#' @importFrom stringr str_subset
get_gh_mdfiles <- function(
    repo_name,
    branch_name,
    filter_pattern = "\\.md$"
){
  # get file list from repo of interest
  req <- httr::GET(
    glue::glue(
      "https://api.github.com/repos/{repo_name}/git/trees/{branch_name}?recursive=1"
    )
  )
  httr::stop_for_status(req)
  filelist <- unlist(
    lapply(
      httr::content(req)$tree, "[", "path"),
    use.names = F
  )
  # subset to code files
  fnames <- filelist %>%
    stringr::str_subset(filter_pattern)
  # crate full URLs
  ref_files <- glue::glue(
    "https://raw.githubusercontent.com/{repo_name}/{branch_name}/{fnames}"
  )
  ref_files <- as.vector(ref_files)
  return(ref_files)
}
