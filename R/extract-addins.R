

#' Extract TODOs Addin
#'
#' Currently a non-interactive addin to insert all TODOs from the ORG that
#' the currently active file is in.
#'
#' @export
addin_extract_todos <- function() {

  #cat( "\nprojectmanagr::addin_insert_section_sep_1():\n" )

  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()

  # get orgPath
  orgPath <- find_org_directory(context$path)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", context$path) )
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # extract todos char vector
  todos_vector <- extract_todos(orgPath)


  #### insert SEP value in active doc ####

  rstudioapi::insertText(todos_vector)

}

