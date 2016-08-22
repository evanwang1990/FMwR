control_assign <- function(default_controls, assign_list)
{
  if (length(assign_list) == 0) {
    return(list(contr = default_controls, is.default = TRUE))
  }

  default_param_names <- names(default_controls)
  to_assign_names <- tolower(names(assign_list))
  names(assign_list) <- to_assign_names
  num_diff <- 0
  for (name_ in to_assign_names)
  {
    if (name_ %in% default_param_names) {
      tmp_value <- assign_list[[name_]]
      class_default <- class(default_controls[[name_]])

      if (class_default == "integer") {
        tmp_value <- as.integer(tmp_value)
        if (tmp_value != assign_list[[name_]]) {
          tmp_value <- max(default_controls[[name_]], tmp_value)
          warning(name_, " is not integer, it will be set as ", tmp_value, "\n\n")
        }
      } else if (class_default == "logical") {
        stopifnot(is.logical(tmp_value))
      }

      default_controls[[name_]] <- tmp_value
      num_diff <- num_diff + 1
    }
  }

  if (num_diff != length(assign_list)) {
    warning("some arguments are unknown...\n\n", immediate. = TRUE)
  }

  list(contr = default_controls, is.default = FALSE)
}
