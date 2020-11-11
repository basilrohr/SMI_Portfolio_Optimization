# This function assigns a list output of a function to independent variables.
# Therefore, instead of calling a variable via "output$variable",
# it can be called with "variable" directly.

assign_list_output = function(output) {
  # Get names of list elements
  names = names(output)
  # Iterate through list elements and assign list element to independent variable
  # with the same name globally
  for (i in 1:length(names)) {
    assign(names[i], (output[i])[[1]], envir = .GlobalEnv)
  }
}
