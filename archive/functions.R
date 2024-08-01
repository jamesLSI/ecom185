
namesFunction <- function(nms) {
  janitor::make_clean_names(nms, case = "upper_camel")}

clipboard_it <- function (data) 
{
  write.table(data, "clipboard", sep = "\t", row.names = FALSE)
}