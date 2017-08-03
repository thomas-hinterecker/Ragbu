
# Make Times New Roman font usable
if (Sys.info()["sysname"] == "Windows") {
  windowsFonts(RMN=windowsFont("Times New Roman"))
  #' @export RMN
  RMN <- "RMN"
} else {
  #' @export RMN
  RMN <- "Times New Roman"
}
