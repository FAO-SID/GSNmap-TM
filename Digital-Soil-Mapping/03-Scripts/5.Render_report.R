# 1 - Open the file C:/GIT/GSNmap-TM/Digital-Soil-Mapping/Report_k.Rmd
# 2 - adjust the content
# 3 - Run the following code
path = 'C:/GIT/GSNmap-TM/Digital-Soil-Mapping/Report_k.Rmd'
output_format = 'pdf_document'
output_file = paste0("Report_", soilatt,".pdf")

rmarkdown::render(path, output_format, output_file)
