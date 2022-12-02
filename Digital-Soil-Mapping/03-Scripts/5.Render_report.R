# 1 - Open the file C:/GIT/GSNmap-TM/Digital-Soil-Mapping/Report_k.Rmd
# 2 - adjust the content
# 3 - Run the following code
wd <-'C:/Users/luottoi/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping/National Report' 
setwd(wd)

ISO <- 'AOI'


path = 'National GSNmap Report.Rmd'
output_format = 'word_document'
output_file = paste0("Report_GSNmap_",ISO,".docx")

rmarkdown::render(path, output_format, output_file)
