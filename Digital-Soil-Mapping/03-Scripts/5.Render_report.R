# 1 - Open the file in the folder Digital-Soil-Mapping/National Report/National GSNmap Report.Rmd
# 2 - adjust the content
# 3 - Run the following code below
rm(list=ls())
gc()


#wd <-'C:/Users/luottoi/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping/National Report' 
wd <-'C:/Users/hp/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping/National Report' 


setwd(wd)

ISO <- 'AOI'


path = 'National GSNmap Report.Rmd'
output_format = 'word_document'
output_file = paste0("Report_GSNmap_",ISO,".docx")

rmarkdown::render(path, output_format, output_file)
