---
title: "IHME COVID19 Animation"
author: "Venkatesh L. Murthy, MD, PhD"
date: 'April 14, 2020'
output:
  html_document: 
    code_folding: hide
    number_sections: yes
    theme: flatly
    toc: yes
    toc_float: yes
    keep_md: yes
knit: (function(inputFile, encoding) { rmarkdown::render(inputFile, encoding = encoding, output_file = paste0(substr(inputFile,1,nchar(inputFile)-4)," ",Sys.Date(),'.html')) })
---




```r
# URL for main IHME download site
ihme.url <- "http://www.healthdata.org/covid/data-downloads"

# Subdirectory to store IHME archive data
data.directory <- "source_data/"

# Download the html page
ihme.page <- read_html(ihme.url)

# Get the links and filter to the ones that are latest or archive datasets
ihme.links <- data.frame(description=ihme.page %>% html_nodes("a") %>% html_text(),
                         url=ihme.page %>% html_nodes("a") %>% html_attr("href")) %>% 
  filter(grepl("latest|archive",url)) %>% mutate_if(is.factor,as.character)

# Clean up descriptions
ihme.links$description[grepl("latest",ihme.links$url)] <- "latest"

# Add target file names
ihme.links$file <- paste0(data.directory,ihme.links$description,".zip")

# Create the download subdirectory if it doesn't exist
if(!dir.exists(data.directory)) { dir.create(data.directory) }

# Download all files
ihme.links$zip.path <- NA

for (i in 1:dim(ihme.links)[1]) {
  download.file(ihme.links$url[i],ihme.links$file[i],quiet=TRUE)
  zip.contents <- unzip(ihme.links$file[i],list=TRUE)
  ihme.links$zip.path[i] <- zip.contents$Name[grepl("\\.csv",zip.contents$Name)]
}

# Find date variables
ihme.links$date <- coalesce(as.Date(ihme.links$description,"%B %d, %Y"), 
                            as.Date(substr(ihme.links$zip.path,1,10),"%Y_%m_%d"))

# Unzip and merge all data
all.data <- foreach(i=1:dim(ihme.links)[1],.combine=bind_rows) %do% {
  pred.data <- read.csv(unz(ihme.links$file[i],ihme.links$zip.path[i]))
  pred.data$pred.date <- ihme.links$date[i]
  
  colnames(pred.data)[colnames(pred.data)=="date"] <- "date_reported"
  
  pred.data  %>% mutate_if(is.factor,as.character)
}

# Remove unnecessary/junk columns
all.data <- all.data %>% select(!c("V1","X"))

# Convert date
all.data$date_reported <- as.Date(all.data$date_reported,"%Y-%m-%d")
```


## Test Plots


```r
mi.data <- all.data %>% filter(location_name=="New York")

mi.plot <- ggplot(mi.data,aes(x=date_reported,y=allbed_mean,colour=as.character(pred.date))) +
  geom_line()
ggplotly(mi.plot + transition_states(pred.date))
```

