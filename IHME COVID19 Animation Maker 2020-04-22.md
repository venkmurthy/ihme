---
title: "IHME COVID19 Animation Maker"
author: "Venkatesh L. Murthy, MD, PhD"
date: 'April 22, 2020'
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

# Subdirector to store images
img.directory <- "images/"

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

# Create the subdirectories if don't exist
if(!dir.exists(data.directory)) { dir.create(data.directory) }
if(!dir.exists(img.directory)) { dir.create(img.directory) }

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

# Define variables that are ID vars
id.vars <- c("location_name","date_reported","pred.date","location")

# Grab all non ID vars
all.vars <- colnames(all.data)[colnames(all.data) %!in% id.vars]

# Subset vars into groups
lcl.vars <- all.vars[grepl("_lower",all.vars)]
est.vars <- all.vars[grepl("_mean",all.vars)]
ucl.vars <- all.vars[grepl("_upper",all.vars)]

# Subset lcl, estiamte and ucl data
lcl.data <- all.data %>% select(all_of(c(id.vars,lcl.vars))) 
est.data <- all.data %>% select(all_of(c(id.vars,est.vars)))
ucl.data <- all.data %>% select(all_of(c(id.vars,ucl.vars)))

# Clean up column names
colnames(lcl.data) <- gsub("_lower","",colnames(lcl.data))
colnames(est.data) <- gsub("_mean","",colnames(est.data))
colnames(ucl.data) <- gsub("_upper","",colnames(ucl.data))

# Convert to long 
lcl.data.long <- lcl.data %>% pivot_longer(-all_of(id.vars),values_to="lcl")
est.data.long <- est.data %>% pivot_longer(-all_of(id.vars),values_to="est")
ucl.data.long <- ucl.data %>% pivot_longer(-all_of(id.vars),values_to="ucl")

# Merge long datasets
all.data.long <- merge(lcl.data.long,est.data.long,by=c(id.vars,"name"))
all.data.long <- merge(all.data.long,ucl.data.long,by=c(id.vars,"name"))

# Clean up labels
all.data.long$name <- recode_factor(all.data.long$name,admis="Admissions",newICU="ICU Admissions",deaths="New Deaths",
                                    allbed="Bed Use",ICUbed="ICU Bed Use",InvVen="Invasive Ventilator Use",
                                    totdea="Total Deaths",bedover="Beds Needed Over Capacity",
                                    icuover="ICU Beds Needed Over Capacity",.ordered=TRUE)

# Add measured field
all.data.long$measured <- all.data.long$date_reported > all.data.long$pred.date
```

# IHME Model for 50 US States Evolved Over Time


```r
us.data.long <- all.data.long %>% filter(location_name %in% usmap::statepop$full) %>% filter(location_name!="District of Columbia") %>% filter(as.character(name) %in% c("Bed Use","ICU Bed Use","Invasive Ventilator Use","Total Deaths"))

us.plot <- ggplot(us.data.long,aes(x=date_reported,y=est,ymin=lcl,ymax=ucl,frame=pred.date,colour=name,fill=name)) +
  geom_line() +
  geom_vline(aes(xintercept=pred.date),colour="black",size=1,alpha=0.7) +
  geom_ribbon(alpha=0.2) + 
  facet_wrap(~ location_name,scales="free_y",ncol=5) +
  scale_fill_brewer(type="qual") +
  scale_colour_brewer(type="qual") +
  theme_minimal() + 
  theme(legend.position="bottom",legend.title=element_blank()) +
  labs(title="Prediction Date: {closest_state}",x="Date",y="Count") +
  transition_states(pred.date) +
  ease_aes()
anim_save(paste0(img.directory,"US.gif"),us.plot,width=1000,height=1500)
```



# IHME Model for Each State Separately Over Time


```r
state="Alabama"
tmp.var <- foreach(state=usmap::statepop$full,.packages=c("tidyverse","gganimate")) %dopar% {
  state.data.long <- all.data.long %>% filter(location_name == state) %>%
    filter(as.character(name) %in% c("Bed Use","ICU Bed Use","Invasive Ventilator Use","Total Deaths"))

  state.plot <- ggplot(state.data.long,aes(x=date_reported,y=est,ymin=lcl,ymax=ucl,frame=pred.date,colour=name,fill=name)) +
    geom_line() +
    geom_vline(aes(xintercept=pred.date),colour="grey",size=1,alpha=0.1) +
    geom_ribbon(alpha=0.2) + 
    facet_grid(~ location_name,scales="free_y") +
    scale_fill_brewer(type="qual") +
    scale_colour_brewer(type="qual") +
    theme_minimal() + 
    theme(legend.position="bottom",legend.title=element_blank()) +
    labs(title="Prediction Date: {closest_state}",x="Date",y="Count") +
    transition_states(pred.date) +
    ease_aes()
  anim_save(paste0(img.directory,tolower(state),".gif"),state.plot,width=450,height=300)
}

if(!is.null((tmp.var))) { tmp.var }
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
## 
## [[3]]
## NULL
## 
## [[4]]
## NULL
## 
## [[5]]
## NULL
## 
## [[6]]
## NULL
## 
## [[7]]
## NULL
## 
## [[8]]
## NULL
## 
## [[9]]
## NULL
## 
## [[10]]
## NULL
## 
## [[11]]
## NULL
## 
## [[12]]
## NULL
## 
## [[13]]
## NULL
## 
## [[14]]
## NULL
## 
## [[15]]
## NULL
## 
## [[16]]
## NULL
## 
## [[17]]
## NULL
## 
## [[18]]
## NULL
## 
## [[19]]
## NULL
## 
## [[20]]
## NULL
## 
## [[21]]
## NULL
## 
## [[22]]
## NULL
## 
## [[23]]
## NULL
## 
## [[24]]
## NULL
## 
## [[25]]
## NULL
## 
## [[26]]
## NULL
## 
## [[27]]
## NULL
## 
## [[28]]
## NULL
## 
## [[29]]
## NULL
## 
## [[30]]
## NULL
## 
## [[31]]
## NULL
## 
## [[32]]
## NULL
## 
## [[33]]
## NULL
## 
## [[34]]
## NULL
## 
## [[35]]
## NULL
## 
## [[36]]
## NULL
## 
## [[37]]
## NULL
## 
## [[38]]
## NULL
## 
## [[39]]
## NULL
## 
## [[40]]
## NULL
## 
## [[41]]
## NULL
## 
## [[42]]
## NULL
## 
## [[43]]
## NULL
## 
## [[44]]
## NULL
## 
## [[45]]
## NULL
## 
## [[46]]
## NULL
## 
## [[47]]
## NULL
## 
## [[48]]
## NULL
## 
## [[49]]
## NULL
## 
## [[50]]
## NULL
## 
## [[51]]
## NULL
```

