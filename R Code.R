
###################################################################
#     WEB SCRAPING BLACKBOARD LEARN TO CREATE DATA DICTIONARY     #
###################################################################

# Select which Schema/Release to scrape from this list https://help.blackboard.com/Learn/Administrator/Hosting/Databases/Open_Database_Schema
# I chose 9.1 Q2 2017, which brings me to http://library.blackboard.com/ref/4c77996d-4fdd-451f-afc6-7f7884f3726d/index.htm
# Trim this down to and enter the base URL for the Blackboard release 

base.url <- "http://library.blackboard.com/ref/4c77996d-4fdd-451f-afc6-7f7884f3726d/"




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# GET SCHEMA AND SCHEMA URLS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(rvest)
library(httr)
# Scrape Schemas page
schemas.pg <- read_html(paste(base.url,"frame-schemas.html", sep=""))

# Get schema names
schemas <- data.frame(schema = html_text(html_nodes(schemas.pg, "li a")), stringsAsFactors = F)

# Get Schema URLs
schemas$sch.url <- paste(base.url, 
                        html_attr(html_nodes(schemas.pg, "li a"), "href"),
                        sep="")

# Create URL for ERD images
schemas$sch.erd <- paste(base.url, "images/", schemas$schema, ".png", sep = "")
# Test ERD image URLs
schemas$test <- sapply(schemas$sch.erd, http_error)
# Remove incorrect URLs
schemas$sch.erd[schemas$test==TRUE] <- NA
# Keep only relevant columns
schemas <- schemas[,1:3]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# GET TABLES ASSOCIATED WITH EACH SCHEMA #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# FUNCTION FOR SCHEMAS: get associated table name and table URL
fun.sch <- function(sch.url) {
  
  sch <- read_html(sch.url) %>%
    html_node("h1") %>%
    html_text()
  
  table <- read_html(sch.url) %>%
    html_node("ul") %>%
    html_children() %>%
    html_text(trim=T)
  
  table <- ifelse(length(table) == 0, NA, list(table))[[1]]  #if schema has zero tables, put NA
  
  return(data.frame(sch,table, stringsAsFactors = F))
}


# Pass URLs Through Function and Combine into one data frame
s <- do.call(rbind, lapply(schemas$sch.url, fun.sch))

# Join Schema and Table data
dd <- merge(schemas,s, by.x = "schema", by.y = "sch", all = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# GET FIELDS ASSOCIATED WITH EACH TABLE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Scrape "All Tables" web page to get the URLs for each table
all.tables.pg <- read_html(paste(base.url,"frame-elements.html", sep=""))

# Create a Data Frame with Table names and Associated URLs
tbl.url <- paste(base.url, 
                 html_attr(html_nodes(all.tables.pg, "a"), "href"),
                 sep="")


# Function to go through each URL, scrape the data dictionary table, and add the table name as the first column
fun.tbl <- function(tbl.url) {
  
  tbl <- read_html(tbl.url) %>%
    html_node("h1") %>%
    html_text(trim=T)
  
  t <- read_html(tbl.url)  %>%
    html_node("table") %>%
    html_table()
  
  return(data.frame(tbl,tbl.url,t, stringsAsFactors = F))
}

# Pass URLs Through Function and Union
t <- do.call(rbind, lapply(tbl.url, fun.tbl))

# Merge Schema/Table and Field Data - reorder columns - Clean up column names - Sort
dd <- merge(dd,t, by.x = "table", by.y = "tbl", all = T)
dd <- dd[c(2,3,4,1,5,6,7,8,9,10,11,12,13)]
colnames(dd) <- c("Schema","Schema.URL","Schema.ERD.URL","Table","Table.URL","Column.name","Data.type","Default.value","Value.constraint","Default.constraint","Identity","Nullable","Description")
dd <- dd[order(dd$Schema,dd$Table),]




#~~~~~~~~~~~~~~~~~~~~~~#
# SAVE DATA DICTIONARY #
#~~~~~~~~~~~~~~~~~~~~~~#
# With ALL Fields
setwd("C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/")
save(dd, file = "DataDictionaryALL.rda")
write.csv(dd, file = "DataDictionaryALL.csv", na = "", row.names = F)


# Bare Bones - Only include Schema with an ERD - Only Essential columns
b <- subset(dd, !is.na(Schema.ERD.URL))
b <- subset(b, select = c(Schema, Table, Column.name, Data.type, Description))
setwd("C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/")
save(b, file = "DataDictionaryERDs.rda")
write.csv(b, file = "DataDictionaryERDs.csv", na = "", row.names = F)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# GET ERD IMAGES FOR EACH SCHEMA #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Include only schemas with a valid URL (determined above)
i <- schemas[complete.cases(schemas),]

# set destination filepath
destFP <- "C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/ERD Images/"

# FUNCTION FOR ERD IMAGES
fun.img <- function(sch.erd) {
  img <- download.file(url = sch.erd,
                       destfile = paste(destFP, sch.erd %>%
                                          strsplit( "/" ) %>%
                                          sapply( tail, 1 ), 
                                        sep = ""),
                       mode = "wb")
}

# APPLY FUNCTION
lapply(i$sch.erd, fun.img)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ADD SCHEMA NAME TO EACH ERD IMAGE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(magick)
# Set filepaths for images coming in and out
inpath <- "C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/ERD Images/"
outpath <- "C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/ERD Images/Annotated/"


# Function to annotate png images and add ERD name
fun.ann <- function(file) {
  title <- toupper(strsplit(file, ".png"))
  
  i <- image_read(file)
  
  height <- round(image_info(i)$height/10, digits = 0) #make annotations 10% of the height
  
  image_border(i,"white", paste("0x",height, sep="")) %>%
    image_annotate(title, size = height, location = "+5+10", color = "black") %>%
    image_write(path = paste(outpath,file,sep = ""), format = "png")
}

# Get list of images
setwd(inpath)
filenames <- list.files(pattern="*.png")

# Run function to annotate images and export
lapply(filenames, fun.ann)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# COMBINE ANNOTATED IMAGES INTO 1 PDF #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(magick)
# Set filepaths for images coming in and out
inpath <- "C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/ERD Images/Annotated/"
outputfile <- "C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Web Scraping/Blackboard Learn Data Dictionary/All ERDs.pdf"

setwd(inpath)
filenames = list.files(pattern="*.png")

# Import All Images into one file
all <- image_read(filenames)

# Convert to PDF
all_pdf <- image_convert(all,"pdf")

# Export Images as single PDF
image_write(all_pdf, path = outputfile, format = "pdf")

