file="~/Documents/shiny_project/2014_Single_Family_Census_Tract_File.pdf"
PDFtoDF = function(file) {
  ## Extract PDF text. Each line of PDF becomes one element of the string vector dat.
  dat = readPDF(control=list(text="-layout"))(elem=list(uri=file), 
                                              language="en", id="id1") 
  dat = c(as.character(dat))
  
  ## Keep only those strings that contain the data we want. 
  ## These are the ones that begin with a number.
  dat = dat[grep("^ {0,2}[0-9]{1,3}", dat)]
  
  ## Create separators so we can turn strings into a data frame. We'll use the 
  ## pipe "|" as a separator.
  
  # Add pipe after first number (the row number in the PDF file)
  dat = gsub("^ ?([0-9]{1,3}) ?", "\\1|", dat)
  
  # Replace each instance of 2 or more spaces in a row with a pipe separator. This 
  # works because the company names have a single space between words, while data
  # fields generally have more than one space between them. 
  # (We just need to first add an extra space in a few cases where there's only one
  # space between two data fields.)
  dat = gsub("(, HVOL )","\\1 ", dat)
  dat = gsub(" {2,100}", "|", dat)
  
  ## Check for data format problems
  # Identify rows without the right number of fields (there should 
  # be six pipe characters per row) and save them to a file for 
  # later inspection and processing (in this case row 11 of the PDF file is excluded))
  excludeRows = lapply(gregexpr("\\|", dat), function(x) length(x)) != 6
  write(dat[excludeRows], "rowsToCheck.txt", append=TRUE)
  
  # Remove the excluded rows from the string vector
  dat = dat[!excludeRows]
  
  ## Convert string vector to data frame 
  dat = read.table(text=dat, sep="|", quote="", stringsAsFactors=FALSE)
  names(dat) = c("RowNum", "Reference Entity", "Sub-Index", "CLIP", 
                 "Reference Obligation", "CUSIP/ISIN", "Weighting")
  return(dat)
}

# Create vector of names of files to read
files = list.files(pattern="CDX.*\\.pdf")

# Read each file, convert it to a data frame, then rbind into single data frame
df = do.call("rbind", lapply(files, PDFtoDF))

# Sample of data frame output from your sample file
df