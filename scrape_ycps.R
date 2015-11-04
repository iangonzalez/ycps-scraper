# script to scrape class data from ycps website
setwd("~/programming/R_scripts/classes")
library(RCurl)
library(stringr)

index_page <- getURL("http://catalog.yale.edu/ycps/subjects-of-instruction/")
index_page <- strsplit(index_page, split="\n", fixed=TRUE)[[1]]
index_page <- index_page[168:length(index_page)]

#getting and trimming subject list
subject_links <- index_page[grep("<a href=", index_page, fixed=TRUE)]
subjects <- unname(sapply(subject_links, function(x) str_extract(x, "\"(.*)\"")))
subjects <- gsub("\"", "", subjects, fixed=TRUE)
subjects <- unique(subjects[1:130])
subjects <- subjects[!grepl("/", subjects, fixed=TRUE)]

# main loop: 
raw_info <- lapply(subjects, function(subj) {
  subj_page <- getURL(sprintf("http://catalog.yale.edu/ycps/subjects-of-instruction/%s/", subj))
  subj_page <- strsplit(subj_page, split="\n", fixed=TRUE)[[1]]
  
  class_line_idx <- grep("courseblocktitle", subj_page, fixed=TRUE)
  class_lines <- subj_page[class_line_idx]
  class_lines <- gsub("&#160;", " ", class_lines, fixed=TRUE)
  class_lines <- gsub("&amp;", "&", class_lines, fixed=TRUE)
  
  time_line_idx <- class_line_idx + 4
  time_lines <- subj_page[time_line_idx]
  times <- gsub("(.*)<br/>|</p>", "", time_lines)
  times[!grepl("HTBA|[0-9]+.*-[0-9]", times)] <- "not listed"
  
  dists <- unname(sapply(time_lines, function(x) str_extract(x, "HU|SC|WR|SO|QR|L[1-5]")))
  
  terms <- sapply(class_lines, function(x) {
    if (grepl("a or b", x)) {
      "both"
    } else if (grepl("[0-9]a", x)) {
      "fall"
    } else if (grepl("[0-9]b", x)) {
      "spring"
    } else {
      "not specified"
    }
  })
  
  full_titles <- unname(sapply(class_lines, function(x) str_extract(x, ", (.*)$")))
  
  titles <- unname(sapply(class_lines, function(x) str_extract(x, "[A-Z|&]+ [0-9]+")))
  list(titles, times, dists, terms, full_titles)
})

titles <- lapply(raw_info, function(x) x[[1]])
times <- lapply(raw_info, function(x) x[[2]])
dists <- lapply(raw_info, function(x) x[[3]])
terms <- lapply(raw_info, function(x) x[[4]])
full_titles <- lapply(raw_info, function(x) x[[5]])

dists <- unlist(dists)
dists[is.na(dists)] <- "not specified"

full_titles <- unlist(full_titles)
full_titles <- gsub(", ", "", full_titles)


num_courses <- length(unlist(titles))
# number of courses found: 2585
course.data <- data.frame(dept=character(num_courses),
                          course_num=character(num_courses),
                          title=character(num_courses),
                          time=character(num_courses),
                          dist=character(num_courses),
                          semester=character(num_courses))


course.data$course_num <- unlist(titles)
course.data$dept <- rep(subjects, sapply(titles, length))
course.data$time <- unlist(times)
course.data$semester <- unlist(terms)
course.data$dist <- dists
course.data$title <- full_titles


write.csv(course.data, "Course_Info_2015.csv")