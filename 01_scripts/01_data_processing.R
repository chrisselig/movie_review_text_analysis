#################################################################################
# Script is used to house some data processing scripts for a tidytext analysis
#   
# 
#
# Script created by Chris Selig Apr 2020 of BIDAMIA Inc
#
#################################################################################


# Load Data ----
# Criticker Ratings
tyler_reviews_raw <- as.data.table(XML::xmlToDataFrame('../00_data/tyler_ratings.xml'))
justin_reviews_raw <- as.data.table(XML::xmlToDataFrame('../00_data/justin_ratings.xml'))
zach_reviews_raw <- as.data.table(XML::xmlToDataFrame('../00_data/zach_ratings.xml'))

# Clean Reviews Data ----

# Add reviewer column to each data set
tyler_reviews_raw[, reviewer := 'Tyler']
justin_reviews_raw[,reviewer := 'Justin']
zach_reviews_raw[,reviewer := 'Zach']

# Combine reviews into single table
review_list <- list(tyler_reviews_raw, justin_reviews_raw, zach_reviews_raw)
reviews <- rbindlist(review_list)

# Remove unwated columns
reviews <- reviews[,c(1:8,10:11) :=NULL]

# Remove unwanted rows
reviews <- reviews[!is.na(quote)]
reviews <- reviews[quote != '']

# Final processing - tokenization ----
tidy_reviews <- reviews %>% 
    unnest_tokens(word, quote) %>% 
    # Remove stop words
    anti_join(stop_words)

rm(justin_reviews_raw,tyler_reviews_raw,zach_reviews_raw,reviews_list)
