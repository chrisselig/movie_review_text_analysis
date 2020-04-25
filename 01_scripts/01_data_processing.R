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
reviews_with_movies_ratings <-  rbindlist(review_list)

# Remove unwated columns
reviews_with_movies_ratings <- reviews_with_movies_ratings[,c(1:2,4:6,8,10,11) := NULL] # To be used for sentiment analysis
reviews <- reviews[,c(1:8,10:11) :=NULL] # To be used for text mining


# Remove unwanted rows
reviews <- reviews[!is.na(quote)]
reviews <- reviews[quote != '']

# Add rating buckets
reviews_with_movies_ratings <- reviews_with_movies_ratings %>% 
    mutate(rating_category = case_when(
        reviewer == 'Justin' & rating <= 15 ~ 'Horrible',
        reviewer == 'Justin' & rating <= 34 ~ 'Bad',
        reviewer == 'Justin' & rating <= 69 ~ 'Average',
        reviewer == 'Justin' & rating <= 79 ~ 'Good',
        reviewer == 'Justin' & rating <= 89 ~ 'Great',
        reviewer == 'Zach' & rating <= 24 ~ 'Horrible',
        reviewer == 'Zach' & rating <= 49 ~ 'Bad',
        reviewer == 'Zach' & rating <= 69 ~ 'Average',
        reviewer == 'Zach' & rating <= 79 ~ 'Good',
        reviewer == 'Zach' & rating <= 89 ~ 'Great',
        reviewer == 'Tyler' & rating <= 24 ~ 'Horrible',
        reviewer == 'Tyler' & rating <= 49 ~ 'Bad',
        reviewer == 'Tyler' & rating <= 69 ~ 'Average',
        reviewer == 'Tyler' & rating <= 79 ~ 'Good',
        reviewer == 'Tyler' & rating <= 89 ~ 'Great',
        TRUE ~ 'Exceptional'
    ))

# Final processing - tokenization ----
tidy_reviews <- reviews %>% 
    unnest_tokens(word, quote) %>% 
    # Remove stop words
    anti_join(stop_words)

rm(justin_reviews_raw,tyler_reviews_raw,zach_reviews_raw,reviews_list)
