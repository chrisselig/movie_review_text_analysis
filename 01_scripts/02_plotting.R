#################################################################################
# Script is used to house some plotting function for a tidytext analysis
#   
# 
#
# Script created by Chris Selig Apr 2020 of BIDAMIA Inc
#
#################################################################################

# Bar Charts ----
bar_charts_func <- function(data, 
                            facet = FALSE,
                            facet_variable = NA,
                            label,
                            title,
                            subtitle,
                            xlabel,
                            ylabel,
                            hjust = 1.25,
                            size = 3){
    
    p <- data %>% 
        ggplot(aes(n,reorder(word,n))) +
        # Geoms
        geom_bar(stat = 'identity',aes(fill = n), fill = '#A2AAB0')
    
    if(facet){
        p <- p + facet_wrap(~reviewer,scales = "free")
    }
    
    p <- p +
        geom_text(aes(label = label_text), size = size, hjust = hjust, color = 'white') +
        tidytext::scale_y_reordered() +
        # Formatting
        #scale_fill_gradient2(low='white', mid='#A2AAB0', high='#3E3E3B') +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlabel,
            y = ylabel
        ) +
        theme(
            plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
            plot.subtitle = element_text(hjust = 0.01, size = 11,family = "ArnoProLightDisplay"),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none"
        )
    return(p)
}

# Network graph ----
network_graph_func <- function(data = bigram_counts,
                               reviewer = 'Justin',
                               num = 4,
                               title = 'title',
                               subtitle = 'subtitle'){
    
    # Transform data to proper format
    data %>% 
        filter(reviewer == !!reviewer) %>% 
        ungroup() %>% 
        select(-reviewer) %>% 
        filter(n > num) %>% 
        graph_from_data_frame() %>% 
        
     # Create the network graph  
        ggraph(layout = 'fr') +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       end_cap = circle(.07,'inches')) +
        geom_node_point(color = '#CBC5C1', size = 3) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1,family = "ArnoProLightDisplay") +
        labs(
            title = title,
            subtitle = subtitle
        ) +
        theme(
            plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
            plot.subtitle = element_text(hjust = 0.01, size = 11,family = "ArnoProLightDisplay"),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none"
        )
}

# Histogram ----
histogram_function <- function(data = sentence_sentiment,
                          reviewer_name = 'Tyler',
                          bin_num = 30){
    
    data %>% 
        filter(reviewer == reviewer_name) %>% 
        ggplot(aes(ave_sentiment)) +
        geom_histogram(bins = bin_num, fill = '#CBC5C1') +
        # Formatting
        labs(
            title = str_glue("{reviewer_name}'s Movie Sentiment Distribution"),
            x = 'Average Sentiment',
            y = ''
        ) +
        theme(
            plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
            plot.subtitle = element_text(hjust = 0.01, size = 11,family = "ArnoProLightDisplay"),
            axis.ticks.x=element_blank(),
            legend.position = "none"
        )
    
}

# Scatterplot ----
scatterplot_function <- function(data = sentence_sentiment,
                                 reviewer_name){
    
    data %>% 
        filter(reviewer == reviewer_name) %>% 
        left_join(reviews_with_movies_ratings, by = c('reviewer' = 'reviewer','filmname' = 'filmname')) %>% 
        select(reviewer, ave_sentiment, rating) %>% 
        filter(!is.na(rating)) %>% 
        ggplot(aes(rating, ave_sentiment)) +
        geom_point(color = '#CBC5C1') +
        scale_x_discrete(breaks = breaks_pretty(n = 8)) +
        labs(
            title = str_glue("{reviewer_name}'s Average Sentiment Compared to Movie Rating"),
                             y = 'Avg Sentiment') +
                theme(
                    plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
                    plot.subtitle = element_text(hjust = 0.01, size = 11,family = "ArnoProLightDisplay"),
                    axis.ticks.x=element_blank()
                )
    
}

# Testing ----
# data <- bigram_counts
# data <- tidy_reviews %>%
#     group_by(reviewer) %>%
#     count(word) %>%
#     top_n(10, n) %>%
#     ungroup() %>%
#     mutate(word = tidytext::reorder_within(word, n,reviewer)) %>%
#     mutate(
#         prop = percent(n/sum(n),accuracy = .1),
#         label_text = str_glue('{n}\n{prop}')
#     ) %>%
#     arrange(desc(n))
# 
# bar_charts_func(data,x = word, y = n,
#                 facet = TRUE,
#                 facet_variable = reviewer,
#                 label = label_text,
#                 title = 'Most Common Reviewer Words',
#                 subtitle = 'Shows counts and proportions of the 10 most common words used by all the reviewers',
#                 xlabel = '',
#                 ylabel = '',
#                 hjust = 1.25,
#                 size = 2.15)


