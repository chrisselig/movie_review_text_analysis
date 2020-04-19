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
                       x = word,
                       y = n,
                       label,
                       title,
                       subtitle,
                       xlabel,
                       ylabel){
    
    data %>% 
        ggplot(aes(n,reorder(word,n))) +
        # Geoms
        geom_bar(stat = 'identity',aes(fill = n)) +
        geom_text(aes(label = label_text), size = 3, hjust = 1.25, color = 'white') +
        # Formatting
        scale_fill_gradient2(low='white', mid='#A2AAB0', high='#3E3E3B') +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlabel,
            y = ylabel
        ) +
        theme(
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none"
        )
}


# Testing ----
# data <- tidy_reviews %>% 
#     count(word) %>% 
#     mutate(
#         prop = percent(n/sum(n),accuracy = .1),
#         label_text = str_glue('{n} ({prop})')
#     ) %>% 
#     arrange(desc(n)) %>% 
#     head(10)
# 
# bar_charts_func(data,x = word, y = n, 
#                 label = label_text,
#                 title = 'Most Common Reviewer Words',
#                 subtitle = 'Shows counts and proportions of the 10 most common words used by all the reviewers',
#                 xlabel = '',
#                 ylabel = '')
