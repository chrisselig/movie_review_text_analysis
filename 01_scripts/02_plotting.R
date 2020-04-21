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
        geom_bar(stat = 'identity',aes(fill = n))
    
    if(facet){
        p <- p + facet_wrap(~reviewer,scales = "free")
    }
    
    p <- p +
        geom_text(aes(label = label_text), size = size, hjust = hjust, color = 'white') +
        tidytext::scale_y_reordered() +
        # Formatting
        scale_fill_gradient2(low='white', mid='#A2AAB0', high='#3E3E3B') +
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




# bar_charts_func <- function(data, 
#                        x = word,
#                        y = n,
#                        facet = FALSE,
#                        facet_variable = NA,
#                        label,
#                        title,
#                        subtitle,
#                        xlabel,
#                        ylabel,
#                        hjust = 1.25,
#                        size = 3){
#     
#     p <- data %>% 
#         ggplot(aes(x = n,y = reorder(word,n))) +
#         # Geoms
#         geom_bar(stat = 'identity',aes(fill = n))
#     
#     if(facet){
#         p <- p + facet_wrap(~reviewer,scales = "free")
#     }
#     
#     p <- p +
#         geom_text(aes(label = label_text), size = size, hjust = hjust, color = 'white') +
#         tidytext::scale_y_reordered() +
#         # Formatting
#         scale_fill_gradient2(low='white', mid='#A2AAB0', high='#3E3E3B') +
#         labs(
#             title = title,
#             subtitle = subtitle,
#             x = xlabel,
#             y = ylabel
#         ) +
#         theme(
#             plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
#             plot.subtitle = element_text(hjust = 0.01, size = 11,family = "Arno Pro Light Display"),
#             axis.text.x=element_blank(),
#             axis.ticks.x=element_blank(),
#             legend.position = "none"
#         )
#     return(p)
# }


# Testing ----
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


