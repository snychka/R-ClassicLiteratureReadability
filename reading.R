library(dplyr)
library(ggplot2)

source('data.R')
# https://dplyr.tidyverse.org/reference/arrange.html
books_by_download <- arrange(books, desc(downloads))
books_refined <- books_by_download %>% select(author, title, words, syllables, sentences)

# guessed, knowing pull from last Proj and how R doesn't chain and head from linux
top_ten_authors <- head(pull(books_refined, author), 10)
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/filter search for %in%
authors_books <- arrange(filter(books_refined, author %in% top_ten_authors), desc(author))

# guessed, basically (knowledge of previous R Projects helped)
reading_ease <- mutate(authors_books, flesch_reading_ease = 206.835 - 1.015 * (words / sentences) - 84.6 * (syllables / words))
reading_grade <- mutate(reading_ease, flesch_kincaid_grade_level = 0.39 * (words / sentences) + 11.8 * (syllables / words) - 15.59)
# https://stackoverflow.com/a/28255866
reading_grouped <- group_by(reading_grade, author)
# sort guessed.  not quite sure why chained piping didn't work.  guessing loses data
# reading_summary <- reading_grouped %>% summarize(flesch_reading_ease = mean(flesch_reading_ease)) %>% summarize(flesch_kincaid_grade_level = mean(flesch_kincaid_grade_level))
reading_summary <- reading_grouped %>% summarize(flesch_reading_ease = mean(flesch_reading_ease), flesch_kincaid_grade_level = mean(flesch_kincaid_grade_level))
reading_long <- gather(reading_summary, type, score, flesch_reading_ease, flesch_kincaid_grade_level)

p <- ggplot(reading_long, aes(author, score)) + geom_bar(stat='identity')
p <- p + facet_grid(rows = vars(type))
# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(p)

