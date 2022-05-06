
# CODE FOR PAGE 377

# Install the tidyverse suite of packages if not already installed
install.packages("tidyverse", dependencies = TRUE)
# Load tidyverse
library(tidyverse)

# CODE FOR PAGE 379, 1ST CHUNK

# In the line below, we name the chart and specify the dataset to use
bar_chart_plain <- ggplot(data = assessment_data,
                          # Test.Subject as the x-axis gives one bar
                          # per Test.Subject
                          aes(x = Test.Subject)) +
  # Specifying a bar chart
  geom_bar()

# CODE FOR PAGE 379, 2ND CHUNK

# Calling up the bar chart by name to make it appear
bar_chart_plain

# CODE FOR PAGES 380-381

# Name the bar chart and specify to use assessment_data for it
bar_chart_color <- ggplot(data = assessment_data,
                          # We give the x-axis column;
                          # "fill" colors bars by Test.Subject
                          aes(x = Test.Subject,
                              fill = Test.Subject)) +
  # Specifying a bar chart
  geom_bar() +
  # Adding a title and specific labels for the axes and the legend
  labs(title = "Count of tests in each subject area across school years",
       # Below"fill" is what labels the legend
       x = "Subject",
       y = "Number of tests",
       fill = "Test subject")

# Calling up our bar chart with colors by name to make it appear
bar_chart_color

# CODE FOR PAGE 382

# First line is as before, with new name for the ggplot2 object
# but specifying the same assessment_data
bar_chart_grouped <- ggplot(data = assessment_data,
                            # The x-axis is also the same, but fill
                            # is set so that color reflects Building
                            aes(x = Test.Subject,
                                fill = Building)) +
  # Specifying a grouped bar chart with position_dodge
  # Note that the combination of posistion_dodge and
  # (preserve = "single") makes it so that all bars will
  # have the same width, even with only one Building
  # represented for a subject area
  geom_bar(position = position_dodge(preserve = "single")) +
  # Adding a title and specific labels for the axes and the legend
  labs(title = "Count of tests in each subject area across school years",
       x = "Subject",
       y = "Number of tests",
       fill = "School building")

# Calling up our grouped bar chart to make it appear
bar_chart_grouped

# CODE FOR BOTTOM OF PAGE 384 TO TOP OF PAGE 385

# Plain histogram of math assessment scores
histogram_plain <- ggplot(data = assessment_data %>%
                            # Filtering to have only one
                            # Test.Subject (Mathematics)
                            filter(Test.Subject == "Mathematics"),
                          # Specifying SCALE_SCORE as the column to
                          # display and having color reflect height
                          # (the count of scores)
                          aes(x = SCALE_SCORE)) +
  # Specifying histogram for the viz
  geom_histogram() +
  # Making nicer labels
  labs(x = "Scale scores in mathematics",
       y = "Count of scores",
       title = "Histogram of math scale scores")

# Calling up the histogram
histogram_plain

# CODE FOR PAGE 386

# Making our histogram with paneling by year where color reflects count
histogram_paneled <- ggplot(data = assessment_data %>%
                              # Filtering to have only one
                              # Test.Subject (Mathematics)
                              filter(Test.Subject == "Mathematics"),
                            # Specifying SCALE_SCORE as the column to
                            # display and having color reflect height
                            # (the count of scores)
                            aes(x = SCALE_SCORE,
                                fill = ..count..)) +
  # Specifying histogram for the viz and setting the binwidth
  # (width of each bar making up the histogram) to 10
  geom_histogram(binwidth = 10) +
  # Creating separate panels on top of each other by value of School.Year
  # The dir = "v" part of the code stacks the panels vertically
  facet_wrap( ~ School.Year, dir = "v") +
  # Making nicer labels, adding a title
  labs(x = "Scale scores by year in mathematics",
       y = "Count of students",
       fill = "Count of students",
       title = "Histogram of math scale scores by school year")

# Calling up our paneled histogram
histogram_paneled

# CODE FOR PAGE 388, 1ST CHUNK

# Store the means for SCALE_SCORE by year
means_by_year <- assessment_data %>%
  # In the graph below, we will filter our data to only have Mathematics
  # and leave out the 2016-2017 school year as well as any rows
  # where the scale score equals the raw score. We do the same
  # filtering here to ensure means match the data for the histogram.
  filter(Test.Subject == "Mathematics" &
           School.Year != "2016/2017" &
           SCALE_SCORE != RAW_SCORE) %>%
  # Selecting only the variables needed to calculate mean by year
  dplyr::select(School.Year, SCALE_SCORE) %>%
  # Grouping by School.Year to get separate means by year
  group_by(School.Year) %>%
  # Storing mean in the variable scale_score_mean
  summarize(scale_score_mean = mean(SCALE_SCORE, na.rm = TRUE))

# CODE FOR BOTTOM OF PAGE 388 (2ND CHUNK) TO TOP OF PAGE 389

# Making paneled histogram with vertical lines showing mean by year
histogram_w_mean_lines <- ggplot(data = assessment_data %>%
                                   # Filter our data to only have 
                                   # Mathematics and leave out the 2016-2017
                                   # school year plus any rows where the 
                                   # scale score equals the raw score
                                   filter(Test.Subject == "Mathematics" &
                                            School.Year != "2016/2017" &
                                            SCALE_SCORE != RAW_SCORE),
                                 # Specifying SCALE_SCORE as the column to
                                 # display and having color reflect height
                                 # (the count of scores)
                                 aes(x = SCALE_SCORE,
                                     fill = ..count..)) +
  # Specifying histogram for the viz and setting the binwidth to 5
  geom_histogram(binwidth = 5) +
  # Putting the means stored in scale_score_means as vertical lines over histogram
  geom_vline(data = means_by_year,
             mapping = aes(xintercept = scale_score_mean)) +
  # Creating separate panels on top of each other by value of School.Year
  facet_wrap(~ School.Year, dir = "v") +
  # Making nicer labels
  labs(x = "Scale scores by year in mathematics",
       y = "Count of students",
       fill = "Count of students",
       title = "Histogram of math scale scores by school year",
       subtitle = "Vertical line gives mean scale score by year")

# Calling up our histogram with mean lines
histogram_w_mean_lines

# CODE FOR PAGE 390

# Filtering, deduplicating, and reshaping the data
math_data_wide <- assessment_data %>%
  # Keeping only math scores and excluding the 2016-2017
  # school year and cases where scale and raw scores
  # are equal
  filter(Test.Subject == "Mathematics" &
           School.Year != "2016/2017" &
           SCALE_SCORE != RAW_SCORE) %>%
  # Deduplicating the data to have only one row
  # per student ID per year
  distinct(STUDENT_ID, School.Year, Test.Subject,
           # This keep_all option tells R to keep all
           # variables, not only the ones named above
           .keep_all = TRUE) %>%
  # Making one column for each school year,
  # where the values are from SCALE_SCORE
  pivot_wider(names_from = School.Year,
              id_cols = c(STUDENT_ID, level_change),
              values_from = SCALE_SCORE) %>%
  # Dropping rows with NA values in any column
  drop_na()

# CODE FOR PAGE 392

# Plain scatterplot of 2017-2018 vs. 2018-2019 math scores
scatter_plot_plain <- ggplot(data = math_data_wide,
                             # Specifying 2017/2018 for the x-axis
                             # and 2018/2019 for the y-axis
                             # Notice the backticks (`)
                             aes(x = `2017/2018`,
                                 y = `2018/2019`)) +
  # Here, geom_point() makes the graph into a scatterplot
  geom_point() +
  # Specifying title, x-axis label, and y-axis label
  labs(title = "Scatterplot of 2017-2018 and 2018-2019 math scale scores",
       x = "2017-2018 math scores",
       y = "2018-2019 math scores")

# CODE FOR PAGE 393

# Calling up the name of our scatterplot to display it
scatter_plot_plain

# CODE FOR PAGE 394

# Same scatterplot as before but with color by level_change
# and semi-transparent points
scatter_plot_color <- ggplot(data = math_data_wide,
                             # Specifying 2017/2018 for the x-axis
                             # 2018/2019 for the y-axis
                             aes(x = `2017/2018`,
                                 y = `2018/2019`,
                                 color = level_change)) +
  # Here, geom_point() makes the graph into a scatterplot, and alpha
  # makes each point semi-transparent, which allows us to see when
  # points are on top of each other
  geom_point(alpha = 0.5) +
  # Specifying title, x-axis label, y-axis label, and legend ("color")
  # label
  labs(title = "Scatterplot of 2017-2018 and 2018-2019 math scale scores",
       x = "2017-2018 math scores",
       y = "2018-2019 math scores",
       # The \n in the label below puts everything that follows it
       # onto a new line
       color = "Level change from\n2017-2018 to 2018-2019")

# Calling up our new graph by name to display it
scatter_plot_color


