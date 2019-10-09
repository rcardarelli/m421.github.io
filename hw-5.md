Assignment 5
================
Rachel Cardarelli
October 9, 2019

Question 1
----------

``` r
library(readxl)
library(stringr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#install.packages("ggplot2")
library(ggplot2)

c2015 <- read_excel("c2015.xlsx")
d <- tbl_df(c2015)

d = d %>% filter_all(~!is.na(.))
d = d %>% filter_all(~!(.=='Unknown'))
d = d %>% filter_all(~!(.=='Not Rep'))
d = d %>% filter_all(~!(.==str_detect(.,'Not Rep')))
d = d %>% filter_all(~!(.==str_detect(.,'Unknown')))
d = d %>% filter_all(~!(.=='Not Reported'))
d = d %>% filter(SEAT_POS=='Front Seat, Left Side')
d = d %>% mutate(AGE = as.numeric(AGE))
d = d %>% mutate(TRAV_SP = str_replace(TRAV_SP," MPH",""))
d = d %>% mutate(TRAV_SP = replace(TRAV_SP, TRAV_SP == "Stopped", "0"))
d = d %>% mutate(TRAV_SP = as.numeric(TRAV_SP))  
```

Question 2
----------

``` r
ggplot(d, aes(AGE, TRAV_SP, color = SEX)) + geom_point()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-2-1.png)

Question 3
----------

``` r
ggplot(d, aes(AGE, TRAV_SP, color = SEX)) + geom_jitter()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-3-1.png)

Question 4
----------

``` r
ggplot(d, aes(AGE)) + geom_histogram(bins = 50)
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(d, aes(TRAV_SP)) + geom_histogram(bins = 50)
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-4-2.png)

Question 5
----------

``` r
ggplot(d, aes(AGE, color = SEX)) + geom_histogram(bins = 50)
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-5-1.png)

Question 6
----------

``` r
ggplot(d, aes(AGE, color = SEX)) + geom_density()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-6-1.png)

Question 7
----------

``` r
ggplot(d, aes(TRAV_SP, color = INJ_SEV)) + geom_density()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-7-1.png)

Question 8
----------

``` r
g <- d %>% mutate(day = ifelse(DAY_WEEK %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) 
  
ggplot(g, aes(TRAV_SP, color = day)) + geom_density()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-8-1.png)

Question 9
----------

``` r
ggplot(d, aes(MONTH)) + geom_bar()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(d, aes(MONTH, fill = SEX)) + geom_bar()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-9-2.png)

Question 10
-----------

``` r
ggplot(d, aes(MONTH, fill = SEX)) + geom_bar(position = 'dodge')
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-10-1.png)

Question 11
-----------

``` r
g <- d %>% group_by(MONTH) %>% summarise(mean_sp = mean(TRAV_SP, na.rm = TRUE))
ggplot(g, aes(MONTH, mean_sp)) + geom_col()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-11-1.png)

Question 12
-----------

``` r
g <- d %>% group_by(MONTH) %>% summarise(mean_sp = mean(TRAV_SP, na.rm = TRUE))
ggplot(g, aes(MONTH, mean_sp)) + geom_bar(stat='identity')
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-12-1.png)

Question 13
-----------

``` r
g <- d %>% group_by(MONTH, SEX) %>% summarise(mean_sp = mean(TRAV_SP, na.rm = TRUE))
ggplot(g, aes(MONTH, mean_sp, fill = SEX)) + geom_col()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-13-1.png)

Question 14
-----------

``` r
g <- d %>% group_by(MONTH, INJ_SEV) %>% summarise(mean_sp = mean(TRAV_SP, na.rm = TRUE))
ggplot(g, aes(MONTH, mean_sp, color = INJ_SEV)) + geom_col()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-14-1.png)

Question 15
-----------

``` r
m <- mean(d$TRAV_SP, na.rm = TRUE)
g <- d %>% group_by(MONTH) %>% summarise(mean_sp = mean(TRAV_SP, na.rm = TRUE)) %>% mutate(Speed = ifelse(mean_sp > m, "Above Average", "Below Average")) 

g$mean_sp <- round((g$mean_sp - mean(g$mean_sp))/sd(g$mean_sp), 2)

ggplot(g, aes(reorder(MONTH, mean_sp),mean_sp)) +
       scale_color_manual(name="Speed", labels = c("Above Average", "Below Average"), values = c("above"="#00ba38",          "below"="#f8766d")) +
       geom_bar(stat='identity', aes(fill=Speed), width=.5) + 
       coord_flip()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-15-1.png)

Question 16
-----------

``` r
m <- mean(d$TRAV_SP, na.rm = TRUE)
g <- d %>% na.omit %>% group_by(SEX, INJ_SEV) %>%summarise(mean_sp = round(mean(TRAV_SP, na.rm = TRUE),3)) %>% mutate(Speed = ifelse(mean_sp > m, "Above Average", "Below Average")) 

ggplot(g, aes(reorder(INJ_SEV, mean_sp), mean_sp, label = mean_sp)) + 
      scale_color_manual(name="Sex", labels = c("Female", "Male"), values = c("Female"="#00ba38", "Male"="#f8766d")) +
      geom_point(stat='identity', aes(col= SEX), size=12)  +    
      geom_text(color="white", size=3) + 
      coord_flip()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-16-1.png)

Question 17
-----------

``` r
#install.packages("ggthemes")
library(ggthemes)
g <- d %>% na.omit %>% group_by(SEX, MONTH, DAY) %>% summarise(mean_sp = round(mean(TRAV_SP, na.rm = TRUE),3)) %>% arrange(mean_sp) %>% mutate(mean_sp = ifelse(SEX == "Female", mean_sp, -mean_sp)) 

brks <- seq(-600, 600, 100)
lbls = paste0(as.character(c(seq(600, 0, -100), seq(100, 600, 100))), "m")

ggplot(g, aes(DAY, mean_sp, fill = SEX)) + 
     geom_bar(stat = "identity", width = .6) +
     scale_y_continuous(breaks= brks, labels = lbls) +
     theme_tufte() +
     coord_flip() +
     geom_hline(yintercept = 0, size  = 1.5, color = "white")
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-17-1.png)

Question 18
-----------

``` r
g = d %>% filter(WEATHER != 'Unkno')
g = g %>% filter(WEATHER != 'Other')
g = g %>% filter(WEATHER != 'Not R')
g = g %>% select(OWNER, SEX, DRINKING, TRAV_SP) %>% mutate(Owner = case_when( OWNER =='Driver (in this crash) was  Registered Owner' ~ "Owner",
  OWNER == 'Driver (in this crash) Not Registered Owner (Other Private Owner Listed)' ~ "Owner",
  OWNER == 'Vehicle Registered as Business/Company/Government Vehicle' ~ "Not Owner",
  OWNER == 'Vehicle Registered as Rental Vehicle' ~ 'Not Owner',
  TRUE ~ "Other"
))

g = g %>% group_by(Owner, SEX, DRINKING) %>% summarise(mean_sp = mean(TRAV_SP, na.rm = TRUE))

ggplot(g, aes(Owner, mean_sp, fill = SEX)) + geom_bar(stat='identity', position = 'dodge') + facet_wrap(~DRINKING, nrow = 2) 
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-18-1.png)

Question 19
-----------

``` r
m <- mean(d$TRAV_SP, na.rm = TRUE)
g = d %>% filter(WEATHER != 'Unkno')
g = g %>% filter(WEATHER != 'Other')
g = g %>% filter(WEATHER != 'Not R')
g = g %>% na.omit %>% group_by(SEX, WEATHER) %>%summarise(mean_sp = round(mean(TRAV_SP, na.rm = TRUE),3)) %>% mutate(Speed = ifelse(mean_sp > m, "Above Average", "Below Average")) 

ggplot(g, aes(reorder(WEATHER, mean_sp), mean_sp, label = mean_sp)) + 
      scale_color_manual(name="Sex", labels = c("Female", "Male"), values = c("Female"="#00ba38", "Male"="#f8766d")) +
      geom_point(stat='identity', aes(col= SEX), size=12)  +    
      geom_text(color="white", size=3) + 
      coord_flip()
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-19-1.png)

Question 20
-----------

``` r
#g <- d %>% mutate(day = ifelse(DAY_WEEK %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) 
  
ggplot(d, aes(TRAV_SP, color = SEX)) + geom_density() + facet_wrap(~DEFORMED)
```

![](hw-5_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
#ggplot(d, aes(INJ_SEV, color = INJ_SEV)) + geom_density()
```
