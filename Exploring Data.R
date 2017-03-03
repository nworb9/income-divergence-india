##  [Exploring Data]  ##

ggplot(data = df.alpha, aes(x = Year, y = GSDP.per.capita, color = State)) +
        geom_line(aes(color = State), size = 1) +
        geom_point(aes(color = State), size = 1.3) +
        labs(title = "Income per capita across India's\n15 largest states") +
        labs(x = "Year", y = "GSDP per capita")

ggplot(data = df.alpha, aes(x = Year, y = Average.GSDP.Growth, color = State)) +
        geom_line(aes(color = State), size = 1) +
        geom_point(aes(color = State), size = 1.3)

# [ Bump Chart ]

ggplot(data = df.alpha, aes(x = Year, y = Income.Rank, color = State)) +
        geom_line(aes(color = State), size = 2) +
        geom_point(aes(color = State), size = 2.3) +
        geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
        labs(title = "Indian States ranked by\nincome per capita") +
        labs(subtitle = "(Income per capita)") +
        labs(x = "Year", y = "Rank") +
        scale_y_continuous(trans = "reverse", breaks = unique(df.alpha$Income.Rank))

##  Themes

theme.porttheme <-  
        theme(text = element_text(family = "Gill Sans", color = "#444444")) +
        theme(plot.title = element_text(size = 24)) +
        theme(plot.subtitle = element_text(size = 18)) +
        theme(axis.title = element_text(size = 14)) +
        theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
        theme(axis.text = element_text(size = 10)) +
        theme(axis.title.x = element_text(margin = margin(t = 20))) +
        theme(legend.title = element_blank())

theme.smallmult <- 
        theme(text = element_text(family = "Gill Sans", color = "#444444")) +
        theme(plot.title = element_text(size = 24)) +
        theme(plot.subtitle = element_text(size = 18)) +
        theme(axis.title = element_text(size = 14)) +
        theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
        theme(axis.title.x = element_text(margin = margin(t = 20))) +
        theme(legend.title = element_blank())
        theme(axis.text = element_text(size = 6)) +
        theme(axis.text.x = element_text(angle = 90))

##  Prelim small multiples plot

ggplot(data = df.alpha, aes(x = Year, y = GSDP.per.capita, color = State)) +
        geom_line(size = 1) +
        geom_point(size = 1.1) +
        facet_wrap( ~State) +
        labs(title = "GSDP per capita among\nIndia's largest states") +
        labs(x = "Year", y="GSDP\nin INR") +
        theme.smallmult

##  Brew Colors

# work in progress...

##  Panel Scatterplot

scatterplot(GSDP.per.capita ~ Year|State, boxplots = F, smooth = T, reg.line = F, data = df.alpha)

##  Correlation Plot

match("Grain.Yields", names(df.alpha)) ## 7
match("Water.Access", names(df.alpha)) ## 4
match("Credit.by.SCBs", names(df.alpha)) ## 12
match("Gross.Fixed.Capital.Formation", names(df.alpha)) ## 16
match("Per.Capita.Elec.Cons", names(df.alpha)) ## 20
match("Share.Rural.Pop", names(df.alpha)) ## 27

correlations <- cor(df.alpha[1:21,c(4, 7, 12, 16, 20, 27)])
corrplot(correlations, method = "circle")

correlations <- cor(df.alpha[1:21, c(7, 12, 16, 27)])
corrplot(correlations, method = "circle")

# red is negative correlation, blue is positive
# the larger the circle, the higher the correlation
        
## [ Fixed Effects Models ]

##  Heterogeineity across states

plotmeans(GSDP.per.capita ~ State, main = "Heterogeineity across states", data=df.alpha)

# clear stagnancy across poorer states

##  Heterogeineity across years

plotmeans(GSDP.per.capita ~ Year, main = "Heterogeneity across years", data = df.alpha)

# clear divergence over time
