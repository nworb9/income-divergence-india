##  [Exploring Data]  ##

##  Themes

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
        theme.porttheme

##  Brew Colors

# work in progress...

##  Scatterplot

scatterplot(GSDP.per.capita ~ Year|State, boxplots = F, smooth = T, reg.line = F, data = df.alpha)


