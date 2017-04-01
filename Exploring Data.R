#==========================================================================================
# Exploring Data
#==========================================================================================
library(ggplot2)


theme.alpha <- theme_bw()+
        theme(axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.line = element_blank())

library(grid)

income.ggplot <- ggplot(data = df.alpha, aes(x = Year, y = GSDP.per.capita, color = State)) +
        geom_line(aes(color = State), size = 1) +
        geom_dl(data = subset(df.alpha, Year == 2011), aes(Year+.2, GSDP.per.capita, color = State, label = State),
                  method = "last.bumpup") +
        labs(x = "Year", y = "Income per capita (rupees)") + 
        scale_x_continuous(limits = x.limits + 1, breaks = x.breaks) +
        theme.alpha +
        theme(legend.position ="none", axis.line = element_blank())

ggsave("income.ggplot.pdf", plot = income.ggplot)


ggplot(data = df.alpha, aes(x = Year, y = Average.GSDP.Growth, color = State)) +
        geom_line(aes(color = State), size = 1) +
        geom_point(aes(color = State), size = 1.3)

#------------------------------------------------------------------------------------------
# Unconditional
#------------------------------------------------------------------------------------------

unconditional.chart <- ggplot(data = df.ggplot, 
                              aes(x = Log.Initial.GSDP, y = Average.GSDP.Growth)) + 
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) + 
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Per Capita SDP 1991", 
                             y = "Average SDP Growth 1991-2011") + 
                        theme.alpha 

ggsave("unconditional.chart.pdf", plot = unconditional.chart)


#------------------------------------------------------------------------------------------
# Rates
#------------------------------------------------------------------------------------------

Average.Population.Growth <- as.numeric(unique(df.alpha$Average.Population.Growth))
Average.Social.Expenditure <- as.numeric(unique(df.alpha$Average.Social.Expenditure))
Average.Gross.Capital.Formation <- as.numeric(unique(df.alpha$Average.Gross.Capital.Formation))
Average.Personal.Loans.by.SCBs <- as.numeric(unique(df.alpha$Average.Personal.Loans.by.SCBs))
Average.Literacy.Rate <- as.numeric(unique(df.alpha$Average.Literacy.Rate))
Average.Elec.Cons <- as.numeric(unique(df.alpha$Average.Elec.Cons))
Average.IMR <- as.numeric(unique(df.alpha$Average.IMR))
Average.Ag.Share <- as.numeric(unique(df.alpha$Average.Ag.Share))
Average.GSDP.Growth <- as.numeric(unique(df.alpha$Average.GSDP.Growth))
Average.GSDP.Level <- as.numeric(unique(df.alpha$Average.GSDP.Level))
State <- c('AN.P', 'ASM', 'BHR', 'GJT', 'HYA', 'KNK', 'KRL','MD.P', 'MHA', 'ODS',
           'PJB', 'RJT', 'TM.N', 'U.P', 'W.B')
Log.Initial.GSDP <- as.numeric(unique(df.alpha$Log.Initial.GSDP))

df.ggplot <- data.frame(State, Average.Population.Growth, Log.Initial.GSDP, Average.Social.Expenditure,
                              Average.Gross.Capital.Formation, Average.Personal.Loans.by.SCBs,
                              Average.Literacy.Rate, Average.Elec.Cons, Average.IMR,
                              Average.Ag.Share, Average.GSDP.Growth, Average.GSDP.Level)

population.growth <- ggplot(df.ggplot, aes(Average.Population.Growth, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Population Growth ",
                             y = "Average SDP Growth") + theme.alpha

ggsave("population.growth.pdf", plot = population.growth)

social.growth <- ggplot(df.ggplot, aes(Average.Social.Expenditure, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Social Expenditure (% of SDP)",
                                y = "Average SDP Growth") + theme.alpha

ggsave("social.growth.pdf", plot = social.growth)

fixed.capital.growth <- ggplot(df.ggplot, aes(Average.Gross.Capital.Formation, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Fixed Capital Formation (% of SDP)",
                                y = "Average SDP Growth") + theme.alpha

ggsave("fixed_capital_growth.pdf", plot = fixed.capital.growth)

loan.growth <- ggplot(df.ggplot, aes(Average.Personal.Loans.by.SCBs, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Personal Loans (% of SDP)",
                                y = "Average SDP Growth") + theme.alpha

ggsave("loan.growth.pdf", plot = loan.growth)

literacy.growth <- ggplot(df.ggplot, aes(Average.Literacy.Rate, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Literacy Rate",
                                y = "Average SDP Growth") + theme.alpha

elec.growth <- ggplot(df.ggplot, aes(Average.Elec.Cons, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Per Capita Electricity Consumption",
                                y = "Average SDP Growth") + theme.alpha

imr.growth <- ggplot(df.ggplot, aes(Average.IMR, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Infant Mortality Rate",
                                y = "Average SDP Growth") + theme.alpha

agshare.growth <- ggplot(df.ggplot, aes(Average.Ag.Share, Average.GSDP.Growth)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Share of Agriculture (% of SDP)",
                                y = "Average SDP Growth") + theme.alpha

#------------------------------------------------------------------------------------------
# Levels
#------------------------------------------------------------------------------------------

population.levels <- ggplot(df.ggplot, aes(Average.Population.Growth, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Population Growth",
                                y = "Average SDP per capita") + theme.alpha

social.levels <- ggplot(df.ggplot, aes(Average.Social.Expenditure, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Social Expenditure\n(% of SDP)",
                                y = "Average SDP per capita") + theme.alpha

fixed.capital.levels <- ggplot(df.ggplot, aes(Average.Gross.Capital.Formation, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Fixed Capital Formation\n(% of SDP)",
                                y = "Average SDP per capita") + theme.alpha

loan.levels <- ggplot(df.ggplot, aes(Average.Personal.Loans.by.SCBs, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Personal Loans (% of SDP)",
                                y = "Average SDP per capita") + theme.alpha

literacy.levels <- ggplot(df.ggplot, aes(Average.Literacy.Rate, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Literacy Rate",
                                y = "Average SDP per capita") + theme.alpha

elec.levels <- ggplot(df.ggplot, aes(Average.Elec.Cons, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Per Capita Electricity Consumption",
                             y = "Average SDP per capita") + theme.alpha

imr.levels <- ggplot(df.ggplot, aes(Average.IMR, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Infant Mortality Rate",
                                y = "Average SDP per capita") + theme.alpha

agshare.levels <- ggplot(df.ggplot, aes(Average.Ag.Share, Average.GSDP.Level)) +
                        geom_point(size = 1.3) +
                        geom_text_repel(aes(label = State), size = 3) +
                        geom_line(stat = "smooth", method = "lm", color = "#9966CC", alpha = .5, se = F, size = 1) +
                        labs(x = "Average Share of Agriculture (% of SDP)",
                                y = "Average SDP per capita") + theme.alpha


multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
        require(grid)
        
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        if (is.null(layout)) {
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots == 1) {
                print(plots[[1]])
                
        } else {
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                for (i in 1:numPlots) {
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}


#------------------------------------------------------------------------------------------
# Bump Chart
#------------------------------------------------------------------------------------------

x.right <- 2011 + 5
x.limits <- c(1991, x.right)
x.breaks <- seq(1990, 2010, 5)

better.bump <- ggplot(df.alpha, aes(Year, Income.Rank, color=State, label = State, Group = State)) +
        geom_line(size=1.3) +
        geom_point(size = .5) +
        geom_text(data = subset(df.alpha, Year == 2011), nudge_x = .1, aes(Year, Income.Rank, label = State), hjust = 0) +
        xlab("Year") +
        ylab("Rank") +
        scale_x_continuous(limits = x.limits, breaks = x.breaks) +
        scale_y_continuous(trans="reverse", breaks=unique(df.alpha$Income.Rank)) +
        theme.alpha +
        theme(legend.position ="none")  

ggsave("better.bump.pdf", plot = better.bump)   

#------------------------------------------------------------------------------------------
#  Themes
#------------------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------------------
#  Preliminary small multiples plot
#------------------------------------------------------------------------------------------

ggplot(data = df.alpha, aes(x = Year, y = GSDP.per.capita, color = State)) +
        geom_line(size = 1) +
        geom_point(size = 1.1) +
        facet_wrap( ~State) +
        labs(title = "GSDP per capita among\nIndia's largest states") +
        labs(x = "Year", y="GSDP\nin INR") +
        theme.smallmult

ggplot(data = df.alpha, aes(x = Year, y = Population.Growth)) +
        geom_line(size = 1) +
        geom_point(size = 1.1) +
        facet_wrap(~State) +
        theme.smallmult

#------------------------------------------------------------------------------------------
#  Panel Scatterplot
#------------------------------------------------------------------------------------------

scatterplot(GSDP.per.capita ~ Year|State, boxplots = F, smooth = T, reg.line = F, data = df.alpha)
scatterplot(GSDP.Growth ~ Year|State, boxplots = F, smooth = T, reg.line = F, data = df.alpha)

#------------------------------------------------------------------------------------------
#  Correlation Plot
#------------------------------------------------------------------------------------------

match("Grain.Yields", names(df.alpha)) ## 7
match("Water.Access", names(df.alpha)) ## 4
match("Credit.by.SCBs", names(df.alpha)) ## 12
match("Gross.Fixed.Capital.Formation", names(df.alpha)) ## 16
match("Per.Capita.Elec.Cons", names(df.alpha)) ## 20
match("Share.Rural.Pop", names(df.alpha)) ## 27

correlations <- cor(df.alpha[1:21,c(4:7, 9:29)])
corrplot(correlations, method = "circle")

correlations2 <- cor(df.alpha[1:21, c(7, 12, 16, 27)])
corrplot(correlations2, method = "circle")

# red is negative correlation, blue is positive
# the larger the circle, the higher the correlation

stargazer(correlations2, title = "Correlation Matrix")

df.malpha <- df.alpha

dropitlikeitshot <- colnames(df.malpha)[colSums(is.na(df.malpha)) > 0]

df.malpha <- df.malpha %>% select(-one_of(dropitlikeitshot))

df.malpha <- as.matrix(df.malpha[,3:29])

whatworks <- rcorr(df.malpha, type = "spearman")


#==========================================================================================
# Fixed Effects Models
#==========================================================================================
# Heterogeineity across states
#------------------------------------------------------------------------------------------

plotmeans(GSDP.per.capita ~ State, main = "Heterogeineity across states", data=df.alpha)

# clear stagnancy across poorer states

plotmeans(GSDP.Growth ~ State, main = "Heterogeineity across states", data=df.alpha)

#------------------------------------------------------------------------------------------
# Heterogeineity across years
#------------------------------------------------------------------------------------------

plotmeans(GSDP.per.capita ~ Year, main = "Heterogeneity across years", data = df.alpha)

# clear divergence over time

plotmeans(GSDP.Growth ~ Year, main = "Heterogeneity across years", data = df.alpha)

#------------------------------------------------------------------------------------------
# Summary Tables
#------------------------------------------------------------------------------------------

Income.91 <- as.numeric(unique(df.alpha$Income.per.capita[Year == 1991]))
Income.01 <- as.numeric(unique(df.alpha$Income.per.capita[Year == 2001]))
Income.11 <- as.numeric(unique(df.alpha$Income.per.capita[Year == 2011]))



State <- unique(df.alpha$State)

df.summary <- data.frame(State, Income.91, Income.01, Income.11)

latex(df.summary, file = "", digits = 6)


ggsave("population.levels.pdf", plot = population.levels)

ggsave("social.levels.pdf", plot = social.levels)

ggsave("fixed.capital.levels.pdf", plot = fixed.capital.levels)

ggsave("loan.levels.pdf", plot = loan.levels)

ggsave("literacy.growth.pdf", plot = literacy.growth)

ggsave("literacy.levels.pdf", plot = literacy.levels)

ggsave("elec.growth.pdf", plot = elec.growth)

ggsave("elec.levels.pdf", plot = elec.levels)

ggsave("imr.growth.pdf", plot = imr.growth)

ggsave("imr.levels.pdf", plot = imr.levels)

ggsave("agshare.growth.pdf", plot = agshare.growth)

ggsave("agshare.levels.pdf", plot = agshare.levels)


