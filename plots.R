## STA 141
## Final Project: EDA


# I. Setup ----------------------------------------------------------------
#  A. Import packages
library(ggplot2)
library(ipumsr)
library(tidyverse)
library(plotly)
library(scales)
library(ggpubr)
library(stargazer)
library(extrafont)

# II. Data Loading --------------------------------------------------------
#  A. Read in the data 
df_ddi <- read_ipums_ddi("Dataset/nhis_00001.xml")
df <- as.data.frame(read_ipums_micro(df_ddi, verbose = FALSE))



# III. Data Processing ----------------------------------------------------



# IV. Data Analysis -------------------------------------------------------
#  A. Plot each variable
#   1. Make a function to plot a single distribution
plot_var <- function(col, title){
	plt_dt <- df %>% mutate(var_factor2 := as_factor({{col}})) %>% 
		group_by(var_factor2) %>% 
		summarize(cnt = n()) %>% 
		mutate(prcnt = cnt/sum(cnt))
	
	p <- ggplot(plt_dt, aes(x=reorder(var_factor2, cnt, sum), y=cnt)) +
		geom_col(fill = "steelblue", color = "black", width = .5) +
		scale_y_continuous(name = "Count of Respondents", labels = scales::comma_format(), expand = expansion(mult = c(0, .3))) +
		scale_x_discrete(name = "", labels = function(x) str_wrap(x, width = 15)) +
		labs(title = paste0(title)) +
		coord_flip() +
		geom_text(label = percent(plt_dt$prcnt, accuracy = 0.1), hjust = -0.15) + 
		theme_classic() +
		theme(plot.title = element_text(size = 19, hjust = 0.5),
								axis.text = element_text(size = 13),
								axis.title = element_text(size = 14),
								aspect.ratio = 1/2,
								text = element_text(family = "Times New Roman"))
	
	return(p)
}

covid_vars_plt <- ggarrange(plot_var(CVDDIAG, "Ever Told had or\nLikely had Coronavirus"),
																	plot_var(CVDTEST, "Ever Been Tested\n for Coronavirus"),
																	ncol=2, align = "hv") 

ggsave("001_chk.pdf", covid_vars_plt, dpi = 320)


# B. Histograms for all variables
#  1. Prep data 
plot_dt <- df %>% 
	select("FAMKIDNO", "ARMFEV", "SCHOOLNOW", "REGION", "URBRRL", "ASTATFLG") %>% 
	mutate(across(everything(), function(x) as.character(as_factor(x)))) %>% 
	gather() %>% 
	group_by(key, value) %>% 
	mutate(count_name_occurr = n())

plot_dt <- gather(df %>% select("FAMKIDNO", "RACEA", "ARMFEV", "EDUC", "SPOUSEDUC", "SCHOOLNOW",
																																"EMPSTAT", "HOURSWRK", "PAIDSICK", "EMPHI", "EMPFT",	"INCFAM07ON",
																																"FAMTOTINC", "USUALPL", "HINOTCOVE", "CVDDIAG","CVDTEST", "CVDTESTRSLT"))

# plot_dt$key2 <- factor(plot_dt$key, labels = c("Age~of~Testing~(days)","Blast~Furnace~Slag~(kg/m^3)","Cement~(kg/m^3)",
# 																																															"Coarse~Aggregate~(kg/m^3)","Concrete~Strength~(MPa)","Fine~Aggregate~(kg/m^3)",
# 																																															"Fly~Ash~(kg/m^3)","Superplasticizer~(kg/m^3)","Water~(kg/m^3)"))
#  2. Plot data 
hist_plot <- ggplot(plot_dt, aes(reorder(value,-count_name_occurr))) +
	geom_histogram(stat = "count", fill="steelblue", color="black") +
	scale_y_continuous(name = "Count of Respondents", labels = scales::comma_format(), expand = expansion(mult = c(0, .1))) +
	scale_x_discrete(name = "", labels = function(x) str_wrap(x, width = 20)) +
	coord_flip() +
	facet_wrap(~key, scales = 'free', labeller = label_parsed, ncol = 2) +
	labs(title = "Figure 2: Histograms of Untransformed Data") +
	ylab("Count") + 
	xlab("Value") +
	theme_classic() + 
	theme(plot.title = element_text(hjust = 0.5, size = 16),
							plot.margin = margin(0.5,0,0.5,0, "cm"),
							aspect.ratio = 1)
ggsave("000_chk.png", hist_plot, width = 7.5, height = 3, dpi = 320, )

# C. Make a table for each variable instead
tbl_dt <- df %>% 
	select("FAMKIDNO", "ARMFEV", "SCHOOLNOW", "REGION", "URBRRL", "ASTATFLG") %>% 
	mutate(across(everything(), function(x) as.character(as_factor(x)))) %>% 
	gather() %>% 
	group_by(key, value) %>% 
	mutate(cnt = n()) %>% 
	unique()


# V. Data Output ----------------------------------------------------------



