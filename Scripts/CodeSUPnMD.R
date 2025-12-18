## Title: script to build the graphics of the correlation between Support values and missing data of the terminals.
## Article: "Over-reliance on Completeness exposes Ultra Conserved Elements datasets to non-randomly distributed missing data" (Garzón-Orduña et al. 2025)
## Date: 2025-09-22
## Author: Ambrosio Torres (Researcher [Ctr. Integr. Biodivers. Discov. - Museum für Naturkunde, Berlin, Germany)
## Maintainer: Ambrosio Torres <atorresgalvis@gmail.com> <Ambrosio.TorresGalvis@mfn.berlin>
## Depends: R version (>= 4.4.1 ). Packages Cairo (1.6-2); ggplot2 (3.5.2); RColorBrewer (1.1-3); tidyverse (2.0.0).
## License: GPL (3)

#########
## Load the packages and fonts needed
#########
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
# extrafont::font_import()
extrafont::loadfonts()

## Blaimer
	data <- read.csv("SUPnMD_Blaimer2023.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p1e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Blaimer et al. 2023")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Blaimer.pdf", width = 2.4, height = 9.6)
		p1e
	dev.off()

## Borowiec
	data <- read.csv("SUPnMD_Borowiec2025.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p2e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Borowiec et al. 2025")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Borowiec.pdf", width = 2.4, height = 9.6)
		p2e
	dev.off()


## Buenaventura
	data <- read.csv("SUPnMD_Buenaventura2021.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p3e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Buenaventura et al. 2021")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Buenaventura.pdf", width = 2.4, height = 9.6)
		p3e
	dev.off()

## Galleti
	data <- read.csv("SUPnMD_Galleti2023.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p4e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Galleti-Lima et al. 2023")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Galleti.pdf", width = 2.4, height = 9.6)
		p4e
	dev.off()

## Joele
	data <- read.csv("SUPnMD_Joele2024.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p5e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Joele et al. 2024")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Joele.pdf", width = 2.4, height = 9.6)
		p5e
	dev.off()

## Reyes
	data <- read.csv("SUPnMD_Reyes2025.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p6e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Reyes-Hernández et al. 2025")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Reyes.pdf", width = 2.4, height = 9.6)
		p6e
	dev.off()

## Santos
	data <- read.csv("SUPnMD_SantosBrady2024.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p7e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Santos & Brady 2024")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Santos.pdf", width = 2.4, height = 9.6)
		p7e
	dev.off()

## Zhang
	data <- read.csv("SUPnMD_Zhang2023.csv", stringsAsFactors = FALSE)
	data$PercOfMiss <- round(as.numeric(data$PercOfMiss), 1)
	
	data <- data %>%
		mutate(MeanNodes = (FirstNode + SecondNode + ThirdNode)/3)
	
	data_long <- data %>%
		pivot_longer(cols = c(FirstNode, SecondNode, ThirdNode, MeanNodes),
					names_to = "Node",
					values_to = "Support") %>%
		mutate(NodeLabel = case_when(
			Node == "FirstNode"  ~ "1st",
			Node == "SecondNode" ~ "2nd",
			Node == "ThirdNode"  ~ "3rd",
			Node == "MeanNodes"  ~ "Mean"
		))
	
	data_long <- data_long %>%
		mutate(NodeLabel = factor(NodeLabel, levels = c("1st","2nd","3rd","Mean")),
			PercOfMiss_f = factor(PercOfMiss, levels = unique(PercOfMiss)))  # eje Y discreto
	
	y_breaks <- levels(data_long$PercOfMiss_f)[seq(1, length(levels(data_long$PercOfMiss_f)), by = 10)]
	
	min_support <- min(data_long$Support, na.rm = TRUE)
	max_support <- max(data_long$Support, na.rm = TRUE)
	
	data_long <- data_long %>%
		mutate(Support_scaled = (Support - min_support) / (max_support - min_support))
	
	p8e <- ggplot(data_long, aes(x = NodeLabel, y = PercOfMiss_f, fill = Support_scaled)) +
		geom_tile(color = "white") +
		scale_fill_gradientn(colors = rev(brewer.pal(9,"Reds")), limits = c(0,1),
							na.value = "white") +
		scale_y_discrete(limits = rev(levels(data_long$PercOfMiss_f)), breaks = y_breaks) +
		labs(
			fill = "Support",
			x = "Node",
			y = "Missing entries in terminals (%)",
			title = bquote("Zhang et al. 2023")
		) +
		theme_bw(base_size = 12) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 12),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 12),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
			plot.title = element_text(family = "Garamond", face = "bold", size = 12),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)	  
	
	pdf("SUPnMD_Zhang.pdf", width = 2.4, height = 9.6)
		p8e
	dev.off()

## All datasets
	pdf("TotalSUPnMD.pdf", width=19.2, height=9.6)
		grid.arrange(p1e, p2e, p3e, p4e, p5e, p6e, p7e, p8e, ncol = 8)
	dev.off()