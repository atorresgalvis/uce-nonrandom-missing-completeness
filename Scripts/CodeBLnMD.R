## Title: script to build the graphics of the correlation between branch lengths and missing data of the terminals.
## Article: "Over-reliance on Completeness exposes Ultra Conserved Elements datasets to non-randomly distributed missing data" (Garzón-Orduña et al. 2025)
## Date: 2025-09-22
## Author: Ambrosio Torres (Researcher [Ctr. Integr. Biodivers. Discov. - Museum für Naturkunde, Berlin, Germany)
## Maintainer: Ambrosio Torres <atorresgalvis@gmail.com> <Ambrosio.TorresGalvis@mfn.berlin>
## Depends: R version (>= 4.4.1 ). Packages Cairo (1.6-2); dplyr (1.1.4); e1071 (1.7-16); ggplot2 (3.5.2); gridExtra (2.3); stringr (1.5.1)
## License: GPL (3)

#########
## Load the packages and fonts needed
#########
	library(Cairo)
	library(dplyr)
	library(e1071)
	library(ggplot2)
	library(gridExtra)
	library(stringr)
	# extrafont::font_import()
	extrafont::loadfonts()

# Branch lengths were taken from the trees provided by the authors of the datasets in the supplementary material. 
# The percentage of missing data was calculated using the same matrix employed to build the selected 
# tree, through the TNT script GeneOccupancy.run (Torres et al. 2022).

## Blaimer
	info <- read.csv("BLnMD_Blaimer2023.csv", header = TRUE)
	info <- subset(info, Position == "Ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalBlaimer <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalBlaimer$estimate, 2)
	p_global <- signif(kendall_globalBlaimer$p.value, 2)
	
	p100 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
		geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
		geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
		labs(
			x = "Missing data (%)",
			y = "Branch lengths",
			title = bquote("Blaimer et al. 2023 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
		) +
		theme_bw(base_size = 16) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1.0),
			plot.title = element_text(family = "Garamond", face = "bold", size = 14),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)
	
	pdf("BLnMD_Blaimer_ingroup.pdf", width = 5.0, height = 3.0)
	p100
	dev.off()

## Borowiec
	info <- read.csv("BLnMD_Borowiec2025.csv", header = TRUE)
	info <- subset(info, Position == "Ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalBorowiec <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalBorowiec$estimate, 2)
	p_global <- signif(kendall_globalBorowiec$p.value, 2)
	
	p200 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
		geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
		geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
		labs(
			x = "Missing data (%)",
			y = "Branch lengths",
			title = bquote("Borowiec et al. 2025 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
		) +
		theme_bw(base_size = 16) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1.0),
			plot.title = element_text(family = "Garamond", face = "bold", size = 14),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)
	
	pdf("BLnMD_Borowiec_ingroup.pdf", width = 5.0, height = 3.0)
	p200
	dev.off()

## Buenaventura		
	info <- read.csv("BLnMD_Buenaventura2021.csv", header = TRUE)
	info <- subset(info, Position == "ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalBuenaventura <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalBuenaventura$estimate, 2)
	p_global <- signif(kendall_globalBuenaventura$p.value, 2)
	
	p300 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
	geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
	geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
	labs(
		x = "Missing data (%)",
		y = "Branch lengths",
		title = bquote("Buenaventura et al. 2021 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
	) +
	theme_bw(base_size = 16) +
	theme(
		axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
		axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
		text = element_text(family = "Garamond"),
		axis.text.x = element_text(angle = 45, hjust = 1.0),
		plot.title = element_text(family = "Garamond", face = "bold", size = 14),
		panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.y = element_blank()
	)
	
	pdf("BLnMD_Buenaventura_ingroup.pdf", width = 5.0, height = 3.0)
	p300
	dev.off()

## Galleti
	info <- read.csv("BLnMD_Galleti2023.csv", header = TRUE)
	info <- subset(info, Position == "Ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalGalleti <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalGalleti$estimate, 2)
	p_global <- signif(kendall_globalGalleti$p.value, 2)
	
	p400 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
		geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
		geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
		labs(
			x = "Missing data (%)",
			y = "Branch lengths",
			title = bquote("Galleti-Lima et al. 2023 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
		) +
		theme_bw(base_size = 16) +
		theme(
			axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
			axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
			text = element_text(family = "Garamond"),
			axis.text.x = element_text(angle = 45, hjust = 1.0),
			plot.title = element_text(family = "Garamond", face = "bold", size = 14),
			panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.x = element_blank(),
			panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
			panel.grid.minor.y = element_blank()
		)
	
	pdf("BLnMD_Galleti_ingroup.pdf", width = 5.0, height = 3.0)
	p400
	dev.off()
	
	
## Joele
	info <- read.csv("BLnMD_Joele2024.csv", header = TRUE)
	info <- subset(info, Position == "ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalJoele <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalJoele$estimate, 2)
	p_global <- signif(kendall_globalJoele$p.value, 2)
	
	p500 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
	geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
	geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
	labs(
		x = "Missing data (%)",
		y = "Branch lengths",
		title = bquote("Joele et al. 2024 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
	) +
	theme_bw(base_size = 16) +
	theme(
		axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
		axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
		text = element_text(family = "Garamond"),
		axis.text.x = element_text(angle = 45, hjust = 1.0),
		plot.title = element_text(family = "Garamond", face = "bold", size = 14),
		panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.y = element_blank()
	)
	
	pdf("BLnMD_Joele_ingroup.pdf", width = 5.0, height = 3.0)
	p500
	dev.off()
	
## Reyes
	info <- read.csv("BLnMD_Reyes2025.csv", header = TRUE)
	info <- subset(info, Position == "Ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalReyes <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalReyes$estimate, 2)
	p_global <- signif(kendall_globalReyes$p.value, 2)
	
	p600 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
	geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
	geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
	labs(
		x = "Missing data (%)",
		y = "Branch lengths",
		title = bquote("Reyes et al. 2025 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
	) +
	theme_bw(base_size = 16) +
	theme(
		axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
		axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
		text = element_text(family = "Garamond"),
		axis.text.x = element_text(angle = 45, hjust = 1.0),
		plot.title = element_text(family = "Garamond", face = "bold", size = 14),
		panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.y = element_blank()
	)
	
	pdf("BLnMD_Reyes_ingroup.pdf", width = 5.0, height = 3.0)
	p600
	dev.off()
	
## Santos
	info <- read.csv("BLnMD_SantosBrady2024.csv", header = TRUE)
	info <- subset(info, Position == "Ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalSantos <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalSantos$estimate, 2)
	p_global <- signif(kendall_globalSantos$p.value, 2)
	
	p700 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
	geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
	geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
	labs(
		x = "Missing data (%)",
		y = "Branch lengths",
		title = bquote("Santos & Brady 2024 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
	) +
	theme_bw(base_size = 16) +
	theme(
		axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
		axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
		text = element_text(family = "Garamond"),
		axis.text.x = element_text(angle = 45, hjust = 1.0),
		plot.title = element_text(family = "Garamond", face = "bold", size = 14),
		panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.y = element_blank()
	)
	
	pdf("BLnMD_Santos_ingroup.pdf", width = 5.0, height = 3.0)
	p700
	dev.off()
	
## Zhang
	info <- read.csv("BLnMD_Zhang2023.csv", header = TRUE)
	info <- subset(info, Position == "Ingroup")
	
	hist(info$PercOfMiss, main = "Histogram of PercOfMiss (ingroup)", col = "skyblue", xlab = "PercOfMiss")
	hist(info$BranchLength, main = "Histogram of BranchLength (ingroup)", col = "tomato", xlab = "BranchLength")
	
	qqnorm(info$PercOfMiss); qqline(info$PercOfMiss, col = "blue")
	qqnorm(info$BranchLength); qqline(info$BranchLength, col = "blue")
	
	shapiro.test(info$PercOfMiss)
	shapiro.test(info$BranchLength)
	
	kendall_globalZhang <- cor.test(info$PercOfMiss, info$BranchLength, method = "kendall")
	tau_global <- round(kendall_globalZhang$estimate, 2)
	p_global <- signif(kendall_globalZhang$p.value, 2)
	
	p800 <- ggplot(info, aes(x = PercOfMiss, y = BranchLength)) +
	geom_point(shape = 21, size = 1.3, color = "black", stroke = 0.2,
				fill = "#377EB8", alpha = 0.6) +
	geom_smooth(method = "lm", se = TRUE, color = "#E41A1CFF", linewidth = 1.0) +
	labs(
		x = "Missing data (%)",
		y = "Branch lengths",
		title = bquote("Zhang et al. 2023 (ingroup, " * tau == .(tau_global) * "; p = " * .(p_global) * ")")
	) +
	theme_bw(base_size = 16) +
	theme(
		axis.title.x = element_text(family = "Garamond", face = "bold", size = 8),
		axis.title.y = element_text(family = "Garamond", face = "bold", size = 8),
		text = element_text(family = "Garamond"),
		axis.text.x = element_text(angle = 45, hjust = 1.0),
		plot.title = element_text(family = "Garamond", face = "bold", size = 14),
		panel.grid.major.x = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.5),
		panel.grid.minor.y = element_blank()
	)
	
	pdf("BLnMD_Zhang_ingroup.pdf", width = 5.0, height = 3.0)
	p800
	dev.off()
	
## All datasets
	pdf("TotalBLnMD_Ingroup.pdf", width=8.0, height=10.0)
		grid.arrange(p100, p200, p300, p400, p500, p600, p700, p800, ncol = 2)
	dev.off()	