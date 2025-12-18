## Title: script to build the graphics of Total Missing Data (i.e. % of missing entries) per terminals.
## Article: "Over-reliance on Completeness exposes Ultra Conserved Elements datasets to non-randomly distributed missing data" (Garzón-Orduña et al. 2025)
## Date: 2025-09-22
## Author: Ambrosio Torres (Researcher [Ctr. Integr. Biodivers. Discov. - Museum für Naturkunde, Berlin, Germany)
## Maintainer: Ambrosio Torres <atorresgalvis@gmail.com> <Ambrosio.TorresGalvis@mfn.berlin>
## Depends: R version (>= 4.4.1 ). Packages Cairo (1.6-2); ggplot2 (3.5.2); gridExtra (2.3)
## License: GPL (3)

#########
## Load the packages and fonts needed
#########
	library(Cairo)
	library(ggplot2)
	library(gridExtra)
	# extrafont::font_import()
	extrafont::loadfonts()

#########
## Fuction to jitter the outliers of the boxplots	
#########
	GeomPointRast <- ggplot2::ggproto(
	"GeomPointRast",
	ggplot2::GeomPoint,
	draw_panel = function(self, data, panel_params, coord, raster.dpi, dev, scale) {
		grob <- ggproto_parent(GeomPoint, self)$draw_panel(data, panel_params, coord)
		class(grob) <- c("rasteriser", class(grob))
		grob$dpi <- raster.dpi
		grob$dev <- dev
		grob$scale <- scale
		return(grob)
	}
	)
	
	DrawGeomBoxplotJitter <- function(data, panel_params, coord, dev="cairo", ...,
									outlier.jitter.width=NULL,
									outlier.jitter.height=0,
									outlier.colour = NULL,
									outlier.fill = NULL,
									outlier.shape = 19,
									outlier.size = 1.5,
									outlier.stroke = 0.5,
									outlier.alpha = NULL,
									raster=FALSE, raster.dpi=getOption("ggrastr.default.dpi", 300),
									raster.width=NULL, raster.height=NULL,
									scale = 1
									) {
	boxplot_grob <- ggplot2::GeomBoxplot$draw_group(data, panel_params, coord, ...)
	point_grob <- grep("geom_point.*", names(boxplot_grob$children))
	if (length(point_grob) == 0){
		return(boxplot_grob)
	}
	
	ifnotnull <- function(x, y) if(is.null(x)) y else x
	
	if (is.null(outlier.jitter.width)) {
		outlier.jitter.width <- (data$xmax - data$xmin) / 2
	}
	
	x <- data$x[1]
	y <- data$outliers[[1]]
	if (outlier.jitter.width > 0 & length(y) > 1) {
		x <- jitter(rep(x, length(y)), amount=outlier.jitter.width)
	}
	
	if (outlier.jitter.height > 0 & length(y) > 1) {
		y <- jitter(y, amount=outlier.jitter.height)
	}
	
	outliers <- data.frame(
		x = x, y = y,
		colour = ifnotnull(outlier.colour, data$colour[1]),
		fill = ifnotnull(outlier.fill, data$fill[1]),
		shape = ifnotnull(outlier.shape, data$shape[1]),
		size = ifnotnull(outlier.size, data$size[1]),
		stroke = ifnotnull(outlier.stroke, data$stroke[1]),
		fill = NA,
		alpha = ifnotnull(outlier.alpha, data$alpha[1]),
		stringsAsFactors = FALSE
	)
	
 	boxplot_grob$children[[point_grob]] <- GeomPointRast$draw_panel(outliers, panel_params, coord, raster.dpi=raster.dpi, dev=dev, scale = scale)
	
	return(boxplot_grob)
	}
	
	GeomBoxplotJitter <- ggplot2::ggproto("GeomBoxplotJitter",
											ggplot2::GeomBoxplot,
											draw_group = DrawGeomBoxplotJitter)

	#' This geom is similar to \code{\link[ggplot2]{geom_boxplot}}, but allows to jitter outlier points and to raster points layer.
	#'
	#' @inheritParams ggplot2::geom_boxplot
	#' @inheritSection ggplot2::geom_boxplot Aesthetics
	#'
	#' @param outlier.jitter.width numeric Amount of horizontal jitter (default=NULL). The jitter is added in both positive and negative directions,
	#' so the total spread is twice the value specified here. If NULL, no jitter performed.
	#' @param outlier.jitter.height numeric Amount of horizontal jitter (default=0). The jitter is added in both positive and negative directions,
	#' so the total spread is twice the value specified here. 
	#' @param raster.dpi integer Resolution of the rastered image (default=300). Ignored if \code{raster == FALSE}.
	#' @param dev string Specifies the device used, which can be one of: \code{"cairo"}, \code{"ragg"} or \code{"ragg_png"} (default="cairo").
	#' @param stat string The statistical transformation to use on the data for this layer, either as a ggproto Geom subclass or as a string naming the stat stripped of the stat_ prefix (e.g. "count" rather than "stat_count"). Refer to ggplot2::layer.
	#' @param scale numeric Scaling factor to modify the raster object size (default=1). The parameter 'scale=1' results in an object size that is unchanged, 'scale'>1 increase the size, and 'scale'<1 decreases the size. These parameters are passed to 'height' and 'width' of grid::grid.raster(). Please refer to 'rasterise()' and 'grid::grid.raster()' for more details.
	#' @return geom_boxplot plot with rasterized layer
	#'
	#' @examples
	#' library(ggplot2)
	#' library(ggrastr)
	#'
	#' yvalues = rt(1000, df=3)
	#' xvalues = as.factor(1:1000 %% 2)
	#' ggplot() + geom_boxplot_jitter(aes(y=yvalues, x=xvalues), outlier.jitter.width = 0.1, raster = TRUE)
	#'
	#' @export
	geom_boxplot_jitter <- function(mapping = NULL, data = NULL, dev = "cairo",
									stat = "boxplot", position = "dodge",
									na.rm = FALSE, show.legend = NA,
									inherit.aes = TRUE, ...,
									outlier.jitter.width=NULL,
									outlier.jitter.height=0,
									raster.dpi=getOption("ggrastr.default.dpi", 300),
									scale = 1
									) {
	ggplot2::layer(
		geom = GeomBoxplotJitter, mapping = mapping, data = data, stat = stat,
		position = position, show.legend = show.legend, inherit.aes = inherit.aes,
		params = list(na.rm = na.rm,
					outlier.jitter.width=outlier.jitter.width,
					outlier.jitter.height=outlier.jitter.height,
					raster.dpi=raster.dpi, dev=dev, scale = scale, ...))
	}

## Blaimer
	#Obtain the list of the Completeness percentage of all loci
	#The file "Blaimer50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Blaimer50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_uce", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*_", "uce_", y)
	writeLines(c("Locus,CompletenessValue", y), "BlaimerCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Blaimer50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Blaimer50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("BlaimerCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTBlaimer.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTBlaimer.tnt")	
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeBlaimer")
	cortes <- c("50", "55", 
				"60", "65",
				"70", "75", 
				"80", "85"
				)
	superarchi <- NULL
	for (i in 1:length(files)) {
		archi <- read.csv(files[i], header=T)
		corte <- rep(cortes[i], dim(archi)[1])
		frag <- cbind(archi, corte)
		superarchi <- rbind(superarchi, frag)
	}
	superarchi$corte <- factor(superarchi$corte, 
								levels = c("50", "55", 
								"60", "65",
								"70", "75", 
								"80", "85"
								))
									
	p1 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Blaimer et al. 2023 (771 taxa; 1118 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesBlaimer.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p1
	dev.off()

## Borowiec
	#Obtain the list of the Completeness percentage of all loci
	#The file "Borowiec50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Borowiec50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("uce-", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*_", "", y)
	writeLines(c("Locus,CompletenessValue", y), "BorowiecCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Borowiec50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Borowiec50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("BorowiecCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTBorowiec.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTBorowiec.tnt")	
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeBorowiec")
	cortes <- c("50", "55", 
				"60", "65",
				"70", "75", 
				"80", "85",
				"90", "95"
				)
	superarchi <- NULL
	for (i in 1:length(files)) {
		archi <- read.csv(files[i], header=T)
		corte <- rep(cortes[i], dim(archi)[1])
		frag <- cbind(archi, corte)
		superarchi <- rbind(superarchi, frag)
	}
	superarchi$corte <- factor(superarchi$corte, 
								levels = c("50", "55", 
								"60", "65",
								"70", "75", 
								"80", "85",
								"90", "95"
								))
									
	p2 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Borowiec et al. 2025 (292 taxa; 1286 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesBorowiec.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p2
	dev.off()
	
## Buenaventura
	#Obtain the list of the Completeness percentage of all loci
	#The file "Buenaventura50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Buenaventura50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("uce-", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*_", "", y)
	writeLines(c("Locus,CompletenessValue", y), "BuenaventuraCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Buenaventura50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Buenaventura50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("BuenaventuraCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTBuenaventura.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTBuenaventura.tnt") #Include an "a" at the end of the name of each locus in this file
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeBuenaventura")
	cortes <- c("50", "55", 
				"60", "65",
				"70", "75", 
				"80", "85",
				"90"
				)
	superarchi <- NULL
	for (i in 1:length(files)) {
		archi <- read.csv(files[i], header=T)
		corte <- rep(cortes[i], dim(archi)[1])
		frag <- cbind(archi, corte)
		superarchi <- rbind(superarchi, frag)
	}
	superarchi$corte <- factor(superarchi$corte, 
								levels = c("50", "55", 
								"60", "65",
								"70", "75", 
								"80", "85",
								"90"
								))
									
	p3 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Buenaventura et al. 2021 (100 taxa; 725 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesBuenaventura.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p3
	dev.off()	
	
## Cohen
	#Obtain the list of the Completeness percentage of all loci
	#The file "Cohen50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Cohen50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_uce", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*_", "", y)
	writeLines(c("Locus,CompletenessValue", y), "CohenCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Cohen50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Cohen50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("CohenCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTCohen.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTCohen.tnt")
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeCohen")
	cortes <- c("50", "55", 
				"60", "65",
				"70", "75", 
				"80", "85"
				)
	superarchi <- NULL
	for (i in 1:length(files)) {
		archi <- read.csv(files[i], header=T)
		corte <- rep(cortes[i], dim(archi)[1])
		frag <- cbind(archi, corte)
		superarchi <- rbind(superarchi, frag)
	}
	superarchi$corte <- factor(superarchi$corte, 
								levels = c("50", "55", 
								"60", "65",
								"70", "75", 
								"80", "85"
								))
									
	p4 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Cohen et al. 2021 (160 taxa; 591 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesCohen.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p4
	dev.off()

## Galleti
	#Obtain the list of the Completeness percentage of all loci
	#The file "Galleti50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Galleti50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_uce", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*uce_", "uce_", y)
	writeLines(c("Locus,CompletenessValue", y), "GalletiCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Galleti50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Galleti50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("GalletiCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTGalleti.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTGalleti.tnt")
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeGalleti")
		cortes <- c("100", "50", "55", 
					"60", "65",
					"70", "75", 
					"80", "85",
					"90", "95"
					)
		superarchi <- NULL
		for (i in 1:length(files)) {
			archi <- read.csv(files[i], header=T)
			corte <- rep(cortes[i], dim(archi)[1])
			frag <- cbind(archi, corte)
			superarchi <- rbind(superarchi, frag)
		}
		superarchi$corte <- factor(superarchi$corte, 
									levels = c("50", "55", 
										"60", "65",
										"70", "75", 
										"80", "85",
										"90", "95",
										"100"))
									
	p5 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Galleti-Lima et al. 2023 (63 taxa; 1375 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesGalleti.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p5
	dev.off()	


## Joele
	#Obtain the list of the Completeness percentage of all loci
	#The file "Joele50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Joele50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_uce", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*uce_", "uce_", y)
	writeLines(c("Locus,CompletenessValue", y), "JoeleCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Joele50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Joele50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("JoeleCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTJoele.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTJoele.tnt")
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeJoele")
		cortes <- c("100", "50", "55", 
					"60", "65",
					"70", "75", 
					"80", "85",
					"90", "95"
					)
		superarchi <- NULL
		for (i in 1:length(files)) {
			archi <- read.csv(files[i], header=T)
			corte <- rep(cortes[i], dim(archi)[1])
			frag <- cbind(archi, corte)
			superarchi <- rbind(superarchi, frag)
		}
		superarchi$corte <- factor(superarchi$corte, 
									levels = c("50", "55", 
										"60", "65",
										"70", "75", 
										"80", "85",
										"90", "95",
										"100"))
									
	p6 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Joele et al. 2024 (73 taxa; 909 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesJoele.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p6
	dev.off()	

## Reyes
	#Obtain the list of the Completeness percentage of all loci
	#The file "Reyes40p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Reyes40p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_uce", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*uce_", "uce_", y)
	writeLines(c("Locus,CompletenessValue", y), "ReyesCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Reyes40p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Reyes40p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("ReyesCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTReyes.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTReyes.tnt")
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeReyes")
	cortes <- c("50", "55", 
				"60", "65",
				"70", "75", 
				"80", "85",
				"90"
				)
	superarchi <- NULL
	for (i in 1:length(files)) {
		archi <- read.csv(files[i], header=T)
		corte <- rep(cortes[i], dim(archi)[1])
		frag <- cbind(archi, corte)
		superarchi <- rbind(superarchi, frag)
	}
	superarchi$corte <- factor(superarchi$corte, 
								levels = c("50", "55", 
								"60", "65",
								"70", "75", 
								"80", "85",
								"90"
								))
									
	p7 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Reyes-Hernández et al. 2025 (100 taxa; 530 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesReyes.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p7
	dev.off()	
	
## Santos
	#Obtain the list of the Completeness percentage of all loci
	#The file "Santos50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Santos50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_UCE", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*_", "", y)
	writeLines(c("Locus,CompletenessValue", y), "SantosCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Santos50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Santos50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("SantosCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTSantos.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTSantos.tnt")
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeSantos")
		cortes <- c("100", "50", "55", 
					"60", "65",
					"70", "75", 
					"80", "85",
					"90", "95"
					)
		superarchi <- NULL
		for (i in 1:length(files)) {
			archi <- read.csv(files[i], header=T)
			corte <- rep(cortes[i], dim(archi)[1])
			frag <- cbind(archi, corte)
			superarchi <- rbind(superarchi, frag)
		}
		superarchi$corte <- factor(superarchi$corte, 
									levels = c("50", "55", 
										"60", "65",
										"70", "75", 
										"80", "85",
										"90", "95",
										"100"))
									
	p8 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Santos & Brady 2024 (235 taxa; 1330 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesSantos.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p8
	dev.off()	
	
## Zhang
	#Obtain the list of the Completeness percentage of all loci
	#The file "Zhang50p.svg" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	y <- readLines("Zhang50p.svg")
	indexo <- grep("/text", y)
	y <- y[indexo]
	indexo2 <- grep("_uce", y)
	y <- y[indexo2]
	y <- gsub(".*\">", "", y); y <- gsub(") </text> ", "", y)
	y <- gsub("[(]", ",", y); y <- gsub(" ", "", y)
	y <- gsub(".*_", "", y)
	writeLines(c("Locus,CompletenessValue", y), "ZhangCompletenessGenes.csv")

	#Obtain the list of the loci included in each Completeness value. 
	#This list will be used to subset the matrix in TNT using the script "ExTotalMiss.run"
	#The file "MissingInformation_Zhang50p.csv" was obtained using the TNT script 
	#  GeneOccupancy.run (Torres et al., 2022)
	MissInfo <- read.csv("MissingInformation_Zhang50p.csv", header= T)
	colnames(MissInfo) <- gsub("\\.", "-", colnames(MissInfo))
	CompValues <- read.csv("ZhangCompletenessGenes.csv", header= T)
	
	cortelos <- seq(50 , floor(max(CompValues$CompletenessValue)), by=5)
	TablaMediaMissSpe <- NULL
	totalTNT <- NULL
	for (i in cortelos ) {
		indexado <- CompValues[CompValues$CompletenessValue >= i,]
		componentes <- indexado$Locus
		lineaTNT <- paste("blocks =", paste(componentes, collapse= " "), ";")
		totalTNT <- paste(totalTNT, lineaTNT, collapse= "\n")
		writeLines(lineaTNT, "ParaTNTZhang.tnt")
		subtabla <- MissInfo[,componentes]
		if (length(componentes) > 1 ) {
			MediaMissSpe <- rowMeans(subtabla, na.rm = T)
		} else {
			MediaMissSpe <- subtabla
		}	
		TablaMediaMissSpe <- cbind(TablaMediaMissSpe, MediaMissSpe)
	}
	#TablaMediaMissSpe <- cbind(MissInfo[,1], TablaMediaMissSpe)
	writeLines(totalTNT, "ParaTNTZhang.tnt")
	colnames(TablaMediaMissSpe) <- cortelos

	#Merge all files of Total Missing per species of all Completeness values 
	# (Obtained with the TNT script "ExTotalMiss.run")
	files <- list.files(pattern = "TotalMissSpeZhang")
		cortes <- c("50", "55", 
					"60", "65",
					"70", "75", 
					"80", "85",
					"90", "95"
					)
		superarchi <- NULL
		for (i in 1:length(files)) {
			archi <- read.csv(files[i], header=T)
			corte <- rep(cortes[i], dim(archi)[1])
			frag <- cbind(archi, corte)
			superarchi <- rbind(superarchi, frag)
		}
		superarchi$corte <- factor(superarchi$corte, 
									levels = c("50", "55", 
										"60", "65",
										"70", "75", 
										"80", "85",
										"90", "95"))
									
	p9 <- ggplot(superarchi, aes(x = corte, y = TotalMiss)) + 
		  stat_boxplot(color = "black", geom = "linerange", width = 0.2, lwd = 1.5) +
		  geom_boxplot_jitter(
		 	 outlier.shape = 21,
		 	 outlier.colour = "black",
		 	 outlier.stroke = 0.3,
		 	 outlier.fill = "#e02b35",
		 	 fatten = NA, lwd = NA, size = NA, alpha = 0,
		 	 outlier.jitter.width = 0.23,
		 	 raster = TRUE,
		 	 outlier.size = 1.95,
		 	 outlier.alpha = 0.5
		  ) +
		  stat_summary(color = "black", fun = mean, geom = "point", size = 5.25) +
		  scale_y_continuous(limits = c(0, 100), breaks = seq(25, 100, by = 25), expand = c(0.02, 0.01)) +
		  xlab("Completeness value (p)") + 
		  ylab("Total missing entries (%)") +
		  ggtitle("Zhang et al. 2023 (57 taxa; 3271 UCEs)") +
		  theme_bw(base_size = 16) +
		  theme(
		 	 axis.title.x = element_text(family = "Garamond", face = "bold", size = 10),
		 	 axis.title.y = element_text(family = "Garamond", face = "bold", size = 10),
		 	 text = element_text(family = "Garamond"),
		 	 axis.text.x = element_text(angle = 45, hjust = 1.0, size = 16),
			 axis.text.y = element_text(angle = 0, vjust = 0.5, size = 16),
		 	 plot.title = element_text(family = "Garamond", face = "bold", size = 13),
		 	 legend.position = "none",
		 	 panel.grid.major.x = element_blank(),
		 	 panel.grid.minor.x = element_blank(),
		 	 panel.grid.minor.y = element_blank(),
		 	 panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted', size = 0.75)
		  )
							
	pdf("TotalMissPerSpeciesZhang.pdf", width=3.0, height=3.5) #Cantidad total de Missing por especie.
		p9
	dev.off()	
	
	
## All datasets	
	pdf("TotalMissPerSpeciesAll.pdf", width=12.0, height=10.5)
		grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)
	dev.off()