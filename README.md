## Data and code repository

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17978442.svg)](https://doi.org/10.5281/zenodo.17978442)

This repository contains the data and scripts accompanying the study:

# Over-reliance on Completeness exposes Ultra Conserved Elements datasets to non-randomly distributed missing data

The manuscript has been accepted for publication in *Systematic Entomology*.

---

## Authors

**Ivonne J. Garzón Orduña**¹²†, **Andrew V. Z. Brower**³⁴⁵§, **Flavia R. Joele**¹‡, **Ambrosio Torres**⁶*

¹ Laboratorio de Sistemática de Polillas, Departamento de Zoología, Colección Nacional de Insectos, Instituto de Biología, UNAM, CDMX, México  
² California Academy of Sciences, 55 Music Concourse Dr, San Francisco, CA 94118, USA  
³ National Identification Services, Plant Protection and Quarantine, USDA-APHIS, 67 Thomas Johnson Dr., Frederick, MD, 21702, USA  
⁴ Division of Invertebrate Zoology, American Museum of Natural History, Central Park West at 79th Street, New York, NY, 10024, USA  
⁵ Department of Entomology, National Museum of Natural History, Smithsonian Institution, Washington, DC, 20013-7012, USA  
⁶ Center for Integrative Biodiversity Discovery, Leibniz Institute for Evolution and Biodiversity Science, Museum für Naturkunde, Invalidenstraße 43, 10115 Berlin, Germany  

† Ivonne.garzon@ib.unam.mx | https://orcid.org/0000-0003-3914-4952  
§ andrew.brower@usda.gov | https://orcid.org/0000-0001-6874-3589  
‡ flaviajoele@gmail.com | https://orcid.org/0009-0008-1027-3383  
\* **Corresponding author:** atorresgalvis@gmail.com | https://orcid.org/0000-0003-4505-5518  

---

## Abstract

As larger amounts of DNA sequence data become available, so does the need for heuristic metrics summarizing the quality of such datasets. This creates two seemly conflicting outcomes for the phylogenomics community: so called “point-and-click” algorithms make analysis of genomic data easier, but automation may perpetuate biases caused by uncritical reliance on algorithms. Here, based on nine Ultra Conserved Element (UCE) datasets from arthropods, we demonstrate that relying solely on the Completeness metric— a common tool to report amounts of missing data (MD) and inform choices about filtering loci—underestimates the actual amount of non-randomly distributed MD, and fails to prevent possible biases introduced by it. By only counting terminals sampled for each locus, Completeness overlooks the amount of MD in loci containing sequences that are truncated or gappy. Thus, datasets assessed for data quality only by this metric, and reported to be “complete”, can retain substantial MD concentrated in some terminals with extreme amounts of it, which may affect estimation of parameters often interpreted as biologically meaningful measures by phylogenetic algorithms (e.g., branch lengths). We demonstrate this by showing that MD could drive the generation of long branches, and affect the support values of nodes containing taxa with high amounts of MD. Finally, we offer six recommendations authors can employ alongside or alternatively to Completeness, to keep the community standards of data quality strong.

---

## Repository contents

This repository includes:
- Processed datasets used in the analyses
- Scripts for quantifying missing data and testing its effects
- Reproducible workflows to replicate all analyses reported in the paper

---

## Citation

If you use the data or code from this repository, please cite (along with the paper):
https://doi.org/10.5281/zenodo.17978442
