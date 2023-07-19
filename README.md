[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8163958.svg)](https://doi.org/10.5281/zenodo.8163958)

## Sampedro-etal_ESR

**Integrated assessment of Russian gas cut-off for the European energy system**

Jon Sampedro<sup>1\*</sup>, Dirk-Jan Van de Ven<sup>1</sup>, Russell Horowitz<sup>1</sup>, Clàudia Rodés-Bachs<sup>2</sup>, Natasha Frilingou<sup>2</sup>, Alexandros Nikas<sup>2</sup>, Matthew Binsted<sup>3</sup>, Gokul Iyer<sup>3</sup>, and Brinda Yarlagadda<sup>3</sup>, 

<sup>1 </sup> Basque Center for Climate Change, Leioa, Spain

<sup>2 </sup> Energy Policy Unit, School of Electrical and Computer Engineering, National Technical University of Athens, Athens, Greece

<sup>3 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, United States of America

\* corresponding author:  jon.sampedro@bc3research.org

## Abstract
The reduction of European Union's pipeline gas imports from Russia as a consequence of the Russo-Ukrainian war has had severe economy-wide implications for the EU. Using a multisector integrated assessment model (GCAM), we find that a potential complete cut-off of Russian pipeline gas exports unevenly impacts the energy mix, prices, and trade flows of different subregions within the EU, depending on their access to alternative gas pipelines and LNG infrastructure. Moreover, there are also large changes in the volume and geographical distribution of global gas infrastructure capacity additions and stranded assets. Our results show that by significantly reducing demand for natural gas, the EU Fit-for-55 policy framework already delivers good resilience against a complete and persistent cut-off of Russian pipeline gas. However, further improvements in energy efficiency and renewable targets could further soften impacts, while bringing climate objectives closer in sight.

## Code reference
Available at Zenodo: https://zenodo.org/record/8163958

Clàudia Rodés Bachs, russellhz, & jonsampedro. (2023). bc3LC/Sampedro-etal_ESR: 1.0.0 (1.0.0). Zenodo. https://doi.org/10.5281/zenodo.8163958


## Contributing modeling software
| Model | Version | Repository Link 
|-------|---------|-----------------
| Global Change Analysis Model (GCAM) | Enhanced version of v-6.0| [https://github.com/jonsampedro/gcam-core](https://github.com/bc3LC/gcam-core/tree/GCAM-GasAnalysis) | 

| Component| Version | Repository Link 
|-------|---------|-----------------
| gcamdata | 1.0| https://github.com/JGCRI/gcamdata | 
| rgcam | 1.2.0| https://github.com/JGCRI/rgcam | 

## Reproduce the experiment
To reproduce the results and figures shown in Sampedro et al.,

1. Install `R` here - https://www.r-project.org/
2. Install `R studio` from here - https://www.rstudio.com/
3. Download input data from Zenodo
4. Run the script called `Results_gas_processing.R` chunk by chunk to generate the figures.  

Note that figures are generated for a suite of representative regions. The user could easily genearte figures for any of the 32 GCAM regions.

Similarly the user can easily change some additional settings (e.g., gases or palettes) in the first lines of the script
