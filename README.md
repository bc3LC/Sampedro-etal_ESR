ADD Zenodo Badge (connected to this GitHub repo)

## Sampedro-etal_ESR

**Integrated assessment of Russian gas cut-off for the European energy system and global gas infrastructure**

Jon Sampedro<sup>1\*</sup>, Dirk-Jan Van de Ven<sup>1</sup>, Russell Horowitz<sup>1</sup>, Clàudia Rodés-Bachs<sup>2</sup>, Natasha Frilingou<sup>2</sup>, Alexandros Nikas<sup>2</sup>, Matthew Binsted<sup>3</sup>, Gokul Iyer<sup>3</sup>, and Brinda Yarlagadda<sup>3</sup>, 

<sup>1 </sup> Basque Center for Climate Change, Leioa, Spain

<sup>2 </sup> Energy Policy Unit, School of Electrical and Computer Engineering, National Technical University of Athens, Athens, Greece

<sup>3 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, United States of America

\* corresponding author:  jon.sampedro@bc3research.org

## Abstract
ADD ABSTRACT

## Code reference
Available at Zenodo: ADD LINK to this repo linked in Zenodo

Add citation

## Data reference
Not needed? If yes, upload it to zeneodo as "Input data of" (Link to Zenodo)

Citation to Zenodo

## Contributing modeling software
| Model | Version | Repository Link 
|-------|---------|-----------------
| Global Change Analysis Model (GCAM) | Enhanced version of v-6.0| [https://github.com/jonsampedro/gcam-core](https://github.com/bc3LC/gcam-core/tree/GCAM-GasAnalysis) | 

| Component| Version | Repository Link 
|-------|---------|-----------------
| gcamdata | 1.0| https://github.com/JGCRI/gcamdata | 
| rgcam | 1.2.0| https://github.com/JGCRI/rgcam | 

## Reproduce my experiment
To reproduce the results and figures shown in Sampedro et al.,

1. Install `R` here - https://www.r-project.org/
2. Install `R studio` from here - https://www.rstudio.com/
3. Download input data from Zenodo
4. Run the script called `Results_processing` chunk by chunk to generate the figures.  

Note that figures are generated for a suite of representative regions. The user could easily genearte figures for any of the 32 GCAM regions.

Similarly the user can easily change some additional settings (e.g., gases or palettes) in the first lines of the script
