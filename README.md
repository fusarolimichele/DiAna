# DiAna
Repository for FAERS cleaning and analysis

# Download the ZIP with the FAERS dataset cleaned according to the scripts here documented: 
https://osf.io/zqu89/files/osfstorage


DiAna (Disproportionality Analyses) is a project to develop a shared procedure to clean and analyse the FDA Adverse Event Reporting System database for signal detection.

FDA makes available different downloadable dataframes (xml, ascii), that differ partly from each other and from the public dashboard. Furthermore, these databases have duplicates and are not entirely standardized. Authors are required to perform a cleaning procedure before conducting the analyses, and different choices in this step can result in even more different and less comparable dataframes, with the result that two studies on the same subject using the FAERS database may retrieve a different number of cases and may detect different signals. Many private platforms are keeping their cleaning procedure secret and make available, for a price, a support service for signal detection. Other databases have been made public and available to everyone but are not updated anymore, or their pre-processing is not transparent and it is proposed as made of objective choice. Further, many public dashboards allow some degree of descriptive and disproportionality analyses, but they are often non-flexible and don’t allow to design and implement specific strategies to correct for the many biases that affect pharmacovigilance. Also, some packages have been developed to increase the feasibility, but these are often poorly documented and offer the possibility to only delve the surface of signal detection.

In this project, we discuss the different choices that have to be done when cleaning the FAERS database. We design a shared and transparent cleaning procedure using the R software. We share it together with the cleaned database, with the idea that an open source, clearly-described procedure may build the basis for a global common effort towards a shared cleaning procedure.

The next step will be to build a connected package for standardized analyses on the cleaned database, with the idea that also this tool may be implemented with time through a common effort to allow everyone more and more complex and in-deep analyses of post-marketing drug safety. A common package could allow also to share innovative pharmacovigilance tools.


Stages

[X] Mapping of existing opportunities to access the FAERS

[X] DiAna dictionary for drug name standardization (continuous refinement)

[X] DiAna access to a clean FAERS (continuous refinement)

[ ] Probabilistic deduplication (work in progress)

[ ] Imputation of missing data

[ ] DiAna R package for drug safety investigations

[ ] Linkage with other ontologies


System and Software
The script was tested on the following operative systems:

Michele’s Macbook

- **Platform**: aarch64-apple-darwin20
- **Language**: R version 4.2.1 (2022-06-23)
- **Software**: RStudio Version 2022.07.2+576

Valentina’s Windows

- **Platform**: Windows 10 Pro Version 21H1
- **RAM**: 8 GB
- **Processor**: Intel Core i57200U CPU 2.50 GHz
- **Language**: R version 4.2.1 ((2022-06-23 Funny-Looking Kid)
- **Software**: RStudio Version 2022.07.1+554

Santa’s Windows

- **Platform**: Windows 10 Pro Version 21H1
- **RAM**: 16 GB
- **Processor**: Intel Core i5-4590 CPU 3.30 GHz
- **Language**: R version 4.2.1 (2022-06-23 Funny-Looking Kid)
- **Software**: RStudio Version 2022.07.2+576

