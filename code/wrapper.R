### Wrapper for creating separate region-specific bibliographies 
### Written by Britt Bertolet
### 2021-12-09

# Load libraries 
library(stringr)
library(tidyverse)
library(ggplot2)
library(cowplot)

# Load function
source('code/sortAffiliations.R')

# Set variables needed to run function
table=sortAffiliations(file="D_scopus.bib", n = 200, GNout="D_scopus_GN.bib", GSout="D_scopus_GS.bib", dir="bibs")

# Check for entries with multiple affiliations of the first author
table[table$citation%in%table$citation[duplicated(table$citation)],]

# Plot affiliations for all 200 citations
(all200=ggplot(table, aes(x=country))+
  geom_bar()+
  facet_grid(.~designation, scales = "free_x", space = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        panel.grid = element_blank(),
        axis.title.x = element_blank())+
  ylab("Count"))

# Plot affiliations for only top 20 for both designations
GN=table[table$designation =="Global North",]
GN=GN[1:20,]
GS=table[table$designation =="Global South",]
top20=rbind(GN, GS)

top=ggplot(top20, aes(x=country))+
  geom_bar()+
  facet_grid(.~designation, scales = "free_x", space = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        panel.grid = element_blank())+
  ylab("Count")+
  xlab("\nCountry of affiliation of first-author")

plot_grid(all200, top, 
          nrow=2, align = "h", 
          labels=c("A", "B"))
ggsave("D_scopus_affiliations.pdf", height=6, width=6)
  
