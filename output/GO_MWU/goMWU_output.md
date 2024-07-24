### High v Control 

BP

CC 

```
go.obo High_v_Control_gene_to_go.tab DE_High_v_Control_LFC.csv CC largest=0.5 smallest=5 cutHeight=0.25Run parameters:largest GO category as fraction of all genes (largest)  : 0.5         smallest GO category as # of genes (smallest)  : 5                clustering threshold (clusterCutHeight) : 0.25-----------------retrieving GO hierarchy, reformatting data...-------------go_reformat:Genes with GO annotations, but not listed in measure table: 1Terms without defined level (old ontology?..): 134--------------------------go_nrify:1782 categories, 7827 genes; size range 5-3913.5	14 too broad	746 too small	1022 remainingremoving redundancy:calculating GO term similarities based on shared genes...813 non-redundant GO categories of good size-------------Secondary clustering:calculating similarities....Continuous measure of interest: will perform MWU test188 GO terms at 10% FDR

bestGOs    delta.rank         pval level nseqs                  term            name        p.adj119       1298 5.807681e-47     5   664 GO:0005694;GO:0044427      chromosome 4.715837e-44258       -425 8.711973e-17     2  3339            GO:0016020        membrane 6.431020e-15343        779 1.779521e-04     2   116            GO:0030496         midbody 1.876586e-03779       -424 4.438827e-10     2  1283 GO:0120025;GO:0042995 cell projection 2.002404e-08
```

MF

```
go.obo High_v_Control_gene_to_go.tab DE_High_v_Control_LFC.csv MF largest=0.5 smallest=5 cutHeight=0.25Run parameters:largest GO category as fraction of all genes (largest)  : 0.5         smallest GO category as # of genes (smallest)  : 5                clustering threshold (clusterCutHeight) : 0.25-----------------retrieving GO hierarchy, reformatting data...-------------go_reformat:Genes with GO annotations, but not listed in measure table: 1Terms without defined level (old ontology?..): 118--------------------------go_nrify:3619 categories, 6950 genes; size range 5-3475	4 too broad	2143 too small	1472 remainingremoving redundancy:calculating GO term similarities based on shared genes...1133 non-redundant GO categories of good size-------------Secondary clustering:calculating similarities....Continuous measure of interest: will perform MWU test117 GO terms at 10% FDR

bestGOs    delta.rank         pval level nseqs                  term                          name        p.adj65         777 8.294551e-37     2  1288            GO:0003676          nucleic acid binding 9.389432e-34141      -1279 4.451359e-06     2    51            GO:0004222 metalloendopeptidase activity 1.679646e-04278       -411 3.418462e-07     1   670 GO:0005215;GO:0022857          transporter activity 1.758954e-05545       -482 9.979936e-08     2   519            GO:0016491       oxidoreductase activity 5.648644e-06806       2156 9.468307e-09     4    28            GO:0035173       histone kinase activity 7.655802e-07
```

### High v Mid 

CC 

```
go.obo High_v_Mid_gene_to_go.tab DE_High_v_Mid_LFC.csv CC largest=0.5 smallest=5 cutHeight=0.25Run parameters:largest GO category as fraction of all genes (largest)  : 0.5         smallest GO category as # of genes (smallest)  : 5                clustering threshold (clusterCutHeight) : 0.25-----------------retrieving GO hierarchy, reformatting data...-------------go_reformat:Genes with GO annotations, but not listed in measure table: 1Terms without defined level (old ontology?..): 136--------------------------go_nrify:1789 categories, 8081 genes; size range 5-4040.5	14 too broad	749 too small	1026 remainingremoving redundancy:calculating GO term similarities based on shared genes...812 non-redundant GO categories of good size-------------Secondary clustering:calculating similarities....Continuous measure of interest: will perform MWU test132 GO terms at 10% FDR

    delta.rank         pval level nseqs                             term                                              name        p.adj117        970 4.252474e-26     3   684            GO:0005694;GO:0044427                                        chromosome 3.448756e-23379       -335 2.520662e-08     3  1924                       GO:0031090                                organelle membrane 1.858415e-06469       -925 8.344373e-06     3   124 GO:0032838;GO:0097014;GO:0005930 plasma membrane bounded cell projection cytoplasm 2.706915e-04
```

MF

```
go.obo High_v_Mid_gene_to_go.tab DE_High_v_Mid_LFC.csv MF largest=0.5 smallest=5 cutHeight=0.25Run parameters:largest GO category as fraction of all genes (largest)  : 0.5         smallest GO category as # of genes (smallest)  : 5                clustering threshold (clusterCutHeight) : 0.25-----------------retrieving GO hierarchy, reformatting data...-------------go_reformat:Genes with GO annotations, but not listed in measure table: 1Terms without defined level (old ontology?..): 118--------------------------go_nrify:3627 categories, 7173 genes; size range 5-3586.5	4 too broad	2126 too small	1497 remainingremoving redundancy:calculating GO term similarities based on shared genes...1149 non-redundant GO categories of good size-------------Secondary clustering:calculating similarities....Continuous measure of interest: will perform MWU test96 GO terms at 10% FDR

     delta.rank         pval level nseqs       term                                                   name        p.adj70          606 2.614568e-22     3  1322 GO:0003676                                   nucleic acid binding 3.001524e-19561        -408 1.114202e-05     2   525 GO:0016491                                oxidoreductase activity 4.919632e-04821        1722 2.322141e-07     2    38 GO:0035173                                histone kinase activity 1.568128e-051080      -1661 7.294640e-05     2    24 GO:0072349 modified amino acid transmembrane transporter activity 2.326180e-031099       2090 6.285241e-13     2    50 GO:0099095         ligand-gated monoatomic anion channel activity 2.405152e-10
```

### Mid v Control

```
go.obo Mid_v_Control_gene_to_go.tab DE_Mid_v_Control_LFC.csv CC largest=0.5 smallest=5 cutHeight=0.25Run parameters:largest GO category as fraction of all genes (largest)  : 0.5         smallest GO category as # of genes (smallest)  : 5                clustering threshold (clusterCutHeight) : 0.25-----------------retrieving GO hierarchy, reformatting data...-------------go_reformat:Genes with GO annotations, but not listed in measure table: 1Terms without defined level (old ontology?..): 136--------------------------go_nrify:1789 categories, 8128 genes; size range 5-4064	14 too broad	738 too small	1037 remainingremoving redundancy:calculating GO term similarities based on shared genes...817 non-redundant GO categories of good size-------------Secondary clustering:calculating similarities....Continuous measure of interest: will perform MWU test160 GO terms at 10% FDR

    delta.rank         pval level nseqs       term     name        p.adj89         490 2.777626e-21     3  3783 GO:0005634  nucleus 2.266543e-18259       -387 1.027608e-13     2  3490 GO:0016020 membrane 1.397547e-11
```

MF

```
go.obo Mid_v_Control_gene_to_go.tab DE_Mid_v_Control_LFC.csv MF largest=0.5 smallest=5 cutHeight=0.25Run parameters:largest GO category as fraction of all genes (largest)  : 0.5         smallest GO category as # of genes (smallest)  : 5                clustering threshold (clusterCutHeight) : 0.25-----------------retrieving GO hierarchy, reformatting data...-------------go_reformat:Genes with GO annotations, but not listed in measure table: 1Terms without defined level (old ontology?..): 118--------------------------go_nrify:3627 categories, 7220 genes; size range 5-3610	4 too broad	2118 too small	1505 remainingremoving redundancy:calculating GO term similarities based on shared genes...1151 non-redundant GO categories of good size-------------Secondary clustering:calculating similarities....Continuous measure of interest: will perform MWU test104 GO terms at 10% FDR

     delta.rank         pval level nseqs                  term                                              name        p.adj71          405 1.127321e-10     2  1324            GO:0003676                              nucleic acid binding 1.080349e-08483        -907 3.836174e-22     3   522 GO:0015075;GO:0015318 monoatomic ion transmembrane transporter activity 4.411600e-19699       -3079 7.778216e-05     3     7 GO:0019865;GO:0019864                                       IgG binding 2.981649e-031127        665 2.067906e-04     3   135            GO:0140993                        histone modifying activity 6.427276e-03
```