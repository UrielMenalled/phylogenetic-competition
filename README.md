# Does the phylogenetic relatedness of cover crops impact their competition with weeds?

These files contain the data and code that [Menalled et al. (2023)](https://doi.org/10.1038/s41598-023-43987-x) used to test how the relatedness between cover crops and weeds affects weed suppression and community assembly. The data was generated through winter and summer field experiments replicated for four site-years. Each site-year consisted of four cover crop treatments and one tilled control, replicated in four blocks for a total of 20 plots per site-year. Plant biomass was sampled at the end of the cover crop growing season in two 0.25 m<sup>2</sup> (76 cm × 33 cm) quadrats randomly placed in each plot. During sampling, each quadrat was placed perpendicularly over four crop rows to ensure a consistent proportion of crop row and interrow area. To avoid pseudoreplication, samples were summed at the plot level. The samples were dried at 60 °C for at least 2 weeks before obtaining dry weight for analysis. Results show that cover crops can modify weed community structure by suppressing phylogenetically related weed species more intensely than phylogenetically unrelated weeds. All analyses and results are reported and discussed in Menalled et al. (2023): “[Phylogenetic relatedness can influence cover crop-based weed suppression](https://doi.org/10.1038/s41598-023-43987-x).”

## File structure

```         
Code/
    ├── COOP weed data analysis log.docx   #Description of analysis
    ├── phylogenetic-competition.md   #Markdown of analysis
    └── phylogenetic-competition.Rmd   #Analysis code
Data/
    ├── COOP1_weedsHighRes.xlsx   #Summer experiment, rep 1; both sites
    ├── COOP2_weedsHighRes.xlsx  #Winter experiment, rep 1; both sites
    ├── COOP3_weedsHighRes.xlsx   #Summer experiment, rep 2; both sites
    ├── COOP4_weedsHighRes.xlsx  #Winter experiment, rep 2; both sites
    ├── FamilyList.csv  #List of family and common names
    ├── NameKey.xlsx   #List of scientific and common names
    └── TraitsCOOPWeeds.xlsx   #Trait data
.gitignore
LICENSE
phylogenetic-competition.Rproj
README.md
```

## Data-specific information

### COOP1_weedsHighRes.xlsx

a.  Number of variables: 30

b.  Number of cases/rows: 40

c.  Variable List:

<!-- -->

1.  `Trial`: Character, describes the fact that the data comes from the Summer 1 experiment
2.  `Site`: Factor, location; Farm Hub = Hudson Valley Farm Hub, Musgrave = Cornell University Musgrave research farm
3.  `Year`: Factor, sampling year
4.  `Block`: Factor, field-block within site:years
5.  `Plot`: Numeric, experimental unit of replication
6.  `CoverCrop`: Factor, treatment name; Tilled = tilled control, Buckwheat = Buckwheat, SH = Sunn hemp, SS = sorghum sudangrass, SSxSH = sorghum sudangrass and Sunn hemp mix
7.  `CoverCropBiomass`: Numeric, total cover crop biomass (g/0.5m<sup>2</sup>), comes from the sum of the three individual cover crop species
8.  `sorghum.sudangrass`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
9.  `sun.hemp`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
10. `buckwheat`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
11. `fall.panicum`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
12. `smooth.crabgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
13. `large.crabgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
14. `oat`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
15. `yellow.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
16. `giant.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
17. `common.lambsquarters`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
18. `shepherd's.purse`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
19. `witchgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
20. `barnyard.grass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
21. `common.velvetgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
22. `redroot.pigweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
23. `wild.mustard`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
24. `velvetleaf`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
25. `common.purslane`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
26. `prostrate.spurge`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
27. `venice.mallow`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
28. `annual.ragweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
29. `dandelion`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
30. `yellow.nutsedge`: Numeric, weed biomass (g/0.5m<sup>2</sup>)

<!-- -->

d.  Missing data codes: blank cell

### COOP2_weedsHighRes.xlsx

a.  Number of variables: 51

b.  Number of cases/rows: 40

c.  Variable List:

<!-- -->

1.  `Trial`: Character, describes the fact that the data comes from the Winter 1 experiment
2.  `Year`: Factor, sampling year
3.  `Site`: Factor, location; Farm Hub = Hudson Valley Farm Hub, Musgrave = Cornell University Musgrave research farm
4.  `Block`: Factor, field-block within site:years
5.  `Plot`: Numeric, experimental unit of replication
6.  `CoverCrop`: Factor, treatment name; Tilled = tilled control, CAN = Canola, CR = Cereal rye, HV = Hairy vetch, HVxCR = Hairy vetch and Cereal rye mix
7.  `CoverCropBiomass`: Numeric, total cover crop biomass (g/0.5m<sup>2</sup>), comes from the sum of the three individual cover crop species
8.  `hairy.vetch`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
9.  `cereal.rye`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
10. `canola`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
11. `common.chickweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
12. `yellow.rocket`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
13. `shepherd's.purse`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
14. `purslane.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
15. `horseweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
16. `annual.bluegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
17. `red.clover`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
18. `brassica.spp.`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
19. `yellow.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
20. `curly.dock`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
21. `canada.thistle`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
22. `annual.ragweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
23. `mouseear.chickweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
24. `lady's.thumb`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
25. `tree.spp`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
26. `thymeleaf.sandwort`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
27. `common.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
28. `roughstalk.bluegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
29. `black.medic`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
30. `dandelion`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
31. `giant.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
32. `barnyard.grass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
33. `yellow.woodsorrel`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
34. `yellow.nutsedge`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
35. `wild.mustard`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
36. `virginia.copperleaf`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
37. `prostrate.knotweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
38. `wild.buckwheat`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
39. `witchgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
40. `green.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
41. `common.lambsquarters`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
42. `broadleaf.plantain`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
43. `persian.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
44. `common.groundsel`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
45. `prickly.lettuce`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
46. `unknown_coop2_mus_1`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
47. `unknown_coop2_mus_2`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
48. `oat`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
49. `corn.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
50. `purple.deadnettle`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
51. `volunteer.cereal.rye`: Numeric, weed biomass (g/0.5m<sup>2</sup>)

<!-- -->

d.  Missing data codes: blank cell

### COOP3_weedsHighRes.xlsx

a.  Number of variables: 46

b.  Number of cases/rows: 40

c.  Variable List:

<!-- -->

1.  `Trial`: Character, describes the fact that the data comes from the Summer 2 experiment
2.  `Year`: Factor, sampling year
3.  `Site`: Factor, location; Farm Hub = Hudson Valley Farm Hub, Musgrave = Cornell University Musgrave research farm
4.  `Block`: Factor, field-block within site:years
5.  `Plot`: Numeric, experimental unit of replication
6.  `CoverCrop`: Factor, treatment name; Tilled = tilled control, Buckwheat = Buckwheat, SH = Sunn hemp, SS = sorghum sudangrass, SSxSH = sorghum sudangrass and Sunn hemp mix
7.  `CoverCropBiomass`: Numeric, total cover crop biomass (g/0.5m<sup>2</sup>), comes from the sum of the three individual cover crop species
8.  `sun.hemp`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
9.  `sorghum.sudangrass`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
10. `buckwheat`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
11. `volunteer.buckwheat`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
12. `smooth.crabgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
13. `fall.panicum`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
14. `hairy.galinsoga`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
15. `barnyard.grass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
16. `wild.mustard`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
17. `common.lambsquarters`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
18. `yellow.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
19. `giant.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
20. `horseweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
21. `witchgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
22. `annual.fleabane`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
23. `redroot.pigweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
24. `lady's.thumb`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
25. `velvetleaf`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
26. `dandelion`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
27. `virginia.copperleaf`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
28. `red.clover`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
29. `common.chickweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
30. `annual.ragweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
31. `broadleaf.plantain`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
32. `annual.ryegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
33. `powell.amaranth`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
34. `cereal.rye`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
35. `yellow.woodsorrel`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
36. `yellow.nutsedge`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
37. `annual.bluegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
38. `green.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
39. `wild.buckwheat`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
40. `timothy`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
41. `mouseear.chickweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
42. `corn.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
43. `winter.pea`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
44. `perrenial.sowthistle`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
45. `large.crabgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
46. `common.purslane`: Numeric, weed biomass (g/0.5m<sup>2</sup>)

<!-- -->

d.  Missing data codes: blank cell

### COOP4_weedsHighRes.xlsx

a.  Number of variables: 50

b.  Number of cases/rows: 40

c.  Variable List:

<!-- -->

1.  `Trial`: Character, describes the fact that the data comes from the Winter 2 experiment
2.  `Year`: Factor, sampling year
3.  `Site`: Factor, location; Farm Hub = Hudson Valley Farm Hub, Musgrave = Cornell University Musgrave research farm
4.  `Block`: Factor, field-block within site:years
5.  `Plot`: Numeric, experimental unit of replication
6.  `CoverCrop`: Factor, treatment name; Tilled = tilled control, CAN = Canola, CR = Cereal rye, HV = Hairy vetch, HVxCR = Hairy vetch and Cereal rye mix
7.  `CoverCropBiomass`: Numeric, total cover crop biomass (g/0.5m<sup>2</sup>), comes from the sum of the three individual cover crop species
8.  `canola`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
9.  `cereal.rye`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
10. `hairy.vetch`: Numeric, cover crop biomass (g/0.5m<sup>2</sup>)
11. `common.chickweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
12. `volunteer.canola`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
13. `wild.mustard`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
14. `red.clover`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
15. `roughstalk.bluegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
16. `large.crabgrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
17. `green.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
18. `purslane.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
19. `horseweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
20. `annual.fleabane`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
21. `shepherd's.purse`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
22. `lady's.thumb`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
23. `white.clover`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
24. `tall.fescue`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
25. `yellow.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
26. `marsh.yellowcress`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
27. `yellow.woodsorrel`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
28. `yellow.rocket`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
29. `giant.foxtail`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
30. `curly.dock`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
31. `common.lambsquarters`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
32. `pennsylvania.smartweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
33. `powell.amaranth`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
34. `thymeleaf.sandwort`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
35. `mouseear.chickweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
36. `night-flowering.catchfly`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
37. `dandelion`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
38. `broadleaf.plantain`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
39. `persian.speedwell`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
40. `kentucky.bluegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
41. `common.groundsel`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
42. `annual.ragweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
43. `wild.buckwheat`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
44. `yellow.nutsedge`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
45. `virginia.copperleaf`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
46. `venice.mallow`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
47. `canada.thistle`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
48. `annual.bluegrass`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
49. `poaceae.spp.`: Numeric, weed biomass (g/0.5m<sup>2</sup>)
50. `prostrate.knotweed`: Numeric, weed biomass (g/0.5m<sup>2</sup>)

<!-- -->

d.  Missing data codes: blank cell

### FamilyNames.csv

a.  Number of variables: 2

b.  Number of cases/rows: 480

c.  Variable List:

<!-- -->

1.  `Family`: Character, family of species
2.  `group`: Character, ecological group of family <!-- -->

<!-- -->

d.  Missing data codes: blank cell

### NameKey.xlsx

a.  Number of variables: 6

b.  Number of cases/rows: 72

c.  Variable List:

<!-- -->

1.  `CommonName`: Character, species name used in the biomass data frames
2.  `species`: Character, scientific name of species
3.  `genus`: Character, genus of species
4.  `family`: Character, family of species
5.  `species.relative`: Character, scientific name of phylogenetic sister taxa. Used when a species was not identified in the ‘V.PhyloMaker2’ database.
6.  `genus.relative`: Character, genus of phylogenetic sister taxa. Used when a species was not identified in the ‘V.PhyloMaker2’ database.

<!-- -->

d.  Missing data codes: blank cell

## Notes

-   The weed names in the file 'COOPWeeds_NameKey.csv' are "valid" according to the world plants database, which was used on Sept. 23, 2022 to create the data frame.

-   Due to peer reviewer feedback, "Rhamphospermum arvense" is referred to as "Sinapis arvensis" in the writing and figures of the article "Phylogenetic relatedness can influence cover crop-based weed suppression"

-   This repository is also saved in Zenodo for long-term storage: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17835637.svg)](https://doi.org/10.5281/zenodo.17835637)
