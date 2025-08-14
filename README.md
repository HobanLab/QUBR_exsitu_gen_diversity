# QUBR_exsitu_gen_diversity
 
### Overview
This repository contains code related to the 2024-2025 RaMP project, which assessed outplanted seedling success in *Quercus brandegeei*.

### Contents
#### Set Up
Imports data from Daniel’s database (2023 and 2024 tabs) on 1927 adopted seedlings and data from November 2024 field work on 157 outplanted seedlings.

#### Data Cleaning
##### [Daniel’s database](https://docs.google.com/spreadsheets/d/16GrmfPXmfTAl9oViUIs21HwsK6D_JKxI/edit?gid=1732693285#gid=1732693285)
- The script renames columns to simplified versions in English.
- Calculates a DateDied (conservative, median, and liberal versions) based on different assumptions of how quickly a seedling died after its last positive ID
- Calculates a TimeAlive (conservative, median, and liberal) for each seedling 
- Calculates a PotentialTimeAlive of how long each seedling has been planted for
- Identifies an Outcome of whether each seedling is Dead or Alive
- Identifies priority sites with potentially living seedlings to prioritize during November 2024 field work

##### [Nov 2024 field data](https://docs.google.com/spreadsheets/d/18FMAklTUCJ3s7tl2p5vEQV8LNIfpGTxcbDykR5Rl164/edit?gid=0#gid=0)
- Renames columns to match other databases
- Standardizes character responses and assigns numeric values to continuous variables (height, health, canopy cover)
- Replaces height measurements with a range of heights
- Adds information back into Daniel’s dataset on the individuals we monitored in Nov 2024
- Recalculates TimeAlive (conservative, median, and liberal) and DateDied (conservative, median, and liberal) including information from Monitor4 (Nov 2024)

#### Survivorship For Loop
- Creates a df that increases by 1 day at a time and identifies how many seedlings were alive that many days after outplanting
- Plots a survivorship curve including all of the seedlings that have been outplanted to date
- We ended up not using the survivorship curve in analysis and replaced it with a waterfall figure

#### Functionalized For Loop

#### Visualizing Outliers

#### Chi-Squared Test

#### Interval Regression

#### Ordinal Regression
- Analyzed Condition and Canopy variables (with and without dead individuals included)
- Found that the best explanatory model includes Canopy and Ranch

#### Poster Figures
- Figures that were used in presentations and on conference poster
  - Survivorship curve
  - Waterfall survivorship figure (conservative, median, liberal interpretations of DateDied): preferred alternative to Survivorship curve
  - Comparing the Frequency of each Condition of seedlings faceted by Ranch
  - Comparing the Frequency of each Condition of seedlings faceted by Canopy
  - Stacked bar charts
    - number of individuals in each Canopy class, colored by Region
    - number of individuals in each Canopy class, colored by Ranch & Region
    - number of individuals in each Condition class, colored by Region
    - number of individuals in each Condition class, colored by Ranch & Region

#### Maps
- Distribution of individuals alive as of November 2024
- Distribution of all individuals historically outplanted





