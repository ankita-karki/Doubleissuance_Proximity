# Proximity Analysis

## Overview
This repository explores how proximity impacts project overlaps by applying buffer distances (5 km, 10 km, and 15 km) around efficient cookstove projects. The goal is to assess the risk of double issuance that undermines carbon market integrity.

## Key Features
* **Buffer Analysis**: Understand spatial relationships between projects.
- **Statistical Analysis**: Perform t-tests to measure the impact of buffer distances on overlaps.
- **Proximity Visualization**: Show how overlap increases with larger buffer zones.

## Project Maps
### Buffer analysis in Africa region
<div align="center">
  <img src="https://github.com/ankita-karki/Doubleissuance_Proximity/blob/main/output_maps/Africa%20.png?raw=true">
</div>

### Buffer analysis in Mozambique
<div align="center">
  <img src="https://github.com/ankita-karki/Doubleissuance_Proximity/blob/main/output_maps/Mozambique.png?raw=true" alt="Zambia Map">
</div>

## Tools and Technologies
* **R**: For geospatial data processing and analysis and  for advanced spatial visualizations
* **QGIS**: To refine and inspect geospatial data
* **Coordinate System**: WGS84 (EPSG:4326).

## Data Preparation
* Download project boundary data from Verra, Gold Standard, or CDM registries
* Preprocess geospatial data using QGIS to generate KML files if needed

## Outputs
* Spatial visualizations saved in the /outputs folder
* CSV files with overlap percentages and statistics

## Citation
* If you use this repository, kindly cite:
* Master's Thesis: "Assessing Double Issuance between Avoided Deforestation and Efficient Cookstove Projects: A Geospatial Analysis Approach."
* Author: Ankita Karki
* Institution: Rheinland-Pfälzische Technische Universität Kaiserslautern-Landau
* Supervisors: Jonathan Jupke, Isabel Hasse
