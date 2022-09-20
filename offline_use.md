
## How to use the Shiny application offline.

This Shiny app relies on a few internet resources, based on the approach users choose 
to for the Coefficient of Variation model:

### For all approaches  

At launch the app download a Google Font: Lora, as it is used for maps style.
The app first looks for a folder 'fonts' then within this folder for a file 'Lora-Italic.ttf' within this folder. If it doesn't exist the font is downloaded in a temporary directory.

**To use the app offline:**

1. **Create a folder 'fonts' in your working directory.**
1. **Download manually the font family from Google Fonts:** https://fonts.google.com/download?family=Lora.
1. **Unzip the downloaded file in the 'fonts' folder.**

Example structure:

```
WD (working directory)  
+-- fonts (user created)  
|   +-- Lora  
|       +-- static  
|           +-- Lora-Italic.ttf  
|           +-- ...
+-- ...
```



### For approach 1: biomass maps

When approach 1 is selected the app asks you to choose a directory to look for the biomass maps and if they are not found, download them in that directory. They are 2 maps to download: 
1. Avitabile et al. 2016, the map contains the whole tropical region,
1. Santoro et al. 2018, the map is divided in tiles.

When downloading from the app, the app finds automatically which Santoro tiles to download based on the Area of Interest (AOI) shapefile uploaded by the users. 

**To use the app offline:**

1. **Create a folder in your working directory (ex. 'data').**
1. **Download Avitabile 2016 map and unzip in your newly create folder:** http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip.
1. **Create a subfolder for Santoro tiles with the name: 'Santoro_agb' (important to use this folder name!).**
1. **Download manually the tiles needed and unzip within 'Santoro_agb':**https://globbiomass.org/wp-content/uploads/GB_Maps/Globbiomass_global_dataset.html.

Example folder structure:

```
WD (working directory)  
+-- data (user created)  
|   +-- Avitabile_AGB_Map  
|       +-- Avitabile_AGB_Map.tif
|   +-- Santoro_agb
|       +-- N00E100_agb.tif
|       +-- N40E100_agb.tif
|       +-- ...
+-- ...
```


