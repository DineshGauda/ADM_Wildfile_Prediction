# Advance Data Mining Project on Wildfire Prediction

This project is a part of the continuous assessment for Advance Data Mining module of Masters in Data Analytics course at National College Of Ireland.

**PROJECT DETAILS**

**Module Name:** Advance Data Mining module

**College:** National College of Ireland

**Course:** Masters in Data Analytics

**Duration:** January, 2019 - April, 2019

**Project Description:** 

In this project, an interesting demonstration of machine learning in the field of wildfire prediction was performed. This was achieved using historical remote sensing and weather data. Machine learning models (SVM, RF, ANN, AutoML) using these data resulted in positively predicting accurate wildfires.

**Tools & Languages Used:**
1. R
2. Excel
3. Tableau


Files description:-
files inside code folder

1) wildfire-data-etl: wildfires extraction code from raw data.
2) weatherParser: weather related data extraction code.
3) NDVI_Data_Extraction: NDVI data extraction code.
4) IntegratedParser: code to integrate all sources of data based on lat & long
5) randomForest and SVM: RF & SVM model implementation code
6) wildfire-predictions-models - EDA & ANN, AutoML codes.

files inside sampledata folder 

1) processed: Contains cleaned data after integration of all sources of data.
2) NDVI: Contains raw .nc ndvi file.
3) NFDB_point_20181129_large_fires: contains raw sample of fires data.
4) sample_wildfire_data: contains processed sample from 'NFDB_point_20181129_large_fires' files.
5) weather_sample: contains sample data for temperature, wind, relative humidity, solar etc.
