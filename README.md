# Exploring-Agricultural-Production-Trends-in-Sri-Lanka
Project Overview
This project explores agricultural production trends in Sri Lanka using data mining and machine learning techniques. By leveraging the FAOSTAT dataset, it identifies hidden patterns, relationships, and predictive insights in crop and livestock production.
The project combines Association Rule Mining to discover co-occurrence patterns and Logistic Regression to predict production outcomes, supported by an interactive dashboard for visualization.

Objectives:
Analyze agricultural production trends in Sri Lanka
Discover relationships between crops and livestock products
Predict high vs low production using machine learning
Build an interactive dashboard for data exploration

Dataset:
Source: FAOSTAT (Food and Agriculture Organization)
Data includes:
Crop & livestock products
Production, yield, and area
Year-wise records
Country-level (Sri Lanka) data

Technologies Used:
R Programming
Libraries: arules, arulesViz, tidyverse, caret, plotly, shiny
Data Visualization: Plotly, ggplot2
Dashboard: Shiny Web Application

Key Features:
1. Association Rule Mining
Uses Apriori algorithm
Identifies frequent itemsets and strong relationships
Metrics used: Support, Confidence, Lift
Visualizations:
Graph plots
Grouped matrix plots
Scatter & parallel coordinate plots

2. Logistic Regression
Predicts High vs Low production
Uses features like Area, Item, and Year
Model evaluation with:
Confusion Matrix
Accuracy score
Probability distribution

3. Interactive Dashboard
Upload FAOSTAT dataset
Adjust parameters (support, confidence, lift)
Explore rules and insights dynamically
Includes multiple visual tabs for better analysis

Results & Insights
Identified strong relationships between agricultural products
Revealed co-production patterns across regions and years
Logistic regression provided predictive insights into production levels
Dashboard enabled easy interpretation of complex data
