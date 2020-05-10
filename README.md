# Global_Development_app
R shiny app to compare health and economic factors 

The purpose of this app is to compare various health and economic factors between countries in order to 
enhance understanding of various development indices in an international context. Interactive visualizations include a 
single factor chloropleth world map with hover, click and zoom capabilities; a two factor categorical interactive 
scatterplot; single country ranking text outputs and a multi-country single factor lineplot. The primary data sources 
used in this application are taken from World Bank and the World Health Organization. Selected world development indices of 
interest include M/F literacy rates, M/F infant mortality rates, M/F life expectancy, GDP per capita, service as % of GDP, and 
trade as % of GDP. 

This app is an extension of a prior application created in collaboration with developers Bryce Huffman and Nathan Barrington 
(available at https://github.com/brycehuffman/sys_2202_finalproject). 
This iteration seeks to provide the following improvements:

a.) increase app run speed and data processing efficency
    - Remove plotly world map and replace with ggplot with manually added zoom, click and hover capabilities. 
      - decrease map drawing runtime speed from ~10 seconds to less than 3 seconds  
      - decreases bootup speed from ~20 seconds to less than 10 seconds 
      
b.) provide more information about individual countries development 
    - Color code scatterplot to categorize continents
    - Add hover over capabilities to scatterplot 
    - Add line plot depicting the differences in a single selected factor over time 
    - Add country rankings for every factor for a given year 
    
c.) provide a cleaner user interface
    - make all graph/charts visible on single page without need to scroll
    - clean up design and color coordinate panels

Source of snippets of code used in this app include: 
(line 465::483) coords to country: 
  https://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r
(line 910::928) ggvis axis: 
  https://ggvis.rstudio.com/axes-legends.html
(line 785::816) customized tooltip for worldmap: 
  https://gitlab.com/snippets/16220
