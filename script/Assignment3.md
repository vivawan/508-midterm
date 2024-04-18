---
title: "Geospatial Risk Modeling - Predictive Policing"
author: "Neve/Viva/Yaohan"
date: "2024-04-06"
output: 
  html_document:
    keep_md: yes
    toc: yes
    theme: flatly
    toc_float: yes
    code_folding: hide
    number_sections: no
  pdf_document:
    toc: yes
---

```{=html}
<style>
.kable thead tr th, .table thead tr th {
  text-align: left !important;}
table.kable, table.table {
  width: 100% !important;}
  body {
  line-height: 1.6;
  font-size: 16px
}
</style>
```


# Introduction

Neve, Viva and Yaohan work together on the scripts, and then finish the write-up separately.

**Analysis goal:**

**Study area:**

**Data description:**

**Discussion:**

-   Import all datasets


```r
# Read and process Chicago boundary data
chicagoBoundary <- 
  st_read(file.path(root.dir, "/Chapter5/chicagoBoundary.geojson")) %>%  # Read Chicago boundary data
  st_transform('ESRI:102271')  # Transform coordinate reference system

# Chicago Neighborhoods
neighborhoods <- 
  st_read("https://raw.githubusercontent.com/blackmad/neighborhoods/master/chicago.geojson") %>% 
  st_transform('ESRI:102271')

# Read and process police districts data
policeDistricts <- 
  st_read("https://data.cityofchicago.org/api/geospatial/fthy-xz3r?method=export&format=GeoJSON") %>%
  st_transform('ESRI:102271') %>%  # Transform coordinate reference system
  dplyr::select(District = dist_num)  # Select only the district number, renaming it to 'District'

# Read and process damagelaries data
crimes2018 <- 
  read.socrata("https://data.cityofchicago.org/resource/3i3m-jwuy.json")

## rank the crimes by the count of each type
crimes2018.asdf <- crimes2018 %>% 
  group_by(primary_type, description) %>% 
  tally() %>% 
  arrange(desc(n))

## choose CRIMINAL DAMAGE TO PROPERTY as dependent variable within the boundary
damage2018 <- crimes2018 %>% 
  filter(primary_type == "CRIMINAL DAMAGE" & description == "TO PROPERTY") %>%
  na.omit() %>% 
  st_as_sf(coords = c("location.longitude", "location.latitude"), crs = 4326, agr = "constant") %>%  
  st_transform('ESRI:102271') %>% 
  st_intersection(chicagoBoundary) %>% 
  distinct()
```

-   Plot criminal damage locations and fishnet in Chicago


```r
# mapping crime data in dots and in the fishnet
fishnet <- 
st_make_grid(chicagoBoundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[chicagoBoundary] %>%            # fast way to select intersecting polygons
  st_sf() %>%   mutate(uniqueID = 1:n())

crime_net <- 
  dplyr::select(damage2018) %>% 
  mutate(count.damage = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(count.damage = replace_na(count.damage, 0),
         uniqueID = 1:n(),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() + 
  geom_sf(data = chicagoBoundary) +  # Add Chicago boundary
  geom_sf(data = damage2018, colour = "red", size = 0.005, show.legend = "point") +
  labs(title = "Criminal Damage, 2018 Chicago") + 
  theme_void() +
  theme(plot.title = element_text(size = 11, face = "bold"))+
ggplot() +
  geom_sf(data = crime_net, aes(fill = count.damage), color = NA) +
  scale_fill_viridis("Count of Criminal Damage") +
  labs(title = "Fishnet of Criminal Damage by Count") +
  theme_void()+
  theme(plot.title = element_text(size = 11, face = "bold")) +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
```

![](Assignment3_files/figure-html/crime map-1.png)<!-- -->

# Risk Factors

-   Variable selection
    -   **One: Abandoned Cars** (count + k=3 nearest neighborhood)\
        description
    -   **Two: Abandoned Buildings** (count + k=3 nearest neighborhood)\
        description
    -   **Three: Graffiti** (count + k=3 nearest neighborhood)\
        description
    -   **Four: Disfunctional Street Lights** (count + k=3 nearest neighborhood)\
        description
    -   **Five: Sanitation Complaints** (count + k=3 nearest neighborhood)\
        description
    -   **Six: Liquor Retail** (count + k=3 nearest neighborhood)\
        description
    -   **Seven: Park Location** (count + k=3 nearest neighborhood)\
        description
    -   **Eight: Environmental Complaints** (count + k=3 nearest neighborhood)


```r
# load variable
abandonCars <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Abandoned-Vehicles/3c9v-pnva") %>%
    # Extract the year from the creation date and filter for the year 2017
    mutate(year = substr(creation_date, 1, 4)) %>% filter(year == "2018") %>%
    # Select latitude and longitude columns and remove rows with missing values
    dplyr::select(Y = latitude, X = longitude) %>%
    na.omit() %>%
    # Convert to simple feature (sf) object with geographic coordinates
    st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    # Transform coordinates to match the coordinate reference system (CRS) of the fishnet
    st_transform(st_crs(fishnet)) %>%
    # Add a legend label indicating abandoned cars
    mutate(Legend = "Abandoned_Cars")

abandonBuildings <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Vacant-and-Abandoned-Building/7nii-7srd") %>%
  mutate(year = substr(date_service_request_was_received,1,4)) %>%
  filter(year == "2018") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Buildings")

graffiti <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Graffiti-Removal-Historical/hec5-y4x5") %>%
    mutate(year = substr(creation_date,1,4)) %>% filter(year == "2018") %>%
    filter(where_is_the_graffiti_located_ %in% c("Front", "Rear", "Side")) %>%
    dplyr::select(Y = latitude, X = longitude) %>%
    na.omit() %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    st_transform(st_crs(fishnet)) %>%
    mutate(Legend = "Graffiti")

streetLightsOut <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Street-Lights-All-Out/zuxi-7xem") %>%
    mutate(year = substr(creation_date,1,4)) %>% filter(year == "2018") %>%
    dplyr::select(Y = latitude, X = longitude) %>%
    na.omit() %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    st_transform(st_crs(fishnet)) %>%
    mutate(Legend = "Street_Lights_Out")

sanitation <-
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Sanitation-Code-Complaints-Hi/me59-5fac") %>%
    mutate(year = substr(creation_date,1,4)) %>% filter(year == "2018") %>%
    dplyr::select(Y = latitude, X = longitude) %>%
    na.omit() %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    st_transform(st_crs(fishnet)) %>%
    mutate(Legend = "Sanitation")

liquorRetail <- 
  read.socrata("https://data.cityofchicago.org/resource/nrmj-3kcf.json") %>%  
    filter(business_activity == "Retail Sales of Packaged Liquor") %>%
    dplyr::select(Y = latitude, X = longitude) %>%
    na.omit() %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    st_transform(st_crs(fishnet)) %>%
    mutate(Legend = "Liquor_Retail")

park <- 
  read.socrata("https://data.cityofchicago.org/resource/eix4-gf83.json") %>%
  select(X = x_coord, Y = y_coord) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    st_transform(st_crs(fishnet)) %>%
    mutate(Legend = "Park")

environ.complaint <- 
  read.socrata("https://data.cityofchicago.org/resource/fypr-ksnz.json") 

environ.complaint <- environ.complaint %>% 
  mutate(year = substr(complaint_date,1,4)) %>% filter(year == "2018") %>%
    dplyr::select(Y = latitude, X = longitude) %>%
    na.omit() %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
    st_transform(st_crs(fishnet)) %>%
    mutate(Legend = "Environmental_Complaint")
```

-   Make the maps of risk factors count


```r
# add variables to the fishnet 
vars_net <- 
  rbind(abandonCars,streetLightsOut,abandonBuildings,
        liquorRetail, graffiti, sanitation, park, environ.complaint) %>% #add on
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

vars_net.long <- vars_net %>%
  gather(Variable, value, -geometry, -uniqueID) %>%
  na.omit()

# mapping risk factors
vars_risk <- unique(vars_net.long$Variable)

mapList <- list()

for(i in vars_risk)
  {mapList[[i]] <- 
    ggplot() +
      geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
      scale_fill_viridis(name="") +
      labs(title=i) +
      theme_void()+
      theme(plot.title = element_text(size = 10)) +
  theme(legend.text = element_text(size = 8))}

do.call(grid.arrange,c(mapList, ncol=2, top="Risk Factors by Fishnet"))
```

![](Assignment3_files/figure-html/map risk factors-1.png)<!-- -->

-   Make the maps of distance to the nearest risk factors


```r
vars_net <-
  vars_net %>%
    mutate(
      Abandoned_Buildings.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(abandonBuildings),3),
      Abandoned_Cars.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(abandonCars),3),
      Graffiti.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(graffiti),3),
      Liquor_Retail.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(liquorRetail),3),
      Street_Lights_Out.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(streetLightsOut),3),
      Park.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(park),3),
      Sanitation.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(sanitation),3),
      Environmental_Complaint.nn =
        nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(environ.complaint),3))

nn.vars_net.long <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

nn.vars <- unique(nn.vars_net.long$Variable)
nn.mapList <- list()

for(i in nn.vars){
  nn.mapList[[i]] <- 
    ggplot() +
      geom_sf(data = filter(nn.vars_net.long, Variable == i), aes(fill=value), colour=NA) +
      scale_fill_viridis(name="") +
      labs(title=i) +
      theme_void()+
      theme(plot.title = element_text(size = 10)) +
  theme(legend.text = element_text(size = 8))}

do.call(grid.arrange,c(nn.mapList, ncol=2, top="Nearest Neighbor Risk Factors by Fishnet"))
```

![](Assignment3_files/figure-html/map nearest distance-1.png)<!-- -->

# Moran's I Related Maps

-   Local Moran's I plot


```r
# join variables data to fishnet
final_net <-
  left_join(crime_net, st_drop_geometry(vars_net), by="uniqueID") 

# join neighborhoods and police districts to fishnet
final_net <-
  st_centroid(final_net) %>%
    st_join(dplyr::select(neighborhoods, name)) %>%
    st_join(dplyr::select(policeDistricts, District)) %>%
      st_drop_geometry() %>%
      left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
      st_sf() %>%
  na.omit()

# calculate Moran's I
final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

damage.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$count.damage, final_net.weights, zero.policy = TRUE)),
    as.data.frame(final_net)) %>% 
    st_sf() 

# plot Moran's I
mp <- moran.plot(as.vector(scale(damage.localMorans$count.damage)), final_net.weights, zero.policy = TRUE)
```

![](Assignment3_files/figure-html/moran1-1.png)<!-- -->

```r
# add one variable to indicate hotspot
damage.localMorans$hotspot <- 0

damage.localMorans[(mp$x >= 0 & mp$wx >= 0) & (damage.localMorans$`Pr(z != E(Ii))` <= 0.01), "hotspot"] <- 1
```

-   Local Moran's I Statistics


```r
damage.localMorans.1 <- damage.localMorans %>% 
  dplyr::select(Damage_Count = count.damage, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`,
                hotspot) %>%
      gather(Variable, Value, -geometry)

damage.vars <- unique(damage.localMorans.1$Variable)
damage.varList <- list()

for(i in damage.vars)
  {damage.varList[[i]] <- 
    ggplot() +
      geom_sf(data = filter(damage.localMorans.1, Variable == i), 
              aes(fill = Value), colour=NA) +
      scale_fill_viridis(name="") +
      labs(title=i) +
      theme_void() + 
      theme(legend.position = "bottom") +
      theme(plot.title = element_text(size = 10)) +
      theme(legend.text = element_text(size = 8))}

do.call(grid.arrange,c(damage.varList, ncol = 4, top = "Local Moran's I Statistics of Criminal Damage"))
```

![](Assignment3_files/figure-html/moran.2-1.png)<!-- -->

# Correlations Test


```r
# add spatial factor
final_net <- final_net %>% 
  mutate(damage.isSig = 
           ifelse(damage.localMorans$hotspot == 1, 1, 0)) %>%
  mutate(damage.isSig.dist = 
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(filter(final_net, 
                                           damage.isSig == 1))), 
                       k = 1))

# calculate correlations
correlation.long <-
  st_drop_geometry(final_net) %>%
    dplyr::select(-uniqueID, -cvID, -name, -District) %>%
    gather(Variable, Value, -count.damage)

correlation.cor <-
  correlation.long %>%
    group_by(Variable) %>%
    summarize(correlation = cor(Value, count.damage, use = "complete.obs"))


ggplot(correlation.long, aes(Value, count.damage)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x = -Inf, y = Inf, vjust = 1.5, hjust = -0.1, size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.5) +
  facet_wrap(~Variable, ncol = 4, scales="free") +
  labs(title = "Criminal Damage Count as a Function of Risk Factors") +
  theme_bw() +
  theme(strip.text = element_text(size = 8))
```

![](Assignment3_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

# A Histogram of Dependent Variable

This histogram suggests an OLS regression is not appropriate method of analysis.


```r
ggplot(final_net, aes(x = count.damage)) +
  geom_histogram(binwidth = 3, fill = "grey", color = "black") +
  labs(title = "Distribution of Criminal Damage Counts, Chicago", x = "Damage Count", y = "Frequency") +
  theme_bw()
```

![](Assignment3_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# Cross-validated Poisson Regression


```r
## define the variables we want
reg.vars <- c(nn.vars)

reg.ss.vars <- c(nn.vars, "damage.isSig", "damage.isSig.dist")

## RUN REGRESSIONS
reg.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "count.damage",
  indVariables = reg.vars) %>%
    dplyr::select(cvID = cvID, count.damage, Prediction, geometry)

reg.ss.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "count.damage",
  indVariables = reg.ss.vars) %>%
    dplyr::select(cvID = cvID, count.damage, Prediction, geometry)
  
reg.spatialCV <- crossValidate(
  dataset = final_net,
  id = "name",
  dependentVariable = "count.damage",
  indVariables = reg.vars) %>%
    dplyr::select(cvID = name, count.damage, Prediction, geometry)

reg.ss.spatialCV <- crossValidate(
  dataset = final_net,
  id = "name",
  dependentVariable = "count.damage",
  indVariables = reg.ss.vars) %>%
    dplyr::select(cvID = name, count.damage, Prediction, geometry)

# calculate error
reg.summary <- 
  rbind(
    mutate(reg.cv, Error = Prediction - count.damage,
                   Regression = "Random k-fold CV: Just Risk Factors"),
                             
    mutate(reg.ss.cv, Error = Prediction - count.damage,
                      Regression = "Random k-fold CV: Spatial Process"),
    
    mutate(reg.spatialCV, Error = Prediction - count.damage,
                          Regression = "Spatial LOGO-CV: Just Risk Factors"),
                             
    mutate(reg.ss.spatialCV, Error = Prediction - count.damage,
                             Regression = "Spatial LOGO-CV: Spatial Process")) %>%
    st_sf() 
```

# Model Error

## Distribution of MAE


```r
error_by_reg_and_fold <- 
  reg.summary %>%
    group_by(Regression, cvID) %>% 
    summarize(Mean_Error = mean(Prediction - count.damage, na.rm = T),
              MAE = abs(Mean_Error), na.rm = T) %>%
  ungroup()

#remove afterwards
error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
    geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
    facet_wrap(~Regression) +  
    geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
    labs(title="Distribution of MAE", subtitle = "k-fold Cross Validation vs. LOGO-CV",
         x="Mean Absolute Error", y="Count") +
    theme_bw()
```

![](Assignment3_files/figure-html/MAE distribution-1.png)<!-- -->

## Error Map


```r
error_by_reg_and_fold %>%
  filter(str_detect(Regression, "k-fold")) %>%
  ggplot() +
    geom_sf(aes(fill = MAE)) +
    facet_wrap(~Regression) +
    scale_fill_viridis() +
    labs(title = "Damage Errors by k-fold Regression") +
    mapTheme() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6),
                       strip.text.x = element_text(size = 10))
```

![](Assignment3_files/figure-html/error map-1.png)<!-- -->

```r
error_by_reg_and_fold %>%
  filter(str_detect(Regression, "LOGO")) %>%
  ggplot() +
    geom_sf(aes(fill = MAE)) +
    facet_wrap(~Regression) +
    scale_fill_viridis() +
    labs(title = "Damage Errors by LOGO-CV Regression") +
  mapTheme() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6),
                       strip.text.x = element_text(size = 10))
```

![](Assignment3_files/figure-html/error map-2.png)<!-- -->

## Table of MAE and SD_MAE by Regression


```r
st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
    summarize(Mean_MAE = round(mean(MAE), 2),
              SD_MAE = round(sd(MAE), 2)) %>%
  kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = T) %>%
  column_spec(1:3, extra_css = "text-align: left;")
```

<table class="table table-striped table-hover" style="color: black; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Regression </th>
   <th style="text-align:right;"> Mean_MAE </th>
   <th style="text-align:right;"> SD_MAE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;text-align: left;"> Random k-fold CV: Just Risk Factors </td>
   <td style="text-align:right;text-align: left;"> 0.93 </td>
   <td style="text-align:right;text-align: left;"> 0.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;text-align: left;"> Random k-fold CV: Spatial Process </td>
   <td style="text-align:right;text-align: left;"> 0.79 </td>
   <td style="text-align:right;text-align: left;"> 0.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;text-align: left;"> Spatial LOGO-CV: Just Risk Factors </td>
   <td style="text-align:right;text-align: left;"> 2.07 </td>
   <td style="text-align:right;text-align: left;"> 1.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;text-align: left;"> Spatial LOGO-CV: Spatial Process </td>
   <td style="text-align:right;text-align: left;"> 1.57 </td>
   <td style="text-align:right;text-align: left;"> 1.57 </td>
  </tr>
</tbody>
</table>

## Error by Race

-   Download census data


```r
tracts18 <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E"), 
          year = 2018, state=17, county=031, geometry=T) %>%
  st_transform('ESRI:102271')  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberWhites = B01001A_001) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White")) %>%
  .[neighborhoods,]
```

-   Comparison between different regressions


```r
reg.summary %>% 
    st_centroid() %>%
    st_join(tracts18) %>%
    na.omit() %>%
      st_drop_geometry() %>%
      group_by(Regression, raceContext) %>%
      summarize(mean.Error = mean(Error, na.rm = T)) %>%
      spread(raceContext, mean.Error) %>%
  kable(caption = "<span style='font-weight: bold; color: black;'>Mean Error by Neighborhood Racial Context, 2018</span>") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = T) %>%
  column_spec(1:3, extra_css = "text-align: left;")
```

<table class="table table-striped table-hover" style="color: black; margin-left: auto; margin-right: auto;">
<caption><span style="font-weight: bold; color: black;">Mean Error by Neighborhood Racial Context, 2018</span></caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Regression </th>
   <th style="text-align:right;"> Majority_Non_White </th>
   <th style="text-align:right;"> Majority_White </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;text-align: left;"> Random k-fold CV: Just Risk Factors </td>
   <td style="text-align:right;text-align: left;"> -0.8914314 </td>
   <td style="text-align:right;text-align: left;"> 0.9723129 </td>
  </tr>
  <tr>
   <td style="text-align:left;text-align: left;"> Random k-fold CV: Spatial Process </td>
   <td style="text-align:right;text-align: left;"> -0.4070593 </td>
   <td style="text-align:right;text-align: left;"> 0.4392447 </td>
  </tr>
  <tr>
   <td style="text-align:left;text-align: left;"> Spatial LOGO-CV: Just Risk Factors </td>
   <td style="text-align:right;text-align: left;"> -0.9662880 </td>
   <td style="text-align:right;text-align: left;"> 1.0011590 </td>
  </tr>
  <tr>
   <td style="text-align:left;text-align: left;"> Spatial LOGO-CV: Spatial Process </td>
   <td style="text-align:right;text-align: left;"> -0.4409691 </td>
   <td style="text-align:right;text-align: left;"> 0.4423649 </td>
  </tr>
</tbody>
</table>

# Comparing Model to Traditional Methods


```r
damage_ppp <- as.ppp(st_coordinates(damage2018), W = st_bbox(final_net))
damage_KD.1000 <- density.ppp(damage_ppp, 1000)
damage_KD.1500 <- density.ppp(damage_ppp, 1500)
damage_KD.2000 <- density.ppp(damage_ppp, 2000)

damage_KD.df <- rbind(
  mutate(data.frame(rasterToPoints(mask(raster(damage_KD.1000), as(neighborhoods, 'Spatial')))), Legend = "1000 m"),
  mutate(data.frame(rasterToPoints(mask(raster(damage_KD.1500), as(neighborhoods, 'Spatial')))), Legend = "1500 m"),
  mutate(data.frame(rasterToPoints(mask(raster(damage_KD.2000), as(neighborhoods, 'Spatial')))), Legend = "2000 m")) 

damage_KD.df$Legend <- factor(damage_KD.df$Legend, levels = c("1000 m", "1500 m", "2000 m"))

ggplot(data=damage_KD.df, aes(x=x, y=y)) +
  geom_raster(aes(fill=layer)) + 
  facet_wrap(~Legend) +
  coord_sf(crs=st_crs(final_net)) + 
  scale_fill_viridis(name="Density") +
  labs(title = "Kernel Density with 3 Different Search Radii") +
  mapTheme() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6),
                       strip.text.x = element_text(size = 10))
```

![](Assignment3_files/figure-html/kernel-1.png)<!-- -->

# Prediction for 2019

- Description


```r
# load 2019 data
damage19 <- 
  read.socrata("https://data.cityofchicago.org/resource/w98m-zvie.json")

# select crime data
damage19 <- damage19 %>% 
filter(primary_type == "CRIMINAL DAMAGE" & description == "TO PROPERTY") %>% 
  na.omit() %>%  # Remove rows with missing values
  st_as_sf(coords = c("location.longitude", "location.latitude"), crs = 4326, agr = "constant") %>%  # Convert to sf object with specified CRS
  st_transform('ESRI:102271') %>%  # Transform coordinate reference system
  distinct() %>%  # Keep only distinct geometries
  .[fishnet,]

# add risk category
damage_KDE_sf <- as.data.frame(damage_KD.1000) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  mutate(label = "Kernel Density",
         Risk_Category = ntile(value, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(damage19) %>% mutate(damageCount = 1), ., sum) %>%
    mutate(damageCount = replace_na(damageCount, 0))) %>%
  dplyr::select(label, Risk_Category, damageCount)

damage_risk_sf <-
  filter(reg.summary, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  mutate(label = "Risk Predictions",
         Risk_Category = ntile(Prediction, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(damage19) %>% mutate(damageCount = 1), ., sum) %>%
      mutate(damageCount = replace_na(damageCount, 0))) %>%
  dplyr::select(label,Risk_Category, damageCount)

rbind(damage_KDE_sf, damage_risk_sf) %>%
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
  ggplot() +
    geom_sf(aes(fill = Risk_Category), colour = NA) +
    geom_sf(data = sample_n(damage19, 3000), size = .1, colour = "black") +
    facet_wrap(~label, ) +
    scale_fill_viridis(discrete = TRUE) +
    labs(title="Comparison of Kernel Density and Risk Predictions",
         subtitle="2018 criminal risk predictions; 2019 criminal damage") +
    mapTheme() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6),
                       strip.text.x = element_text(size = 10))
```

![](Assignment3_files/figure-html/predict 2019-1.png)<!-- -->


```r
rbind(damage_KDE_sf, damage_risk_sf) %>%
  st_set_geometry(NULL) %>% na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category) %>%
  group_by(label, Risk_Category) %>%
  summarize(count.theft = sum(Value)) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(Rate_of_test_set_crimes = count.theft / sum(count.theft)) %>%
    ggplot(aes(Risk_Category,Rate_of_test_set_crimes)) +
      geom_bar(aes(fill=label), position="dodge", stat="identity") +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = "Risk Prediction vs. Kernel Density, 2019 Criminal Damage") +
      theme_bw()
```

![](Assignment3_files/figure-html/bar plot-1.png)<!-- -->
