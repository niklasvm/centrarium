---
layout: post
title: 'Choosing a school in Cape Town, Southern Suburbs'
author: 'Niklas von Maltzahn'
output: 
  html_document
tags: [r, ggplot2, ggmap]
category: r
comments: yes
---



The Cape Town southern suburbs contain some of the Western Cape's top schools but getting your child in to your desired school is very much dependent on being in the *correct zone* or catchment area. The catchment areas are usually badly defined with most schools just indicating that it must be the closest school to the child's home.

My objective was to determine which areas would be best suited to getting in to a particular school.

We start by loading the **ggmap** package in order to geocode a set of candidate schools.


{% highlight r %}
# load some packages
library(ggmap)
library(dplyr)

# define candidate schools
school_names <- c('Rondebosch Boys Preparatory School',
                  'Groote Schuur Primary School, Rondebosch',
                  'Golden Grove Primary School, Rondebosch',
                  'Claremont Primary School, Claremont, Cape Town',
                  'Rosebank Junior School',
                  'Grove Primary School, Claremont, Cape Town'
                )
# geocode
coords <- geocode(school_names)
coords$name <- school_names
{% endhighlight %}

This gives a data frame:


|   lon    |    lat    |                      name                      |
|:--------:|:---------:|:----------------------------------------------:|
| 18.47534 | -33.96447 |       Rondebosch Boys Preparatory School       |
| 18.47343 | -33.96927 |    Groote Schuur Primary School, Rondebosch    |
| 18.49046 | -33.97614 |    Golden Grove Primary School, Rondebosch     |
| 18.47072 | -33.97929 | Claremont Primary School, Claremont, Cape Town |
| 18.48917 | -33.96159 |             Rosebank Junior School             |
| 18.45988 | -33.98285 |   Grove Primary School, Claremont, Cape Town   |

We can now use ggmap to plot these on a map:


{% highlight r %}
# load libraries for plotting
library(ggplot2)
library(ggrepel)
library(tidyr)
library(dplyr)

# clean names
coords <- coords %>% separate(name,into=c('clean_name','other'),sep = ',',remove = F)

# create bounding box for map and pad slightly bigger
lng_range <- diff(range(coords$lon))
lat_range <- diff(range(coords$lat))
margin <- 1

bbox <- c(
  left=min(coords$lon),
  bottom=min(coords$lat),
  right=max(coords$lon),
  top=max(coords$lat)
)
# add some padding
bbox <- bbox+c(-lng_range*margin,-lat_range*margin,lng_range*margin,lat_range*margin)

# download map from google
map <- get_map(location=bbox,zoom=14,color = 'bw')

# create a base map to work off of
p <- ggmap(map,
           extent = 'normal')+
  geom_point(data=coords,
             aes(x=lon,y=lat),col='darkblue',size=2)+
  coord_map(projection="mercator", 
              xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
              ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat))+
  labs(x='',y='')+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# plot schools
p+
  geom_text_repel(data=coords,
             aes(x=lon,y=lat,label=clean_name),col='darkblue')
{% endhighlight %}

![center](/figs/2017-02-25-choosing-a-school/unnamed-chunk-3-1.png)

In order to separate the areas on the map in to *zones* we need to define boundary areas around each school which are equi-distant locations to the nearest schools.

To do this lets define a grid across the map:


{% highlight r %}
resolution <- 100 # set grid resolution

# generate a sequence across latitudes
lats <- seq(bbox[2],bbox[4],length=resolution)
# generate a sequence across longitudes
lngs <- seq(bbox[1],bbox[3],length=resolution)

# create all combinations
all <- expand.grid(lng=lngs,lat=lats)

# plot
p+
  geom_vline(xintercept = lngs,alpha=0.3,col='darkblue')+
  geom_hline(yintercept = lats,alpha=0.3,col='darkblue')
{% endhighlight %}

![center](/figs/2017-02-25-choosing-a-school/unnamed-chunk-4-1.png)

The aim will be to now map each point to its closest school which we can do using **k nearest neighbours** with *k = 1*. Note here that I am not calculating the exact distance but rather just looking at the absolute latitude and longitude measures.


{% highlight r %}
# load class package
library(class)
# map each point to closest school
closest_school <- class::knn1(coords[,1:2],all,coords[,4])
all$school <- closest_school

# lets map again
p+
  geom_point(data=all,
             aes(x=lng,y=lat,col=school),size=2)+
  geom_point(data=coords,
             aes(x=lon,y=lat),size=3)+
  theme(legend.position = 'top')+
  labs(col='',caption='All points are mapped to their closest school')
{% endhighlight %}

![center](/figs/2017-02-25-choosing-a-school/unnamed-chunk-5-1.png)

To get a more granular view let's rerun the code segment with resolution set to 1000.




It would be nice to rather have a set of polygons define each region instead of a large set of points. It turns out that one can compute a convex hull which is the polygon that surrounds all the points.

Here is an example:


{% highlight r %}
# generate two variables
x <- rnorm(100)
y <- rnorm(100)
df <- data.frame(x=x,y=y)

# compute convex hull
ch <- df %>% 
    as.matrix %>%
    chull
# close polygon
ch <- c(ch,ch[1])

# plot
ggplot(df,aes(x,y))+
  geom_point()+
  geom_polygon(data=df[ch,],aes(x,y),col='red',fill=NA)+
  labs(title='Example of a convex hull')+
  theme_minimal()+
  theme(aspect.ratio=1)
{% endhighlight %}

![center](/figs/2017-02-25-choosing-a-school/unnamed-chunk-7-1.png)

Now let's apply this concept to our grid:


{% highlight r %}
# create a helper function to compute the convex hull
compute_convexhull <- function(df) {
  ch <- df %>% 
    select(lng,lat) %>% 
    as.matrix %>%
    chull
  return(df[c(ch,ch[1]),])
}

suppressPackageStartupMessages(library(purrr))

# compute convex hull using grid
convex_hulls <- all %>% 
  split(.$school) %>% 
  map(~compute_convexhull(.)) %>% 
  reduce(function(x,y) {
    union_all(x,y)
  })
{% endhighlight %}

The result looks like this:


|   lng    |    lat    |           school            |
|:--------:|:---------:|:---------------------------:|
| 18.48467 | -34.00335 |  Claremont Primary School   |
| 18.48477 | -34.00399 |  Claremont Primary School   |
| 18.48477 | -34.00412 |  Claremont Primary School   |
| 18.47292 | -34.00412 |  Claremont Primary School   |
| 18.47283 | -34.00399 |  Claremont Primary School   |
| 18.46943 | -33.99364 |  Claremont Primary School   |
| 18.46410 | -33.97743 |  Claremont Primary School   |
| 18.46291 | -33.97379 |  Claremont Primary School   |
| 18.46245 | -33.97238 |  Claremont Primary School   |
| 18.46226 | -33.97181 |  Claremont Primary School   |
| 18.46226 | -33.97168 |  Claremont Primary School   |
| 18.46245 | -33.97168 |  Claremont Primary School   |
| 18.47898 | -33.97615 |  Claremont Primary School   |
| 18.48036 | -33.97653 |  Claremont Primary School   |
| 18.48045 | -33.97685 |  Claremont Primary School   |
| 18.48072 | -33.97857 |  Claremont Primary School   |
| 18.48164 | -33.98432 |  Claremont Primary School   |
| 18.48467 | -34.00335 |  Claremont Primary School   |
| 18.52105 | -34.00412 | Golden Grove Primary School |
| 18.48486 | -34.00412 | Golden Grove Primary School |

This data frame should be enough to enclose our points in to a polygon.

Let's plot the convex hulls as polygons on our map.


{% highlight r %}
p+
  geom_polygon(data=convex_hulls,
               aes(x=lng,y=lat,group=school,fill=school),alpha=0.5)+
  geom_path(data=convex_hulls,
               aes(x=lng,y=lat,group=school,fill=school),alpha=0.5,col='white')+
  geom_point(data=coords,
             aes(x=lon,y=lat),col='darkblue',size=2)+
  labs(x='',y='',fill='',title='School Zones')
{% endhighlight %}

![center](/figs/2017-02-25-choosing-a-school/unnamed-chunk-10-1.png)

And there we have it! Now we know which areas are best suited to move to if we desire a particular school.

<!-- Here is a an interactive version using leaflet. -->
<!-- ```{r} -->
<!-- # load leaflet library -->
<!-- library(leaflet) -->

<!-- polys <- convex_hulls %>%  -->
<!--   split(.$school) -->

<!-- # generate some colours -->
<!-- factpal <- colorFactor(rainbow(nrow(coords)),levels(convex_hulls)) -->

<!-- # create a black and white base map and add school markers -->
<!-- leaflet_map <- leaflet(polys) %>%  -->
<!--   addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') -->

<!-- for (p in polys) { -->
<!--   leaflet_map <- leaflet_map %>%  -->
<!--     addPolygons(data=p, -->
<!--                 lng=~lng, -->
<!--                 lat=~lat, -->
<!--                 fillOpacity = 0.02, -->
<!--                 fillColor=~factpal(school), -->
<!--                 stroke=F) -->
<!-- } -->

<!-- leaflet_map <- leaflet_map %>%  -->
<!--   addMarkers(data=coords,lng=~lon,lat=~lat,label=~clean_name) -->

<!-- leaflet_map -->
<!-- ``` -->


