---
layout: post
title: "Visualising my Strava network interactively using R"
author: 'Niklas von Maltzahn'
output: html_document
tags: [r, strava, rStrava,igraph,visNetwork,tidyverse]
category: r
comments: yes
image: /figs/2017-04-17-visualising-my-strava-network-2/featured_image.png
---



In my [previous post](https://niklasvm.github.io/r/2017/04/07/visualising-my-strava-network-1.html) I wrote about how I went about visualising my Strava network using ggraph. This is a short follow up looking at how one can visualise the network interactively using the *visNetwork* package.

Let start by loading our required packages and reading in the data we downloaded previously:


{% highlight r %}
# read in packages
library(tidyverse)
library(igraph)
library(visNetwork)
library(widgetframe)

# read in data set
all_friends <- readRDS('./all_friends.rds')
{% endhighlight %}

Here's a quick reminder of whats in it:


{% highlight r %}
colnames(all_friends)
{% endhighlight %}



{% highlight text %}
##  [1] "follower"       "followee"       "id"             "username"      
##  [5] "resource_state" "firstname"      "lastname"       "city"          
##  [9] "state"          "country"        "sex"            "premium"       
## [13] "created_at"     "updated_at"     "badge_type_id"  "profile_medium"
## [17] "profile"        "friend"         "my_friend"
{% endhighlight %}

First let's prep the data using igraph:


{% highlight r %}
# extract some meta data to describe the vertices
meta_data <- all_friends %>% select(-follower,-followee)
meta_data <- meta_data %>% filter(!duplicated(id))

# create graph from data frame
graph <- graph_from_data_frame(all_friends,directed = T,vertices = meta_data)

# create flag for whether someone is a friend, follower, both or neither
followers <- neighbors(graph,mode = 'in',v=as.character(1411289))$name
friends <- neighbors(graph,mode = 'out',v=as.character(1411289))$name
V(graph)$type <- case_when(
  V(graph)$name %in% as.character(1411289) ~ 'me',
  V(graph)$name %in% followers & V(graph)$name %in% friends ~ 'friend and follower',
  V(graph)$name %in% followers ~ 'follower',
  V(graph)$name %in% friends ~ 'friend'
)

# reduce network to my immediate neighbourhood
n <- neighborhood(graph,order=1,nodes=which(V(graph)$name %in% as.character(1411289)))
g1 <- subgraph(graph,v = unlist(n))
{% endhighlight %}

Here's the new stuff relevant to visNetwork:


{% highlight r %}
# get nodes and prep
nodes <- get.data.frame(g1,what='vertices') # create data frame of vertices/nodes
nodes$id <- nodes$name # code id column for use with visNetwork

# get edges
edges <- get.data.frame(g1,what='edges') %>% select(from,to) %>% distinct # create data frame of edges

# plot with visNetwork
vn1 <- visNetwork(nodes,edges) %>% 
  visIgraphLayout(physics=F,smooth=F) # disable physics for faster rendering, only draw single connections
{% endhighlight %}

<iframe src="/figs/2017-04-17-visualising-my-strava-network-2/vn1.html" width="100%" height="550px" scrolling="no" frameborder="0"></iframe>

This plot is zoomable, draggable and interactive but still quite boring. We can add quite a lot more customisation:


{% highlight r %}
# change the shapes of nodes
nodes$shape <- case_when(
  nodes$type=='me' ~ 'circle',
  nodes$type=='follower' ~ 'square',
  nodes$type=='friend' ~ 'triangle',
  nodes$type=='friend and follower' ~ 'star'
)

# change the colours of nodes
nodes$color <- case_when(
  nodes$type=='me' ~ 'red',
  nodes$type=='follower' ~ 'blue',
  nodes$type=='friend' ~ 'green',
  nodes$type=='friend and follower' ~ 'orange'
)

# add a tooltip and label (I generate a random string to protect my network's privacy)
nodes$title=paste(stringi::stri_rand_strings(nrow(nodes),5),stringi::stri_rand_strings(nrow(nodes),5))
nodes$label=nodes$title

# colour edges
edges$color.color <- 'lightgray'
edges$arrows <- 'to'

# create legend info
lnodes <- nodes %>% select(type,color,shape) %>% rename(label=type) %>% distinct %>% mutate(title=label)

set.seed(pi) # for reproducibility
vn2 <- visNetwork(nodes,edges,main='My Strava Network',submain='Click and Zoom to interact',width='100%') %>% 
  visIgraphLayout(physics=F,smooth=T) %>%  # disable physics for faster rendering, only draw single connections
  visInteraction(tooltipDelay=0,hover = T) %>% 
  visLegend(addNodes = lnodes,useGroups=F)
{% endhighlight %}

<iframe src="/figs/2017-04-17-visualising-my-strava-network-2/vn2.html" width="100%" height="550px" scrolling="no" frameborder="0"></iframe>

Pretty simple actually. Combining the interactivity with shiny could lead to some pretty cool apps.

Thanks for reading.

