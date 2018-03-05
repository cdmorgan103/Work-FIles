install.packages("networkD3")

# Load package
library(networkD3)

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)




# Load data
data(MisLinks)
data(MisNodes)

MisLinks
MisNodes

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8, zoom=T)


URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)
Energy
# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)
head(Energy$links)



Cat<-read.csv("c://users/Chris/Desktop/Sankey/Categories.csv")
Net<-read.csv("c://users/Chris/Desktop/Sankey/Network.csv")

Cat<-read.csv("c://users/Chris/Documents/links.csv")
Net<-read.csv("c://users/Chris/Documents/nodes.csv")
asdf<-list(Cat,Net)


sankeyNetwork(Links = asdf$Net, Nodes = asdf$Cat, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)
?sankeyNetwork

head(Cat)
head(Energy$nodes)

write.csv(Energy$links,"links.csv")
head(Net)
head(Energy$links) 


library(networkD3)
nodes = data.frame("name" = 
                     c("Node A", # Node 0
                       "Node B", # Node 1
                       "Node C", # Node 2
                       "Node D"))# Node 3
links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from. 
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

library(networkD3)



nodes = data.frame("name" = 
                     c("All Patients", # Node 0
                       "Terminal Eligible (60%)", # Node 1
                       "Manual Check In (40%)", # Node 2
                       "Completes Check In With Terminal (20%)",# Node 3
                       "Needs Assistance With Terminal (40%)"))#Node 4
color_scale <- 
  "d3.scaleOrdinal()
     .domain(['All Patients', 'Terminal Eligible', 'Manual Check In', 'Completes Check In With Terminal', 
              'Needs Assistance With Terminal'])
     .range(['#464646', '#004165', '#ffc04c', '#94d9ce', '#007f80']);
  "


links = as.data.frame(matrix(c(
  0, 1, 60, 
  0, 2, 40,
  1, 3, 20, 
  1, 4, 40),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 15, nodePadding = 100, 
              units = "%",colourScale = color_scale)


