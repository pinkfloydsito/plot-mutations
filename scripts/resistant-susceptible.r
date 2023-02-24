library(ggplot2)
library(ggrepel)
library(randomcoloR)

data <- read.csv("./files/resistant-susceptible.csv", header=TRUE, stringsAsFactors =FALSE, sep=';')
x=as.list(data['Position'])
y=as.list(data['yaxis'])
grouping=as.list(data['Domains'])$Domains
df <- data.frame(x = as.list(x), y=as.list(y), grouping=grouping)
x <- df$Position
y <- df$yaxis
df$Position <- as.numeric(df$Position)
df$Value <- as.numeric(df$yaxis)
mutations <- data['Mutation']
mutations <- as.list(mutations)
mutations <- unlist(mutations, use.names=FALSE)

graph <- ggplot(df, aes(grouping, x = x, xmax=165, y = y, ymin=-2, ymax=2, label=mutations)) 
graph <- graph + geom_hline(yintercept = 0, color="black", linewidth=5) 
graph <- graph + geom_segment(aes(x = x, xend = x, y=0, yend=y), color="black")
graph <- graph + geom_point(color="orange", size = 2) + theme_light() + geom_text_repel()
graph <- graph + geom_text(label="Mutations found in resistant strains", x=140, y=2.5, color="black" ) 
graph <- graph + geom_text(label="Mutations found in sensitive strains", x=140, y=-2.5, color="black" )

idxPosition = 8
idxDomain = 9
lastPosition <- 0
domainsRange <- list()
domainIntervals <- list()
currentDomain <- ""
currentLookup <- list()

for (idx in 1:nrow(data)) {
    if(idx == 1) {
        domainsRange <- append(domainsRange, data[idx, ][idxDomain]$Domains)
        currentDomain <- data[idx, ][idxDomain]$Domains
        currentLookup <- c( data[idx, ][idxDomain]$Domains,  data[idx, ][idxPosition]$Position)

        next 
    }

    if(idx == nrow(data)) {
        currentLookup <- append( currentLookup,  data[idx, ][idxPosition]$Position)
        domainIntervals <- append(domainIntervals, list(currentLookup))
        next 
    }

  if(currentDomain != data[idx, ][idxDomain]$Domains) {
    domainsRange <- append(domainsRange, data[idx, ][idxDomain]$Domains)
    currentDomain <- data[idx, ][idxDomain]$Domains

    if(idx > 1) {
        currentLookup <- append( currentLookup,  lastPosition)
        domainIntervals <- append(domainIntervals, list(currentLookup))
    }
    currentLookup <- c( data[idx, ][idxDomain]$Domains,  data[idx, ][idxPosition]$Position)

  }
    lastPosition <- data[idx, ][idxPosition]$Position
}

for(idx in 1:length(domainIntervals)) {
    domain <- domainIntervals[idx][[1]][1]
    xstart <- as.numeric(domainIntervals[idx][[1]][2])
    xend <- as.numeric(domainIntervals[idx][[1]][3])

    if(domain != "") {
        color <- randomColor(1, luminosity="dark")
        graph <- graph + geom_rect(xmin=xstart,xmax=xend,fill=factor(color), ymin=-.05,ymax=.05, size=0.2, alpha=0.3)
        graph <- graph + geom_label(size=2.5, x= (xend - xstart)/2 + xstart, y = 0, label = domain, colour = "white", fontface = "bold", fill = factor(color), label.padding = unit(0.5, "lines"))
    }

}

graph
