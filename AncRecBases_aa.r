#############################
### PLOT ANCESTRAL HOXA10 ###
#############################

library(ape)
library(protr)
library(RColorBrewer)

fasta<-readFASTA("ancestral.fasta")
tree<-read.tree("Tetrapoda_species_rename_sel.tre")

fas$names<-names(fasta)
fas$seq<-sapply(fasta, "[[", 1)

colors<-vector()
colors<-c(colors, brewer.pal(n =9, name = "Reds"))
colors<-c(colors, brewer.pal(n =9, name = "Purples"))
colors<-c(colors, brewer.pal(n =9, name = "Greens"))
colors<-c(colors, brewer.pal(n =9, name = "Blues"))
colors<-c(colors, brewer.pal(n =9, name = "Greys"))

colors2<-sample(colors)

pdf("HA_ancestral.pdf", height=30, width=10)

for(i in 1:nchar(fas$seq[[1]])){
	list<-sapply(strsplit(sapply(fas$seq, "[[", 1), ""), "[[", i)
	if(length(unique(list))>1){
	let2<-list[tree$tip.label]

	old<-sort(unique(let2))
	new<-1:length(old)
	let2[let2 %in% old] <- new[match(let2, old, nomatch = 0)]
	cols2<-as.numeric(let2)

	let<-list[(length(tree$tip.label)+1):length(list)]
	let[let %in% old] <- new[match(let, old, nomatch = 0)]
	cols<-as.numeric(let)
	

	plot(tree, cex=0.7, label.offset = 3, main=paste("HoxA10 - aa =", i))
	tiplabels(toupper(let2), frame = "circle", bg=colors2[cols2], cex=0.5)
	nodelabels(toupper(let), frame = "circle", bg=colors2[cols], cex=0.6)
	}
}
dev.off()

