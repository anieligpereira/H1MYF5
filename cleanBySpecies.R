############################################
### CLEAN BY SPECIES - HIGHEST AA NUMBER ###
############################################

library(prtotr)

fasta<-readFASTA("all_HOXD10_NCBI_ENS.fasta")

SPP<-sapply(strsplit(names(fasta), split="\\_"), "[",1)

fasta_clean<-list()
for(i in 1:length(unique(SPP))){
	sp<-unique(SPP)[i]
	temp<-grep(sp, names(fasta))	
	temp2<-list()
	nch<-vector()
	for(n in temp){
		seq<-seq<-gsub("X", "", fasta[[n]])
		nch<-c(nch, nchar(seq))
		temp2<-c(temp2, fasta[[n]])
	}
	fasta_clean[[i]]<-temp2[[which(nch==max(nch))[1]]]
	names(fasta_clean)[i]<-names(fasta)[temp[1]]
}