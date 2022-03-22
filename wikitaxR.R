#' parse_rayyan
#' @description A function to scrape taxonomic classification information from Wikipedia from a vector of highest/finest taxonomic classifications available (e.g. genus names)
#' @details Very beta version - uploaded mainly for a friend. Will improve upon if get time
#' @author Matt Lloyd Jones
#' @param highestassignment A vector of highest taxonomic classifications (e.g. genus names)
#'
#' @return
#' @export

wikitaxaR<-function(highestassignment){

# MULTIPLE SEARCHES EXAMPLE ------------------------------------------------------------
  
#install.packages('textreadr')
library(textreadr)
  
searchurl_stem<-"http://en.wikipedia.org/wiki/Special:Search/"
taxonomic_ranks<-c('Domain:','Phylum:','Class:','Order:','Family:','Genus:')
taxonomy_df<-data.frame(matrix(ncol=7, nrow=0))
colnames(taxonomy_df)<-taxonomic_ranks

for (o in 1:length(highestassignment)){
  
  #create search string for the highest taxonomic assignment
  searchurl_full<-paste(searchurl_stem,highestassignment[o],sep='')
  print(paste(o,searchurl_full))
  
  #read in the page that the search redirects to
  wiki_content <- textreadr::read_html(searchurl_full)
  
  #if the search has no hits, then go to the next most relevant result
  if(TRUE%in%grepl("does not exist",wiki_content)==TRUE){
    #parse the page
    parsed <- xml2::read_html(searchurl_full)
    #get all the hyperlinks
    hyperlinks<-rvest::html_attr(rvest::html_nodes(parsed, "a"), "href")
    #get the hyperlink after 'Articles_for_creation', which is the top most relevant alternative link
    hyperlink_needed<-hyperlinks[which(grepl('Articles_for_creation',hyperlinks))+1]
    #make it a full url
    searchurl_full_alternative<-paste('https://en.wikipedia.org',hyperlink_needed, sep='')
    #read in the page
    wiki_content<-wiki_content <- textreadr::read_html(searchurl_full_alternative)
  }
  
  #find the lines that match to the taxonomic rank titles (e.g. Domain, Phylum etc.)
  taxonomic_rank_titles_lineids<-which(wiki_content%in%taxonomic_ranks)
  #find the lines that match to the taxanomic rank names by adding 1 (line below titles e.g. Bacteria, Proteobacteria etc.)
  taxonomic_rank_names_lineids<-taxonomic_rank_titles_lineids+1
  #retrieve the taxonomic rank names from the highest taxonomic assignment to the lowest
  taxonomic_rank_names<-wiki_content[taxonomic_rank_names_lineids]
  #change the names of the vector to match the taxonomic rank titles
  names(taxonomic_rank_names)<-wiki_content[taxonomic_rank_titles_lineids]
  
  #append to taxonomy dataframe
  match_to_taxonomydfcols<-match(names(taxonomic_rank_names),colnames(taxonomy_df))
  taxonomy_df[o,match_to_taxonomydfcols]<-taxonomic_rank_names
  
}

return(taxonomy_df)

}