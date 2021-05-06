# Indexer-Search
Searches through a corpus and returns most relevant search results. Relevant indexing information is preprocessed in the indexer prior to searching.

1.) In order to run the program, first add a corpus (xml file) of your choice. 
(A default mediumWiki.XML. is included for convenience)

2.) Then run the Indexer file insering the arguments as specified in the file. 
3.) Afterwards run the querier to search the the corpus. 

Output - The code returns the top 10 files from the corpus that match your search! (in order of relevance)

Relevancy is determined by the term frequency and inverse document frequencies of the words and documents. 
More information about those terms can be found here: https://en.wikipedia.org/wiki/Tf%E2%80%93idf

Additionally if pageRank is chosen to be included in the scoring (relevancy) criteria then the authority that each document holds is taken into consideration. 

Authority is determined by these principles: 
1.) The more pages that link to a page j, the more authoritative j should be.
2.) The more authoritative those pages are, the still more authoritative j should be.
3.) The fewer links those pages have to pages other than j, the more authoritative j should be. 
4.) The closer j is to another page k (measured by the number of links that must be followed to get from j to k), the more k's score should influence j's.

The formulas used to calculate this are shown in the Indexer. 
