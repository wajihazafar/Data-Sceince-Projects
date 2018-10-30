## Document Finder
Document Finder provides a fast and easy way to query from directory of documents stored in HDFS. Document Finder finds the relevant document from a list of documents in a directory depending on the query. Querying multiple documents and getting relevant results involves two steps, where in the first step the inverted index with TF-IDF scores for all the documents are calculated and are saved in the HDFS and in second step relevant results are retrieved with maximum TF-IDF scores based on the query. The Document Finder is very fast in returning the relevant documents since it precalculates the TF-IDF scores and uses MapReduce. The best part of the Document Finder is that it first creates the TF-IDF table and saves it in a parquet format. And in the second step it retrieves the relevant document using that table. 
This reduces the unnecessary computation for calculating the TF-IDF scores again and again while fetching the document.
## Implementation of TF-IDF calculation using MapReduce
# Term Frequency (TF):
Term Frequency also called as TF. It calculates the number of times a word (term) arises in a document. The terms and their frequency on each of the document is measured by the term frequency.
# Inverse Document Frequency (IDF):
The search concept is to find out applicable documents similar the query. To find certain words that arise too repeatedly have slight power in defining the significance.
The logarithm method is used for calculating the IDF.
