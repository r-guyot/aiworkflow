


bm25_search <- function(reference_docs, query_sentence, top_results=5, remove_zero_match=TRUE) {
  
# example  
#   text <- "Tokenization: The text is first tokenized into a sequence of word pieces. The [CLS] token is prepended to the input for sentence-level tasks, and [SEP] tokens are inserted to separate sentences and indicate the end.
# Embedding: Each token is converted into vectors using an embedding matrix, similar to models like Word2Vec. Positional embeddings are added to these token embeddings to preserve information about the order of words, while segment embeddings distinguish different sentences.
# Encoders: The vectors pass through multiple layers of encoders, each composed of self-attention mechanisms and feed-forward neural networks. These layers iteratively refine the representation of each token based on the context provided by all other tokens in the sequence.
# Output: The final layer outputs a sequence of embeddings. Typically, for sentence-level tasks, the embeddings of the [CLS] token are the aggregate representation of the entire input. Embeddings of individual tokens are utilized for fine-grained tasks or combined via operations like max or sum pooling to form a singular dense representation."
  
  # reference_docs <- split_text_as_sentences(text)
  
  # query_sentence <- "what is Tokenization and how does it work?"
  # top_results <- 5
  
  results <- superml::bm_25(document=query_sentence, corpus=reference_docs, top_n=top_results)
  results <- as.data.frame(results)
  results$docs <- row.names(results)
  row.names(results) <- NULL
  if (remove_zero_match==T) {
    results <- results |> dplyr::filter(results>0)
  }
  
  results <- results |> head(top_results)
  
  return(results)
  
}