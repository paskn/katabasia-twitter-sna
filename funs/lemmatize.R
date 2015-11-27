# takes character vector and returns lemmatized text
# requires python3 and pymystem3 module (https://github.com/Digsolab/pymystem3)

lemmatize <- function (texts) {
  path2script='"./funs/lemmatize.py"'
  lemm_loop <- NULL
  for (i in 1:length(texts)) {
    print(paste("Progress: ", i, "/", length(texts), sep=""))
    output <- system2("python3", args=c(path2script, paste('"',texts[i],'"')),
                      stdout=TRUE)
    lemm_loop <- c(lemm_loop, output[1])
  }
  return(lemm_loop)
}
