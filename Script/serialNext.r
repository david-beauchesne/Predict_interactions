# Generating serial number for files in case file already exists
# http://stackoverflow.com/questions/25429557/how-to-create-a-new-output-file-in-r-if-a-file-with-that-name-already-exists
serialNext = function(prefix){
    if(!file.exists(prefix)){
        return(prefix)
    }
        i=1
    repeat {
        f = paste(unlist(strsplit(prefix, '[.]'))[1],i,'.',unlist(strsplit(prefix, '[.]'))[2],sep="")
        if(!file.exists(f)){return(f)}
        i=i+1
     }
  }
