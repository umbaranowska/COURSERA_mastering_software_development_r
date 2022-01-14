##### setting S4 class LongitudinalData #####

setClass("LongitudinalData",
         slots = list(id = 'numeric',
                      visit = 'numeric',
                      room = 'character',
                      value = 'numeric',
                      timepoint = 'numeric'))

##### print(LongitutinalData) #####

setGeneric("print")
setMethod("print",
          c(x = "LongitudinalData"),
          function(x){
            paste("Longitudinal dataset with", length(unique(x@id)),"subjects")
          })

##### convert data into LongitudinalData with make_LD function #####

make_LD = function(data){
  S4 = new('LongitudinalData',
           id = data$id,
           visit = data$visit,
           room = data$room,
           value = data$value,
           timepoint = data$timepoint)
  return(S4)
}