# create.schema.track <- function(basePath=NULL,name,columnName=name,unitSuffix=NULL,extension=NULL,hasDeepStructure=FALSE){
#   o <- list(basePath=basePath,name=name,columnName=columnName,unitSuffix=unitSuffix,fileExtension=extension)
#   class(o) <- 'emuDB.schema.track'
#   invisible(o)
# }
# 
# create.schema.annotationDescriptor <- function(name=NULL,basePath=NULL,extension=NULL,type=NULL,timeFactor=NULL,levelDefinitions=NULL){
#   o <- list(name=name,basePath=basePath,extension=extension,type=type,timeFactor=timeFactor,levelDefinitions=levelDefinitions)
#   class(o) <- 'emuDB.schema.annotationDescriptor'
#   invisible(o)
# }

# ## Create emuDB attribute definition object
# ## 
# ## @param name name of the level
# ## @param type level type (ITEM,EVENT,SEGMENT)
# ## @return object of class emuDB.schema.attributeDefinition
# ## @author Klaus Jaensch
# ## @keywords emuDB attribute level Emu
# ## 
# create.schema.attributeDefinition <- function(name, type='STRING',labelGroups=NULL){
#   if(is.null(labelGroups)){
#     o <- list(name=name,type=type)
#   }else{
#     o <- list(name=name,type=type,labelGroups=labelGroups)
#   }
#   class(o) <- c('emuDB.schema.attributeDefinition','list')
#   invisible(o)
# }

# ## Create emuDB level definition object
# ## 
# ## @param name name of the level
# ## @param type level type (ITEM,EVENT,SEGMENT)
# ## @param attributeDefinitions list of attribute definitions
# ## @return object of class emuDB.levelDefinition
# ## @author Klaus Jaensch
# ## @keywords emuDB level Emu
# ## 
# create.schema.levelDefinition <- function(name,type=NULL,attributeDefinitions=NULL){
#   if(is.null(attributeDefinitions)){
#     defAttrDef=create.schema.attributeDefinition(name=name)
#     attributeDefinitions=list(defAttrDef)
#   }
#   o <- list(name=name,type=type,attributeDefinitions=attributeDefinitions)
#   class(o) <- 'emuDB.schema.levelDefinition'
#   invisible(o)
# }

# ## Create emuDB link definition object
# ## Represents the hierarchical information of EMU hlb files
# ## 
# ## @param name name of the link (optional)
# ## @param type link type ("ONE_TO_ONE", "ONE_TO_MANY", "MANY_TO_MANY")
# ## @param superlevelName name of the super level (link from)
# ## @param sublevelName name of the sublevel (link to)
# ## @return object of class emuDB.schema.linkDefinition
# ## @author Klaus Jaensch
# ## @keywords emuDB database schema link Emu
# ## 
# create.schema.linkDefinition <- function(name=NULL,type,superlevelName,sublevelName){
#   o <- list(name=name,type=type,superlevelName=superlevelName,sublevelName=sublevelName)
#   class(o) <- 'emuDB.schema.linkDefinition'
#   invisible(o)
# }

# create.schema.databaseDefinition <- function(name,UUID=uuid::UUIDgenerate(),mediafileBasePathPattern=NULL,mediafileExtension=NULL,ssffTrackDefinitions=list(),levelDefinitions=list(),linkDefinitions=list(),EMUwebAppConfig=NULL,annotationDescriptors=NULL,tracks=NULL,flags=NULL){
#   o <- list(name=name,UUID=UUID,mediafileBasePathPattern=mediafileBasePathPattern,mediafileExtension=mediafileExtension,ssffTrackDefinitions=ssffTrackDefinitions,levelDefinitions=levelDefinitions,linkDefinitions=linkDefinitions,EMUwebAppConfig=EMUwebAppConfig,annotationDescriptors=annotationDescriptors,tracks=tracks,flags=flags)
#   class(o) <- c('list','emuDB.schema.databaseDefinition')
#   #rTypes=list(levelDefinitions=c('list','emuDB.schema.levelDefinition',linkDefinitions=c('list','emuDB.schema.linkDefinition')
#   invisible(o)
# }

# summary.emuDB.schema.databaseDefinition<-function(schema,header=TRUE){
#   cat("SSFF track definitions:\n")
#   for(td in schema[['ssffTrackDefinitions']]){
#     cat("\tName: ",td[['name']],"\tColumn: ",td[['columnName']],"\tExt.: ",td[['fileExtension']],"\n")
#   }
#   cat("\n")
#   cat("Level definitions:\n")
#   for(ld in schema[['levelDefinitions']]){
#     print(ld)
#     cat("\tAttribute definitions:\n")
#     for(ad in ld[['attributeDefinitions']]){
#       cat("\t")
#       print(ad)
#     }
#     cat("\n")
#   }
#   cat("\n")
#   lblGrps=schema[['labelGroups']]
#   if(length(lblGrps)>0){
#     cat("Database label group definitions:\n")
#     for(lblGrp in lblGrps){
#       print.emuDB.schema.labelGroup(lblGrp)
#     }
#   }
#   cat("\n")
#   cat("Link definitions:\n")
#   for(ld in schema[['linkDefinitions']]){
#     print(ld)
#   }
#   
# }
# 
# print.emuDB.schema.labelGroup<-function(labelGroup){
#   labelVals=c()
#   for(lblGrpVal in labelGroup[['values']]){
#     labelVals=c(labelVals,lblGrpVal)
#   }
#   cat("\tLabel group: ",labelGroup[['name']],": ",labelVals,"\n")
# }
# 
# print.emuDB.schema.levelDefinition<-function(levelDefinition){
#   cat(levelDefinition[['name']],"\ttype:\t",levelDefinition[['type']],"\n")
# }
# 
# print.emuDB.schema.attributeDefinition<-function(attributeDefinition){
#   cat(attributeDefinition[['name']],"\ttype:\t",attributeDefinition[['type']],"\n")
#   lblGrps=attributeDefinition[['labelGroups']]
#   if(length(lblGrps)>0){
#     cat("\n\tLabel group definitions:\n")
#     for(lblGrp in lblGrps){
#       print.emuDB.schema.labelGroup(lblGrp)
#     }
#   }
# }
# 
# print.emuDB.schema.linkDefinition<-function(linkDefinition){
#   cat(toString(linkDefinition),"\n")
# }
# toString.emuDB.schema.linkDefinition<-function(linkDefinition){
#   paste(linkDefinition[['superlevelName']],'->',linkDefinition[['sublevelName']],linkDefinition[['type']],sep="\t")
# }

# # persistence filters
# # the properties listed are not persisted to JSON files
# emuR.persist.filters.DBconfig=list()
# emuR.persist.filters.DBconfig[[1]]=c('annotationDescriptors')
# emuR.persist.filters.DBconfig[[2]]=c('tracks')
# emuR.persist.filters.DBconfig[[3]]=c('flags')
# emuR.persist.filters.DBconfig[[4]]=c('ssffTrackDefinitions','basePath')
# emuR.persist.filters.DBconfig[[5]]=c('mediafileBasePathPattern')
# emuR.persist.filters.DBconfig[[6]]=c('maxNumberOfLabels')
# emuR.persist.filters.DBconfig[[7]]=c('itemColNames')
# emuR.persist.filters.DBconfig[[8]]=c('basePath')
# emuR.persist.filters.DBconfig[[9]]=c('DBconfigPath')
# 
# # persistent class hierarchy
# # the calss names are applied to the object hierarchy loaded from JSON file
# emuR.persist.class.DBconfig=list()
# emuR.persist.class.DBconfig[['emuDB.schema.databaseDefinition']]=character(0)
# emuR.persist.class.DBconfig[['emuDB.schema.levelDefinition']]=list(c('levelDefinitions','*'))
# emuR.persist.class.DBconfig[['emuDB.schema.linkDefinition']]=list(c('linkDefinitions','*'))
# emuR.persist.class.DBconfig[['emuDB.schema.attributeDefinition']]=list(c('levelDefinitions','*','attributeDefinitions','*'))

# load.emuDB.DBconfig<-function(DBconfigFilePath){
#   # with warn=TRUE and some files
#   # R complains about incomplete (last) line
#   # See https://stat.ethz.ch/pipermail/r-help/2006-July/108654.html
#   # TODO does problem with jsonlite still exist ?
#   dbCfgJSONLns=readLines(DBconfigFilePath,warn=FALSE)
#   dbCfgJSON=paste(dbCfgJSONLns,collapse='')
#   dbCfgPersisted=jsonlite::fromJSON(dbCfgJSON,simplifyVector=FALSE)
#   
#   # unmarshal schema object (set class names)
#   schema=unmarshal.from.persistence(dbCfgPersisted,emuR.persist.class.DBconfig)
#   return(schema)
# }

