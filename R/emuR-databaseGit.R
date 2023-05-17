# currently not used
# update_emuDBgit <- function(emuDBhandle, verbose = TRUE){
#   
#   
#   ######################
#   # get/create repo
#   repo = tryCatch({
#     git2r::repository(emuDBhandle$basePath)
#   }, warning = function(warning_condition) {
#     if(verbose){
#       print("got following warning:", warning_condition)
#     }
#   }, error = function(error_condition) {
#     # no repo present so make one
#     if(verbose){
#       print("Init a new repository")
#     }
#     git2r::init(emuDBhandle$basePath)
#     
#   }, finally = {
#     # print("done!")
#   })
#   
#   ######################
#   # create .gitignore if it doesn't already exist
#   gitignorePath = file.path(emuDBhandle$basePath, 
#                             ".gitignore")
#   
#   if(!file.exists(gitignorePath)){
#     # only ignore cache 4 now
#     readr::write_lines(c("*_emuDBcache.sqlite"), gitignorePath)
#   }
#   
#   ######################
#   # check if remote:origin is ahead of local
#   remotes = git2r::remotes(repo)
#   
#   if(length(remotes) > 0){
#     # todo: check if remote ahead and warn if so
#     # simply print out suggested git2r command
#     # (git fetch needed)
#   }
#   
#   # add everything and commit with fixed message
#   
#   status = git2r::status(repo)
#   if (length(status$staged) == 0 && length(status$unstaged) == 0 && length(status$untracked) == 0) {
#     if(verbose){
#       # TODO add sha1 here as well
#       print("INFO: No changes to commit!")
#     }
#   } else {
#     git2r::add(repo, "*")
#     commit = git2r::commit(repo, 
#                            message = "emuR::load_emuDB() git auto snapshot")
#     
#     if(verbose){
#       print(paste0("INFO: emuDB git commit SHA1: ", 
#                    stringr::str_sub(commit$sha, start = 1, end = 7)))
#     }
#   }
#   
#   
# }
