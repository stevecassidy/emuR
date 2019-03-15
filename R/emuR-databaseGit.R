update_emuDBgit <- function(emuDBhandle){
  
  
  ######################
  # get/create repo
  repo = tryCatch({
    git2r::repository(emuDBhandle$basePath)
  }, warning = function(warning_condition) {
    print("got following warning:", warning_condition)
  }, error = function(error_condition) {
    # no repo present so make one
    print("Init a new repository")
    git2r::init(emuDBhandle$basePath)
    
  }, finally = {
    # print("done!")
  })
  
  ######################
  # create .gitignore if it doesn't already exist
  gitignorePath = file.path(emuDBhandle$basePath, 
                            ".gitignore")
  
  if(!file.exists(gitignorePath)){
    # only ignore cache 4 now
    readr::write_lines(c("*_emuDBcache.sqlite"), gitignorePath)
  }
  
  ######################
  # check if remote:origin is ahead of local
  remotes = git2r::remotes(repo)
  
  if(length(remotes) > 0){
    # todo: check if remote ahead and warn if so
  }
  
  # add everything and commit with fixed message
  git2r::add(repo, "*")
  
  git2r::commit(repo, "emuR::load_emuDB() git auto snapshot")

}