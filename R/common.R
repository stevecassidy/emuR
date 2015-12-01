
# Internal common vars

internalVars<- new.env(parent = emptyenv())
# list of lists holding emuDB handles
internalVars$sqlConnections = list()
# internal vars for tests
internalVars$testingVars = list()
internalVars$testingVars$inMemoryCache = F