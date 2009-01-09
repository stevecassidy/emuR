"trackinfo" <-
function(template)
{
	unlist(strsplit(tclvalue(.TclEval(sprintf("emuR::gettracks %s",template))), " "))
}

