\" -*-nroff-*-
\name{phon}
\alias{phon}
\title{
Generate a segment list
}
\usage{
phon(utterances, query=paste(find, "!=''"), find="Phoneme", 
     ret="?", extn=options()$labextn,
     dirc = paste(getenv("SHOME"), "/library/mu/ttsfiles", sep=""))
}
\arguments{
\item{utterances}{
A mu+ utterance vector. 
}
\item{query}{
The specification of segments to be found. See later for the details of
the syntax of this argument.
}
\item{find}{
The level in the utterance hierarchy at which the search will take
place. That is the level of the segments who's properties you are
specifying in \code{query}.
}
\item{ret}{
The type of segment you wish to be returned. This will normally be the
same as the level specified in \code{find} but may specify a higher level in
the utterance hierarchy.
}
\item{extn}{
The file extension of the label files in the database. 
}
\item{dirc}{
The directory where the labed template file and other system files are
stored. Defaults to the directory \code{ttsfiles} in the Splus library
directory where Mu+ is installed.  
}}
\value{
A mu+ segment list (an object of class "segs") corresponding to those
segments matching the query.
}
\details{
This function is the central part of the mu+ system, it generates
segment lists where each segment matches the query specified as the
first argument.  This documentation provides a brief description of the
syntax of \code{phon} queries and the other options to the function. Further
information, including a tutorial, can be found in other parts of the
mu+ documentation. 
}
\section{Query Syntax}{
Queries in \code{phon} consist of a set of conditions which must hold for
the segments we are interested in.  The conditions consist of a level
name (corresponding to a column heading in LABED), a comparator and a
value. For example: 
"Phonetic='A'"
constrains the segments to have the Phonetic label 'A'.  In addition,
it is possible to refer to labels to the left and right of the current
label using the syntax:
"Phonetic[1]='A'"
which would match segments which are followed by an 'A' Phonetic label,
and: 
"Phonetic[-1]='A'"
which matches segments preceeded by an 'A' Phonetic label. These
patterns would normally be used in conjuction with other patterns as
shown below. 


Legal comparators for boolean conditions are equal (=) and not equal
(!=).


The value part of the condition (the right hand side of the expression)
can be either a literal value or a disjuction. The pattern 'A/B/C' will
constrain the value to be either 'A' or 'B' or 'C'.  Thus the query:
"Phonetic='A/E/I/O/U/V'"
might be used to generate a segment list of these six vowels.  


Simple expressions like these may be joined with \code{and} or \code{or} into
more complex conditions. For example, to find the above six vowels only
in strong syllables we write: 
"Phonetic='A/E/I/O/U/V' and Syllable=S"
If both \code{and} and \code{or} are used in a query, \code{and} binds more tightly
than \code{or}.  So, for example, 
"Phonetic='A' or Phonetic='E' and Syllable=S"
would be read as:
"Phonetic='A' or (Phonetic='E' and Syllable=S)"
To force the alternative interpretation, explicit brackets can be used: 
"(Phonetic='A' or Phonetic='E') and Syllable=S"
}
\section{Find and Return Types}{


When \code{phon} searches the database it examines objects at a particular
level of the utterance hierarchy specified by the \code{find} argument.  The
condition specified in the \code{query} refers to objects at this level.
So, for example, if you are searching for all strong Syllable level
segments the \code{find} argument will be \code{"Syllable"} and the condition: 
"Syllable='S'"
It is also valid to specify conditions on levels 
above
the find level, for example: 
"Syllable='S' and Word='C'"
will find strong syllables in content words. Note that you may not
specify conditions below the find level.  


For some searches you may wish to return an object higher in the
hierarchy than the find level, for example if you want all Word
segments containing strong Syllables.  Since it is not possible to
specify \code{"Syllable=}S'"' when the find level is \code{"Word"} you must make
the find level \code{"Syllable"} and the \code{return} level \code{"Word"} (using the
\code{return} argument). For example:
phon("Syllable='S'", find="Syllable", return="Word")
}
\section{Positional Conditions}{
You may also specify conditions involvind the position of segments in
the hierarchy using the \code{Start}, \code{Medial} and \code{End} conditions.  To
find all phonemes at the start of syllables use: 
phon("Start(Syllable,Phoneme)=T", find="Phoneme")
Where \code{Start(x,y)} means that element \code{y} is at the start of an element
of type \code{x}.  Similarly \code{End(x,y)} is used to find elements at the end
of other segments and \code{Medial(x,y)} to find elements which are neither
at the start or end of the other segment.  So to find all non-Syllable-medial i:
phonemes we use the query: 
phon("Phoneme='i:' and Medial(Syllable,Phoneme)=F", find="Phoneme")
}
\section{Numerical Conditions}{
The condition \code{Num(x,y)} can be used to find segments with a specific
number of sub-segments, such as three-syllable words or syllables with
two phonemes.  For example, \code{Num(Word,Syllable)=2}  will find all
\code{Word} level segments which dominate exactly two \code{Syllable} segments.
In this case the comparitors can be any of =, !=, >, >=, <, <=.


Further examples of the use of phon are given in the user manual. 
}
\seealso{
mkdb, track, frames
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
