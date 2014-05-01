This directory contains the xsl stylesheets used to generate the code from the
OpenAMQ protocol specification. They require an XSLT2.0 processor, currently 
Saxon 8 is used.

The generation process is controlled by the framing.xsl stylesheet. This performs
several phases of transformation, using the other stylesheets. The transformation
in each phase is defined in a separate file, and these are designed to also allow
then to be run individually. 

The generation takes the amq.asl as input, it also requires that the path to the 
directory where the base asl definitions reside (those definitions that the main
amq.asl defintion inherits from) be passed in via a paramter called asl_base.

The files involved are as follows:

    framing.xsl    The control file for the entire generation process

    prepare1.xsl   Resolves the separate files that make up the protocol 
                   definition, building a single tree containing all the
                   information as a set of 'frame' elements, each of which
                   has attributes for its name, and ids for the class and
                   method it refers to and contains zero or more field 
                   elements. 

                   A method id is generated based on the order of the 
                   method elements within the class elements in the original
                   specification. The class id is taken from the enclosing
                   class element.  

    prepare2.xsl   Resolves domains into their corresponding types. (This is
                   much easier when all the information is in a single tree, 
                   hence the separate frame). 

    prepare3.xsl   Converts names into valid java names and augments the
                   tree to include information that makes the subsequent
                   generation phase simpler e.g. the index of boolean 
                   fields as several boolean flags are combined into a
                   single byte. (This is easier once the domains have been
                   resolved, hence the separate phase).

    java.xsl       Generates java classes for each frame, and a registry of 
                   all the frames to a 'magic' number generated from their 
                   class and method id.

    utils.xsl      Contains some utility methods for e.g. producing valid
                   java names.

For debugging the framing.xsl can output the intermediary files. This can be
enabled by uncommenting the relevant lines (a comment explaining this is
provided inline).     
 
 