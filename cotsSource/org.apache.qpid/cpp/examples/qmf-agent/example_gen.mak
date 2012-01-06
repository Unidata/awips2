# This nmake file generates the C++ mapping from the example schema.
#
# The Visual Studio projects assume the existence of the generated files.
# These generated files must be created using this makefile for Windows 
# developers working from the source repository.

mgen_dir=..\..\..\cpp\managementgen

all: 
	python $(mgen_dir)\qmf-gen -o .\gen\qmf .\schema.xml
