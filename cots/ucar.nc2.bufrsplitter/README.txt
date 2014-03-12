This contains the BUFR splitter classes from netcdf-java version 4.3.20. 
The classes have been modified to work with netcdf-java version 4.2. Modifications have 
been tagged with a comment containing the issue number related to the work (#2905).

This will not be needed if/when the ucar netcdf-java dependency is upgraded to 4.4.
It is also recommended that the newer BufrSplitter2.java be used instead of BufrSPlitter.java.
BufrSplitter2 has an improved interface that allows for use in other packages without modification.