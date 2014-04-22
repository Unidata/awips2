bufr-3.0.jar has been modified from the released 4.2 version of the UCAR BUFR IOSP.
The java file ucar.nc2.iosp.bufr.tables.BufrTables has been modified to fix a bug
in the getWmoTableB(int) method. The modified java source file is included along
side the class files in the jar. This was done under ticket #2905.

The bug involves an inconsistency in logic when choosing the key to the Table B
version map. This results in the version 14 Table B being replaced by
the version 13 Table B. The code has been rewritten in newer versions (4.3,4.4).
This fix is not a long term solution. Moving to later versions isn't an immediate option
due to an issue retrieving time and location information in those versions.
The time and location variables are not populated in the structure variables.
A post has been made to the netcdf-java mailing list inquiring about the
issue. Another roadblock is the grib dependency on the UCAR grib library. The grib
API has changed in the later versions and would require reworking and testing
the grib 1 decoder. Aside from those issues, later versions of the library
also require updating the joda.time jar used by ws-security and adding the
google protocol buffer library and jdom2.
