/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf-java/COPYING file.                                                   *
 *                                                                          *
 ****************************************************************************/

package ncsa.hdf.hdflib;

/**
 *  <p>
 *  HDFNotImplementedException indicates a function that is part
 *  of the HDF API, but which cannot or will not be implemented
 *  for Java.
 *  <p>
 *  For instance, C routines which take Unix FILE objects
 *  as parameters are not appropriate for the Java interface
 *  and will not be implemented.  These routines will raise
 *  an HDFNotImplementedException.
 */

public class HDFNotImplementedException  extends HDFJavaException 
{
    String msg;

    public HDFNotImplementedException() {
        HDFerror = 0;
    }

    public HDFNotImplementedException(String s) {
        msg = "HDFJavaException: HDF function not implmented (yet): "+s;
    }

    public String getMessage() {
        return msg;
    }
}
