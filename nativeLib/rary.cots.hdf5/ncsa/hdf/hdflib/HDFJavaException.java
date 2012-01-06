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
 *  The class HDFJavaException returns errors from the Java
 *  wrapper of theHDF library.
 *  <p>
 *  These errors include Java configuration errors, security
 *  violations, and resource exhaustion.
 */
public class HDFJavaException extends HDFException 
{
    String msg;

    public HDFJavaException() {
        HDFerror = 0;
    }

    public HDFJavaException(String s) {
        msg = "HDFLibraryException: "+s;
    }

    public String getMessage() {
        return msg;
    }
}
