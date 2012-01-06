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
 *  The class HDFException returns errors from the HDF
 *  library.
 *  <p>
 *  Two sub-classes of HDFException are defined:
 *  <p>
 *  <ol>
 *  <li>
 *   HDFLibraryException -- errors raised the HDF library code
 *  <li>
 *   HDFJavaException -- errors raised the HDF Java wrapper code
 *  </ol>
 *  <p>
 *  These exceptions will be sub-classed to represent specific
 *  error conditions, as needed.
 *  <p>
 *  The only specific exception currently defined is
 *  HDFNotImplementedException, indicating a function that is part
 *  of the HDF API, but which cannot or will not be implemented
 *  for Java.
 */
public class HDFException extends Exception 
{
    static public final String OutOfMemoryMessage="ERROR: HDF Library: Out of memory";
    static public final String HDFExceptionMessage="ERROR: HDF Library Error";
    static public final String HDFMessage="ERROR: Unknown HDF Error";

    int HDFerror;
    String msg;

    public HDFException() {
        HDFerror = 0;
    }

    public HDFException(String s) {
        msg = s;
    }

    public HDFException(int err) {
        HDFerror = err;
    }

    public String getMessage() {
        return msg;
    }
}
