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
 *  The class HDFException returns errors raised by the HDF
 *  library.
 *  <p>
 *  In principle, this includes any and all errors possible
 *  from the HDF library.  However, most error conditions
 *  are not yet detected in this version of the Java
 *  interface.  This will be added in future releases.
 *
 *  The only HDF library error currently raised are errors
 *  in Hopen, such as ``file not found''.
 */


public class HDFLibraryException extends HDFException 
{

    int HDFerror;
    String msg;

    public HDFLibraryException() {
        HDFerror = 0;
        msg = null;
    }

    public HDFLibraryException(String s) {
        msg = "HDFLibraryException: "+s;
    }

    public HDFLibraryException(int err) {
        HDFerror = err;
    }

    public String getMessage() {
        if (msg != null) {
            return msg;
        }

        String s;
        try {
            s = HDFLibrary.HEstring(HDFerror);
        } catch (HDFException e) {
            s = new String("HDF error number: "+HDFerror+", HEstring failed");
        }
        msg = "HDFLibraryException: "+s;
        return msg;
    }
}
