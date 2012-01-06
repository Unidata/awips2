/****************************************************************************
 * NCSA HDF5                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf-java/COPYING file.                                                  *
 *                                                                          *
 ****************************************************************************/

package ncsa.hdf.hdf5lib.exceptions;


/**
 *  <p>
 *  The class HDF5JavaException returns errors from the Java
 *  wrapper of theHDF5 library.
 *  <p>
 *  These errors include Java configuration errors, security
 *  violations, and resource exhaustion.
 */
public class HDF5JavaException extends HDF5Exception 
{
    /**
     * Constructs an <code>HDF5JavaException</code> with no
     * specified detail message.
     */
    public HDF5JavaException() {
        super();
    }

    /**
     * Constructs an <code>HDF5JavaException</code> with the
     * specified detail message.
     *
     * @param   s   the detail message.
     */
    public HDF5JavaException(String s) {
        super(s);
    }
}
