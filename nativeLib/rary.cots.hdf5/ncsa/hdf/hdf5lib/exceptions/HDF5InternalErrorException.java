/****************************************************************************
 * NCSA HDF5
 *
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
 *  The class HDF5LibraryException returns errors raised by the HDF5
 *  library.
 *  <p>
 *  This sub-class represents HDF-5 major error code
 *       <b>H5E_INTERNAL</b>
 */

public class HDF5InternalErrorException extends HDF5LibraryException 
{
    /**
     * Constructs an <code>HDF5InternalErrorException</code> with
     * no specified detail message.
     */
    public HDF5InternalErrorException() {
        super();
    }

    /**
     * Constructs an <code>HDF5InternalErrorException</code> with
     * the specified detail message.
     *
     * @param   s   the detail message.
     */
    public HDF5InternalErrorException(String s) {
        super(s);
    }
}
