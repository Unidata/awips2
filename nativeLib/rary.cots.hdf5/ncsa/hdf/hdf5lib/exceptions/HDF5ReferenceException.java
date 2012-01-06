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



public class HDF5ReferenceException extends HDF5LibraryException 
{
    /**
     * Constructs an <code>HDF5ReferenceException</code> with
     * no specified detail message.
     */
    public HDF5ReferenceException() {
        super();
    }

    /**
     * Constructs an <code>HDF5ReferenceException</code> with
     * the specified detail message.
     *
     * @param   s   the detail message.
     */
    public HDF5ReferenceException(String s) {
        super(s);
    }
}
