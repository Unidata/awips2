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
 *  The class HDF5Exception returns errors from the
 *  Java HDF5 Interface.
 *  <p>
 *  Two sub-classes of HDF5Exception are defined:
 *  <p>
 *  <ol>
 *  <li>
 *   HDF5LibraryException -- errors raised the HDF5 library code
 *  <li>
 *   HDF5JavaException -- errors raised the HDF5 Java wrapper code
 *  </ol>
 *  <p>
 *  These exceptions are sub-classed to represent specific
 *  error conditions, as needed.  In particular,
 *  HDF5LibraryException has a sub-class for each major error
 *  code returned by the HDF5 library.
 *
 */
public class HDF5Exception extends Exception 
{
    protected String detailMessage;

    /**
     * Constructs an <code>HDF5Exception</code> with no specified
     * detail message.
     */
     public HDF5Exception() {
        super();
     }

    /**
     * Constructs an <code>HDF5Exception</code> with the specified
     * detail message.
     *
     * @param   message   the detail message.
     */
    public HDF5Exception(String message) {
        super();
        detailMessage = message;
    }

    /**
     * Returns the detail message of this exception
     *
     * @return  the detail message
     *          or <code>null</code> if this object does not
     *          have a detail message.
     */
    public String getMessage() {
    return detailMessage;
    }
}
