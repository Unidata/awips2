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
 * <p>
 *  This class is a generic container for the parameters to the HDF
 *  compressed classes, with the ``ole'' encoding.
 * <p>
 *  Compression parameters are expressed as instances of sub-classes
 *  of this type.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */


public class HDFOldCompInfo extends HDFCompInfo {
    public int ctype; /* from COMP_NONE defines */
    public HDFOldCompInfo() {
        ctype = HDFConstants.COMP_NONE;
    } ;
}


