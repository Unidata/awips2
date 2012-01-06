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
 *  ``Skipping Huffman'' compression classes.
 * <p>
 *  In this case, the information is the skip size
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */
public class HDFSKPHUFFCompInfo extends HDFNewCompInfo {

    public int skp_size;

    public HDFSKPHUFFCompInfo() {
        ctype = HDFConstants.COMP_CODE_SKPHUFF;
    } ;
}


