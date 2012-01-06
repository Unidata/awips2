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
 *  RLE compressed classes, with the ``new'' type encoding.
 * <p>
 *  In this case, there is no auxilliary information.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */
public class HDFRLECompInfo extends HDFNewCompInfo {

    public HDFRLECompInfo () {
        ctype = HDFConstants.COMP_CODE_RLE;
    }

}
