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
 *  This class is a container for the parameters to the HDF
 *  SZIP compression algorithm.
 * <p>
 * In this case, the only parameter is the ``level'' of deflation.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */


public class HDFSZIPCompInfo extends HDFNewCompInfo {

    public int bits_per_pixel;
    public int options_mask;
    public int pixels;
    public int pixels_per_block;
    public int pixels_per_scanline;

    public HDFSZIPCompInfo() {
        ctype = HDFConstants.COMP_CODE_SZIP;
    } ;
    public HDFSZIPCompInfo(
    int bits_per_pixel_in,
    int options_mask_in,
    int pixels_in,
    int pixels_per_block_in,
    int pixels_per_scanline_in) {
        ctype = HDFConstants.COMP_CODE_SZIP;
    } ;
}


