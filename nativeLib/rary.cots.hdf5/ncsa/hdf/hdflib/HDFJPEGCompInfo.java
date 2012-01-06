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
 *  JPEG compression class.
 * <p>
 * In this case, the parameters are the quality and baseline.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */


public class HDFJPEGCompInfo extends HDFOldCompInfo {

           /* Struct to contain information about how to compress */
            /* or decompress a JPEG encoded 24-bit image */

            public int    quality;    /* Quality factor for JPEG compression, should be from */
            /* 0 (terrible) to 100 (very good) */

            public int    force_baseline;     /* If force_baseline is set to TRUE then */
            /* quantization tables are limited to */
            /* 0..255 for JPEG baseline compability */
            /* This is only an issue for quality */
            /* settings below 24 */

    public HDFJPEGCompInfo() {
                ctype = HDFConstants.COMP_JPEG;
    }

    public HDFJPEGCompInfo(int qual, int fb) {
                ctype = HDFConstants.COMP_JPEG;
        quality = qual;
        force_baseline = fb;
    }

}
