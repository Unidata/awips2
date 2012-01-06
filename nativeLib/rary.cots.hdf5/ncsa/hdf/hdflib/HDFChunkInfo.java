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
 *   This is a generic class to represent the HDF chunk_info
 *   union, which contains parameters for the different
 *   chunking schemes.
 * <p>
 * The variant parameters are expressed as sub-classes of this
 * class.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 *
 */


public class HDFChunkInfo {
    public int ctype;
    public int[] chunk_lengths = new int[HDFConstants.MAX_VAR_DIMS];
    public int comp_type = HDFConstants.COMP_CODE_NONE;
    public HDFCompInfo cinfo = null;

    public HDFChunkInfo() {
        ctype = HDFConstants.HDF_NONE;
    } ;

    public HDFChunkInfo( int[] cl, int ct, HDFCompInfo ci ) {
        if (ct == HDFConstants.COMP_CODE_NONE) {
		ctype = HDFConstants.HDF_CHUNK;
        } else {
		ctype = HDFConstants.HDF_COMP | HDFConstants.HDF_CHUNK;
        }
        chunk_lengths = cl;
        comp_type = ct;
        cinfo = ci;
    }

    public HDFChunkInfo(int[] cl) {
        ctype = HDFConstants.HDF_CHUNK;
        chunk_lengths = cl;
    }
}
