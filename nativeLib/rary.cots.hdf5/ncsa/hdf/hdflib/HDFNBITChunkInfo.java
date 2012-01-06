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
 *  NBIT compressed chunked class.
 * <p>
 * In this case, the information is the start bit, len, sign extension
 * and fill.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */


public class HDFNBITChunkInfo extends HDFChunkInfo {

    public int[] chunk_lengths = new int[HDFConstants.MAX_VAR_DIMS];
    public int start_bit = 0;
    public int bit_len = 0;
    public int sign_ext = 0;
    public int fill_one = 0;

    public HDFNBITChunkInfo() {
        ctype = HDFConstants.HDF_NBIT;
    };

    public HDFNBITChunkInfo( int[] cl, int sb, int bl, int se, int fo) {
        ctype = HDFConstants.HDF_NBIT;
        chunk_lengths = cl;
        start_bit = sb;
        bit_len = bl;
        sign_ext = se;
        fill_one = fo;
    }

}
