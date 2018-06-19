/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.mpe.util;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class PackGrib {

    /**
     * @param grib_lbl
     * @param pds_ext
     * @param iplen
     * @param grid_data
     * @param idim
     * @param xmissing
     * @param output_buffer
     * @param odim
     * @param length
     * @return
     */
    public int packgrib(int[] grib_lbl, String pds_ext, int pds_ext_len,
            float[] gridpoints, int gi_len, float miss_val,
            Byte[] output_buffer, int out_buf_len, int grib_len) {

        int status = 0;
        int off = 0;
        char pds_flag = 0x0;

        status = packIS(grib_lbl, output_buffer, out_buf_len, grib_len);
        if (status == 1) {
            return 1;
        }
        status = packPDS(grib_lbl, pds_ext, pds_ext_len, output_buffer,
                out_buf_len, grib_len, off, pds_flag);
        if (status == 1) {
            return 1;
        }

        // TODO Auto-generated method stub
        return 0;
    }

    /**
     * @param grib_lbl
     * @param pds_ext
     * @param pds_ext_len
     * @param output_buffer
     * @param out_buf_len
     * @param grib_len
     * @param off
     * @param pds_flag
     */
    private int packPDS(int[] grib_lbl, String pds_ext, int pds_ext_len,
            Byte[] out_buf, int out_buf_len, int grib_len, int off,
            char pds_flag) {
        short sign;
        int pds_len;
        int dval, yr, cent, hr, min;
        int n;
        Byte[] c_buf = out_buf;

        switch (grib_lbl[0]) {

        case 0:
            off = 32;
            grib_len += 24;
            /* length of PDS */
            setBits(out_buf, 24, off, 24);
            /* Edition number */
            setBits(out_buf, 0, off + 24, 8);

            /* force the decimal scale factor to zero */
            grib_lbl[22] = 0;
            break;

        case 1:
            off = 64;
            pds_len = (pds_ext_len == 0) ? 28 : 40 + pds_ext_len;
            grib_len += pds_len;
            /* length of PDS */
            setBits(out_buf, pds_len, off, 24);
            /* table version */
            setBits(out_buf, grib_lbl[2], off + 24, 8);
            break;

        default:
            System.out.println(String.format(
                    "Error: invalid GRIB edition number %d\n", grib_lbl[0]));
            return 1;
        }

        if (grib_len > out_buf_len) {
            System.out
                    .println("Error 6: dimension of output buffer not large enough\n");
            return 1;
        }
        /* center ID */
        setBits(out_buf, grib_lbl[3], off + 32, 8);

        return 0;

    }

    /**
     * @param grib_lbl
     * @param output_buffer
     * @param out_buf_len
     * @param grib_len
     */
    private int packIS(int[] grib_lbl, Byte[] output_buffer, int out_buf_len,
            int grib_len) {

        switch (grib_lbl[0]) {
        case 0:
            grib_len = 4;
            break;
        case 1:
            grib_len = 8;
            /* GRIB Edition - set to 1 */
            setBits(output_buffer, 1, 56, 8);
            break;
        }
        if (grib_len > out_buf_len) {
            System.out
                    .println("Error 5: dimension of output buffer not large enough\n");
            return 1;
        }
        /* "GRIB" message */
        setBits(output_buffer, 0x47524942, 0, 32);
        return 0;
    }

    /**
     * @param buf
     * @param loc
     * @param off
     * @param bits
     */
    private void setBits(Byte[] buf, int loc, int off, int bits) {
        int mask = 0;
        int size = Integer.SIZE * 8;
        int wskip, bskip, lclear, rclear, left, right, more;
        int n;
        // int off = offin / 8;
        // int bits = bitsin / 8;
        /* no work to do */
        if (bits == 0) {
            return;
        }

        /*
         * create a mask to use when right-shifting (necessary because different
         * compilers do different things when right-shifting a signed bit-field)
         */
        mask = 1;
        for (n = 1; n < size; n++) {
            mask <<= 1;
            mask++;
        }
        if (bits > size) {
            System.out
                    .println(String.format(
                            "Error: packing %d bits into a %d-bit field\n",
                            bits, size));
            System.out.println(String.format(
                    "Error 4: packing %d bits into a %d-bit field\n", bits,
                    size));
            return;
        } else {
            /* get number of words and bits to skip before packing begins */
            wskip = off / size;
            bskip = off % size;
            lclear = bskip + bits;
            rclear = size - bskip;
            left = (rclear != size) ? (buf[wskip] & (mask << rclear)) : 0;
            if (lclear <= size) {
                /* all bits to be packed are in the current word */
                /* clear the field to be packed */
                right = (lclear != size) ? (buf[wskip] & ~(mask << (size - lclear)))
                        : 0;
                /* fill the field to be packed */
                buf[wskip] = (byte) (left | right | (loc << (rclear - bits)));
            } else {
                /* bits to be packed cross a word boundary(ies) */
                /* clear the bit field to be packed */
                more = bits - rclear;
                buf[wskip] = (byte) (left | ((loc >> more) & ~(mask << rclear)));
                /* clear the next (or part of the next) word and pack those bits */
                while (more > size) {
                    more -= size;
                    buf[++wskip] = (byte) ((loc >> more) & ~(mask << size));
                }
                wskip++;
                more = size - more;
                right = (more != size) ? (buf[wskip] & ~(mask << more)) : 0;
                buf[wskip] = (byte) (right | (loc << more));
            }
        }

    }

}
