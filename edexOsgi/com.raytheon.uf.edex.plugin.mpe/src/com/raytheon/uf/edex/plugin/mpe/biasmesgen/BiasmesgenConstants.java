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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.text.SimpleDateFormat;

/**
 * Common, centralized declaration of constants that will be utilized by
 * {@link Biasmesgen}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2016 5576       bkowal      Initial creation
 * May 20, 2016 5576       bkowal      Defined constants required to generate the output
 *                                     bias table.
 * Jun 13, 2016 5576       bkowal      Renamed to identify the application that uses this
 *                                     constants.
 * Jun 16, 2016 5576       bkowal      C/C++ sizeof(char) is actually 1.                                    
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasmesgenConstants {

    private static final String BIAS_TABLE_FILE_NAME_DATE_FMT = "ddHHmm";

    public static final SimpleDateFormat biasTableSDF = new SimpleDateFormat(
            BIAS_TABLE_FILE_NAME_DATE_FMT);

    public static final String BIAS_TABLE_FILE_NAME_FMT = "%03d.%03d.%d.%d.%6s";

    /*
     * Number of hours to generate bias tables for.
     */
    public static final int BIAS_GENERATION_HOURS = 2;

    /*
     * Number of rows in the Bias Table.
     */
    public static final short BIAS_TABLE_ROWS = 10;

    /*
     * Number of columns in the Bias Table.
     */
    public static final short BIAS_TABLE_COLUMNS = 5;

    public static final short BIAS_MESSAGE_CODE = 15;

    public static final short BIAS_TABLE_NUM_BLOCKS = 2;

    public static final short BIAS_BLOCK_DIVIDER = -1;

    public static final short BIAS_BLOCK_ID = 1;

    public static final short BIAS_VERSION = 0;

    /*
     * C/C++ sizeof(char)
     */
    private static final int SIZE1 = 1;

    /*
     * C/C++ sizeof(short)
     */
    private static final int SIZE2 = 2;

    /*
     * C/C++ sizeof(int)
     */
    private static final int SIZE4 = 4;

    /*
     * This value is a direct copy paste of the undocumented value from
     * /rary.ohd.pproc/src/biasmesgen/TEXT/create_biastable_mesg.pgc:
     * biastable.block_len (line 146 - on May 2016).
     */
    public static final int BIAS_BLOCK_LENGTH = SIZE2 * (6 * 2 + 5) + SIZE1
            * (4 * 2) + BIAS_TABLE_COLUMNS * SIZE4 * BIAS_TABLE_ROWS;

    /*
     * This value is a direct copy paste of the undocumented value from
     * /rary.ohd.pproc/src/biasmesgen/TEXT/create_mesg_hdr.c: MesgHdr.mesg_len
     * (line 60 - on May 2016).
     */
    public static final int BIAS_MIN_MSG_LENGTH = SIZE2 * 5 + SIZE4 * 2;

    public static class AppsDefaults {

        public static final String BIAS_MSG_DIR = "bias_message_dir";

        public static final String FXA_LOCAL_SITE = "fxa_local_site";

        public static final String SEND_LOCAL_BIAS_WHEN_RFC_BIAS_MISSING = "send_local_bias_when_rfc_bias_missing";

        protected AppsDefaults() {
        }
    }

    protected BiasmesgenConstants() {
    }
}