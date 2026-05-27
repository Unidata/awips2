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
package com.raytheon.uf.common.mpe.gribit2;

import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.IPathManager;

/**
 * Constants utilized during the xmrg to grib conversion.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2016 4619       bkowal      Initial creation
 * Jul 20, 2016 4619       bkowal      Added {@link #GRIBIT_ROOT}.
 * Aug 01, 2016 4619       bkowal      Added {@link #ORIGINATING_CENTER_ID} and
 *                                     {@link #PDS_NGRID}.
 * Aug 11, 2016 4619       bkowal      Added {@link #SUPPORTED_GRID_218} and
 *                                     {@link #XMRG_IGNORE_CONSTANT}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class XmrgToGribConstants {

    public static final int SUB_CENTER_0 = 999;

    public static final int ORIGINATING_CENTER_ID = 9;

    public static final int DEFAULT_ACCUMULATION_HOURS = 1;

    public static final int DEFAULT_NGRID = 240;

    public static final int SUPPORTED_GRID_218 = 218;

    public static final int PDS_NGRID = 255;

    public static final int ACCUM_HOURS_GROUP = 1;

    public static final float XMRG_IGNORE_CONSTANT = -50.0f;

    private static final String ACCUM_HOUR_EXTRACT_REGEX = ".*(\\d{2})";

    public static final Pattern accumHourExtractPattern = Pattern
            .compile(ACCUM_HOUR_EXTRACT_REGEX);

    public static final String PROC_CONTAINS_DHR = "DHR";

    public static final String PROC_CONTAINS_DSP = "DSP";

    public static final String PROC_STARTS_QPA = "QPA";

    public static final String PROC_STARTS_QPM = "QPM";

    public static final int QUARTER_HRAP_GRID_FACTOR = 4;

    public static final String GRIBIT_ROOT = "mpeLookup"
            + IPathManager.SEPARATOR + "gribit";

    public static final int PDS_BYTE_COUNT = 28;

    public static class AppsDefaults {

        public static final String GRIB_SET_SUBCENTER_0 = "grib_set_subcenter_0";

        public static final String AWIPS_RFC_ID = "awips_rfc_id";

        public static final String AWIPS_SEND_ID = "awips_send_id";

        public static final String HPE_HRAP_GRID_FACTOR = "hpe_hrap_grid_factor";

        public static final String GRIB_CONVERT_DATA = "grib_convert_data";

        public static final String GRIB_PTBL_SEARCH = "grib_ptbl_search";

        protected AppsDefaults() {
        }
    }

    protected XmrgToGribConstants() {
    }
}