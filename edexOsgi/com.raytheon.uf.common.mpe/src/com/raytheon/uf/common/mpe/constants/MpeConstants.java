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
package com.raytheon.uf.common.mpe.constants;

/**
 * Defines constants common to all of the converted MPE applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2016 5699       bkowal      Initial creation
 * Aug 25, 2016 5631       bkowal      Added {@link AppsDefaults#FXA_LOCAL_SITE}.
 * Sep 01, 2016 4628       bkowal      Added Apps Defaults properties utilized by
 *                                     Process Grib Files.
 * Sep 13, 2016 5631       bkowal      Added {@link #RANGE_CHECK_DEFAULT}.
 * Sep 27, 2016 5631       bkowal      Added {@link #RADAR_DEFAULT} and {@link #ACC_MIN}.
 * Oct 11, 2016 5631       bkowal      Added {@link #FACTOR_PRECIP}, {@link #FACTOR_OTHER},
 *                                     and {@link #MOSAIC_DEFAULT}.
 * Aug 08, 2017 6334       bkowal      Made the constructor private.                                    
 * Apr 06, 2018 7184       bkowal      Relocated to common so that defined constants can be used
 *                                     by both Viz and EDEX.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class MpeConstants {

    public static final double MISSING_VALUE = -9999.0;

    /*
     * Constant declared in: /rary.ohd.pproc/inc/mpe_constants.h
     */
    public static final double RANGE_CHECK_DEFAULT = -999.0;

    /*
     * Constant declared in: hpe_fieldgen/TEXT/empe_constants.h
     */
    public static final float RADAR_DEFAULT = -9.0f;

    /*
     * Constant declared in multiple places: hpe_fieldgen/TEXT/read_dhr_data.c
     */
    public static final float ACC_MIN = 0.01f;

    /*
     * Constant declared in: /rary.ohd.pproc/inc/mpe_constants.h
     */
    public static final double FACTOR_PRECIP = 100.0;

    /*
     * Constant declared in: /rary.ohd.pproc/inc/mpe_constants.h
     */
    public static final double FACTOR_OTHER = 1.0;

    /*
     * Constant declared in: /rary.ohd.pproc/inc/mpe_constants.h
     */
    public static final double MOSAIC_DEFAULT = -9.0;

    public static class AppsDefaults {

        public static final String FXA_LOCAL_SITE = "fxa_local_site";

        public static final String MPE_GRIB_DIR = "mpe_grib_dir";

        public static final String RFCWIDE_XMRG_DIR = "rfcwide_xmrg_dir";

        public static final String MPE_SEND_QPE_TO_SBN = "mpe_send_qpe_to_sbn";

        public static final String MPE_QPE_SBN_DIR = "mpe_qpe_sbn_dir";

        public static final String MPE_QPE_GRIB_SBN_DIR = "mpe_qpe_grib_sbn_dir";

        public static final String RFCWIDE_OUTPUT_DIR = "rfcwide_output_dir";

        public static final String D2D_INPUT_DIR = "d2d_input_dir";

        public static final String MPE_STATION_LIST_DIR = "mpe_station_list_dir";

        public static final String MPE_POINT_PRECIP_DIR = "mpe_point_precip_dir";

        public static final String MPE_POINT_TEMPERATURE_DIR = "mpe_point_temperature_dir";

        public static final String MPE_LOAD_HOURLYPC = "mpe_load_hourlypc";

        public static final String MPE_TEMPERATURE_WINDOW = "mpe_temperature_window";

        public static final String DQC_PREPROCESSOR_BASETIME = "dqc_preprocessor_basetime";

        public static final String MPE_SITE_ID = "mpe_site_id";

        public static final String MPE_AREA_NAMES = "mpe_area_names";
    }

    private MpeConstants() {
    }
}