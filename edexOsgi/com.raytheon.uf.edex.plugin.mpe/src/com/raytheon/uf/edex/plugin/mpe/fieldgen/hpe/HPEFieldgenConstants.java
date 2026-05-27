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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

/**
 * Common, centralized declaration of constants that will be utilized by
 * {@link HPEFieldgen}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2016 5631       bkowal      Initial creation
 * Aug 31, 2016 5631       bkowal      Added {@link AppsDefaults#GEO_ST3_ASCII} and
 *                                     {@link AppsDefaults#GEO_ST3_BIN}.
 * Sep 01, 2016 4628       bkowal      Centralized common Apps Defaults property
 *                                     constants.
 * Sep 13, 2016 5631       bkowal      Added {@link AppsDefaults#RFCWIDE_GAGELOC_DIR}
 * Sep 19, 2016 5631       bkowal      Added {@link #HEIGHT_DEFAULT} and 
 *                                     {@link AppsDefaults#RFCWIDE_PRISM_DIR}.
 * Sep 27, 2016 5631       bkowal      Added Apps_defaults token constants utilized
 *                                     by HPE Field Gen DHRMOSAIC generation.
 * Oct 05, 2016 5631       bkowal      Added {@link #MISBIN_DEFAULT} and {@link AppsDefaults#HPE_RFC_BIAS_LAG}.
 * Oct 11, 2016 5631       bkowal      Added {@link #ID_DEFAULT}, {@link #DHR_PROC_FLAG}, and
 *                                     {@link AppsDefaults#DHRMOSAIC_SAVE_GRIB}.
 * Oct 18, 2016 5631       bkowal      Added {@link AppsDefaults#ST3_AUTO_GRAPHIC_SCALE}.
 * Oct 19, 2016 5631       bkowal      Added additional Apps Defaults properties used to convert
 *                                     hpe fieldgen mosaics to grib files.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class HPEFieldgenConstants {

    /*
     * Based on the define with the same name in:
     * hpe_fieldgen/TEXT/empe_constants.h.
     */
    public static final double HEIGHT_DEFAULT = 100000.0;

    public static final short MISBIN_DEFAULT = 1;

    public static final int ID_DEFAULT = 0;

    public static final String DHR_PROC_FLAG = "DHR     ";

    public static class AppsDefaults {

        public static final String HPE_TIMELAG = "hpe_timelag";

        public static final String HPE_BASE_RADAR_MOSAIC = "hpe_base_radar_mosaic";

        public static final String HPE_QPE_FIELDTYPE = "hpe_qpe_fieldtype";

        public static final String HPE_GENERATE_LIST = "hpe_generate_list";

        public static final String HPE_DEL_GAGE_ZEROS = "hpe_del_gage_zeros";

        public static final String RFCW_RFCNAME = "rfcw_rfcname";

        public static final String DHR_WINDOW = "dhr_window";

        public static final String DSP_WINDOW = "dsp_window";

        public static final String DSP_DURATION = "dsp_duration";

        public static final String ST3_DATE_FORM = "st3_date_form";

        public static final String HPE_GAGE_QC = "hpe_gage_qc";

        public static final String HPE_LOCBIAS_1HR_RERUN = "hpe_locbias_1hr_rerun";

        public static final String HPE_LOAD_MISBIN = "hpe_load_misbin";

        public static final String HPE_USE_LOCBIAS = "hpe_use_locbias";

        public static final String HPE_RUN_NOWCAST = "hpe_run_nowcast";

        public static final String HPE_DUALPOL_ON = "hpe_dualpol_on";

        public static final String HPE_BIAS_SOURCE = "hpe_bias_source";

        public static final String HPE_RFC_BIAS_FLAG = "hpe_rfc_bias_flag";

        public static final String WHFS_GEODATA_DIR = "whfs_geodata_dir";

        public static final String GEO_ST3_BIN = "geo_st3_bin";

        public static final String GEO_ST3_ASCII = "geo_st3_ascii";

        public static final String RFCWIDE_GAGELOC_DIR = "rfcwide_gageloc_dir";

        public static final String RFCWIDE_PRISM_DIR = "rfcwide_prism_dir";

        public static final String HPE_SAVE_DHRHEIGHT = "hpe_save_dhrheight";

        public static final String HPE_DHRHEIGHT_DIR = "hpe_dhrheight_dir";

        public static final String HPE_SAVE_DHRINDEX = "hpe_save_dhrindex";

        public static final String HPE_DHRINDEX_DIR = "hpe_dhrindex_dir";

        public static final String DHRMOSAIC_SAVE_NETCDF = "dhrmosaic_save_netcdf";

        public static final String DHRMOSAIC_NETCDF_DIR = "dhrmosaic_netcdf_dir";

        public static final String DHRMOSAIC_NETCDF_ID = "dhrmosaic_netcdf_id";

        public static final String DHRMOSAIC_SAVE_GIF = "dhrmosaic_save_gif";

        public static final String DHRMOSAIC_GIF_DIR = "dhrmosaic_gif_dir";

        public static final String DHRMOSAIC_GIF_ID = "dhrmosaic_gif_id";

        public static final String DHRMOSAIC_SAVE_JPEG = "dhrmosaic_save_jpeg";

        public static final String MPE_MISBIN_DIR = "mpe_misbin_dir";

        public static final String DPR_GRID_DIR = "dpr_grid_dir";

        public static final String DHR_GRID_DIR = "dhr_grid_dir";

        public static final String HPE_RFC_BIAS_LAG = "hpe_rfc_bias_lag";

        public static final String DHRMOSAIC_SAVE_GRIB = "dhrmosaic_save_grib";

        public static final String HPE_MAX_ERMOSAIC_DIR = "hpe_max_ermosaic_dir";
        
        public static final String ST3_AUTO_GRAPHIC_SCALE = "st3_auto_graphic_scale";

        protected AppsDefaults() {
        }
    }

    protected HPEFieldgenConstants() {
    }
}