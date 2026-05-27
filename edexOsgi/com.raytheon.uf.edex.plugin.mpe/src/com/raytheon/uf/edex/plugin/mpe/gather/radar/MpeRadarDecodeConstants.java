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
 * Contractor Address:   = 6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                       = 402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import java.text.SimpleDateFormat;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.gather.MpeRadarGatherConstants;

/**
 * Constants for decoding DHR and DSP files. Based on
 * decode_dhr_dsp/TEXT/decode_constants.h
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 * Jul 18, 2018 5588       mapeters    Fix DSP dir constants
 *
 * </pre>
 *
 * @author nabowle
 */

public class MpeRadarDecodeConstants {

    public static class AppsDefaults {
        public static final String DSP_GRID_DIR = "dsp_grid_dir";

        public static final String DSP_PROD_DIR = "dsp_prod_dir";

        public static final String DHR_GRID_DIR = "dhr_grid_dir";

        public static final String DHR_PROD_DIR = MpeRadarGatherConstants.AppsDefaults.DHR_PROD_DIR;
    }

    public static final ThreadLocal<SimpleDateFormat> filenameDateFormat = TimeUtil
            .buildThreadLocalSimpleDateFormat("MMddyyyHHmm'Z'",
                    TimeUtil.GMT_TIME_ZONE);

    public static final int LEVEL_OUT_OF_RANGE = 255;

    public static final int NUM_LEVEL_DBZ = 256;

    public static final int MAX_AZIMUTH = 360;

    public static final int MAX_RANGE = 230;

    public static final int MAX_IHRAP = 524;

    public static final int MAX_JHRAP = 524;

    public static final int NUM_ROW = MAX_JHRAP;

    public static final int NUM_COL = MAX_IHRAP;

    public static final int BEYOND_RANGE = -99;

    public static final int BEYOND_GRID = -77;

    public static final double GRID_COORD_I = 433.0;

    public static final double GRID_COORD_J = 433.0;

    public static final double ANGLE_THRESH = 9.81E-6;

    public static final double R2KO = 249.6348607;

    public static final double PRIME = 105.0;

    public static final double B_CON = 0.025;

    public static final int IKA = 40;

    public static final double KA = 40.0;

    public static final int OFFSET = 263;

    public static final float DEFAULT_RADAR_1KM = -0.9F;

    public static final float MAX_RATE = 999.0F;

    public static final float MIN_RATE = -999.0F;

    public static final double CONST = 135.0;

    public static final double EARTH_RADIUS = 6380.0;

    public static final double FLOAT_MISSING = -999.0;

    public static final double ONE = 1.0;

    public static final double HALF = 0.5;

    public static final double DEGREE_TO_RADIAN = 0.01745329;

    public static final double SCALE_FACTOR = 1000.0;

    public static final double EARTH_RADIUS_SQ = EARTH_RADIUS * EARTH_RADIUS;

    public static final double R_360 = 360.0;

    public static final double NINTY = 90.0;

    public static final double ZERO = 0.0;

    public static final double TWO = 2.0;

    public static final double AZ_RND = 1.05;

    public static final int SHORT_BYTES = 2;

    public static final int DEFAULT_BUILD_VERSION = MpeRadarGatherConstants.DHR_DEFAULT_VERSION;

    private MpeRadarDecodeConstants() {
    }

}
