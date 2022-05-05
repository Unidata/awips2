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
package com.raytheon.viz.hydrocommon.util;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.mpe.constants.AppsDefaultsContants;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * Defines the color combinations used in the MPE legends.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2008            randerso     Initial creation
 * Aug 08, 2009 2675       mpduff       Added Radar Coverage.
 * Apr 04, 2012 8672       lbousaidi    changed color scale for 24h 
 *                                      gridded precip.
 * Oct 06, 2017 6407       bkowal       Cleanup. Updates to support GOES-R SATPRE.
 * </pre>
 * 
 * @author randerso
 */

public class MPEColors {
    private static final String height_colors[] = { "GRAY30", "BLACK", "ORANGE",
            "YELLOW", "GREENYELLOW", "YELLOWGREEN", "GREEN", "TURQUOISE4",
            "TURQUOISE3", "TURQUOISE2", "DEEPSKYBLUE1", "DEEPSKYBLUE2",
            "DODGERBLUE1", "BLUE", "PURPLE2", "PURPLE3", "PURPLE4", "WHITE" };

    private static final double height_levels[] = { -9999.0, -8888.0, 0.0, 250.,
            500., 750., 1000., 1500., 2000., 2500., 3000., 3500., 4000., 5000.,
            6000., 7000., 8000., 10000. };

    private static final String index_colors[] = { "GRAY30", "BLACK", "YELLOW",
            "GREENYELLOW", "YELLOWGREEN", "GREEN", "TURQUOISE4", "TURQUOISE3",
            "TURQUOISE2", "DEEPSKYBLUE1", "DEEPSKYBLUE2", "DODGERBLUE1", "BLUE",
            "PURPLE2", "PURPLE3", "PURPLE4", "WHITE", "ORANGE" };

    private static final double index_levels[] = { -9999.0, -8888.0, 1.0, 2.0,
            3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,
            15.0, 16.0 };

    private static final String locbias_colors[] = { "GRAY30", "GRAY20", "RED",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4" };

    private static final double locbias_levels[] = { -9999.0, 0.0, 0.05, 0.4,
            0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.5, 3.0 };

    private static final String locspan_colors[] = { "GRAY30", "GRAY20", "RED",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "WHITE" };

    private static final double locspan_levels[] = { -9999.0, 0.0, 0.05, 1.0,
            2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };

    private static final String precip_colors[] = { "GRAY30", "GRAY20",
            "GRAY10", "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN",
            "GREENYELLOW", "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3",
            "RED4", "MAGENTA1", "DARKORCHID", "WHITE" };

    private static final double precip_levels[] = { -9999.0, -8888.0, 0.00,
            0.01, 0.10, 0.20, 0.30, 0.40, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75,
            2.00, 2.50, 3.00 };

    private static final String precip_bias_colors[] = { "DarkGray", "Gray",
            "Light Gray", "DarkBlue", "Blue", "DarkCyan", "Cyan", "DarkGreen",
            "Green", "Yellow", "Orange", "Red", "Dark Red", "Magenta",
            "White" };

    private static final double precip_bias_levels[] = { 0.0, 0.05, 0.1, .2, .4,
            0.8, 1.2, 2.0, 3.0, 4.0, 5.0, 8.0, 16.0, 32.0, 100.0 };

    private static final String precip_diff_colors[] = { "GRAY30", "GRAY30",

            "MAGENTA1", "MEDIUMORCHID", "DARKORCHID", "MEDIUMBLUE", "BLUE",
            "DODGERBLUE", "DARKTURQUOISE",

            "CYAN",

            "BLACK", "BLACK", "GREEN", "GREENYELLOW", "YELLOW",

            "DARKORANGE1", "ORANGERED", "RED2", "RED3",

    };

    private static final double precip_diff_levels[] =

            { -9999.0, -8888.0,

                    -10.0, -0.75, -0.5, -.25, -0.1, -0.08, -0.06, -0.03,

                    -0.01, // CYAN

                    0.0, 0.01,

                    0.03,

                    0.06, 0.08, 0.1, 0.25, 0.50

            };

    private static final String precip_ratio_colors[] = { "GRAY30", "GRAY30",

            "MAGENTA1", "MEDIUMORCHID", "DARKORCHID", "BLUE", "DODGERBLUE",
            "DARKTURQUOISE",

            "CYAN", "BLACK", "GREEN", "GREENYELLOW", "YELLOW",

            "DARKORANGE1", "ORANGERED", "RED2", "RED3", };

    private static final double precip_ratio_levels[] =

            {

                    -9999.0, -8888.0, 0.0, 0.1, 0.2, 0.3,

                    0.5, 0.6, 0.75,

                    0.95, 1.05, // between this number shows up as black
                    1.5, 2.0, 4.0,

                    6.0, 8.0, 16.0

            };

    private static final String prism_colors[] = { "GRAY30", "GRAY20", "GRAY10",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4", "MAGENTA1",
            "DARKORCHID", "WHITE" };

    private static final double prism_levels[] = { -9999.0, 0.0, 0.05, 0.1, 0.2,
            0.3, 0.4, 0.5, 0.75, 1.0, 1.25, 1.50, 2.0, 2.5, 3.0, 4.0, 5.0 };

    private static final String max_temp_prism_colors[] = { "GRAY30", "GRAY20",
            "PURPLE", "BLUE", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4" };

    private static final double max_temp_prism_levels[] = { -9999.0, 0.0, 0.05,
            10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 65.0, 70.0, 75.0, 80.0, 90.0 };

    private static final String min_temp_prism_colors[] = { "GRAY30", "GRAY20",
            "PURPLE", "BLUE", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4" };

    private static final double min_temp_prism_levels[] = { -9999.0, 0.0, 0.05,
            10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 65.0, 70.0, 75.0, 80.0, 90.0 };

    private static final String radclim_colors[] = { "GRAY30", "BLACK", "RED" };

    private static final double radclim_levels[] = { -9999.0, 0.0, 1.0 };

    private static final String radCov_colors[] = { "GRAY30", "BLACK", "RED" };

    private static final double radCov_levels[] = { -9999.0, 0.0, 1.0 };

    /* Define the color sets for the DailyQC data. */

    /* 6h gridded precip */

    private static final String gridded_precip_colors_6hr[] = { "GRAY69",
            "GRAY10", "GRAY30", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN",
            "GREEN1", "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3",
            "GOLD3", "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double gridded_precip_levels_6hr[] = { -9999.0, 0.0,
            0.01, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.3, 1.6, 2.0, 2.3, 2.5, 2.8,
            3.0, 3.5, 4.0 };

    /* 24h gridded precip */
    private static final String gridded_precip_colors_24hr[] = { "GRAY10",
            "GRAY69", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double gridded_precip_levels_24hr[] = { 0.0, 0.01, 0.1,
            0.2, 0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5,
            6.0 };

    /* 6hr mean precip */

    private static final String mean_precip_colors_6hr[] = { "GRAY69", "GRAY10",
            "GRAY30", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double mean_precip_levels_6hr[] = { -9999.0, 0.0, 0.01,
            0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.3, 1.6, 2.0, 2.3, 2.5, 2.8, 3.0,
            3.5, 4.0 };

    /* 24h mean precip */

    private static final String mean_precip_colors_24hr[] = { "GRAY69",
            "GRAY10", "GRAY30", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN",
            "GREEN1", "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3",
            "GOLD3", "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double mean_precip_levels_24hr[] = { -9999.0, 0.0,
            0.01, 0.1, 0.2, 0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5,
            5.0, 5.5, 6.0 };

    /* 6hr gridded freezing level */

    private static final String gridded_freezing_colors_6hr[] = { "GRAY69",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "WHITE" };

    private static final double gridded_freezing_levels_6hr[] = { -99.0, 0.0,
            1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0,
            14.0, 15.0 };

    /* 6hr mean freezing level */

    private static final String mean_freezing_colors_6hr[] = { "GRAY69",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double mean_freezing_levels_6hr[] = { -99.0, 0.0, 1.0,
            2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0,
            14.0, 15.0 };

    /* 6hr gridded temperature */

    private static final String gridded_temperature_colors_6hr[] = { "GRAY74",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double gridded_temperature_levels_6hr[] = { -9999.0,
            -30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
            80.0, 90.0, 100.0, 110.0, 120.0 };

    /* 24h max, min temperature */

    private static final String gridded_temperature_colors_max[] = { "GRAY74",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final String gridded_temperature_colors_min[] = { "GRAY74",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double gridded_temperature_levels_max[] = { -9999.0,
            -30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
            80.0, 90.0, 100.0, 110.0, 120.0 };

    private static final double gridded_temperature_levels_min[] = { -9999.0,
            -30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
            80.0, 90.0, 100.0, 110.0, 120.0 };

    /* 6hr mean temperature */

    private static final String mean_temperature_colors_6hr[] = { "GRAY10",
            "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    private static final double mean_temperature_levels_6hr[] = { -9999.0,
            -30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
            80.0, 90.0, 100.0, 110.0, 120.0 };

    /**
     * Returns the list of default color sets for MPE
     * 
     * @return
     */
    public static List<NamedColorUseSet> build_mpe_colors() {
        NamedColorUseSet pColorUseSet = null;
        List<NamedColorUseSet> pColorSetGroup = new ArrayList<>();

        /* Create a color use group for each of the MPE products. */

        /* PRECIP_BIAS */
        pColorUseSet = new NamedColorUseSet("PRECIP_BIAS", "Precip Bias",
                precip_bias_levels, precip_bias_colors, "GRAY30", "GRAY10",
                3600);
        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("PRECIP_DIFF", "Precip Difference",
                precip_diff_levels, precip_diff_colors, "GRAY30", "GRAY30",
                3600);
        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("PRECIP_RATIO", "Precip Ratio",
                precip_ratio_levels, precip_ratio_colors, "GRAY30", "GRAY30",
                3600);
        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("PRECIP_ACCUM",
                "Precip Accumulated", precip_levels, precip_colors, "GRAY30",
                "GRAY30", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("RMOSAIC", "Radar Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY30", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* DP Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("RDMOSAIC", "DP Radar Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY30", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("AVGRDMOSAIC",
                "DP Avg Radar Mosaic", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("MAXRDMOSAIC",
                "DP Max Radar Mosaic", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("BDMOSAIC",
                "DP Field Bias Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("LDMOSAIC",
                "DP Local Bias Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("MDMOSAIC",
                "DP Field Bias Multisensor Radar Mosaic", precip_levels,
                precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("MLDMOSAIC",
                "DP Local Bias Multisensor Radar Mosaic", precip_levels,
                precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("SRDMOSAIC",
                "DP Satellite Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("SRDGMOSAIC",
                "DP Satellite Radar Gage Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("RMOSAIC", "Radar Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Average Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("AVGRMOSAIC",
                "Average Radar Mosaic", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Max Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("MAXRMOSAIC",
                "Maximum Radar Mosaic", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Field Bias Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("BMOSAIC",
                "Field Bias Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Local Bias Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("LMOSAIC",
                "Local Bias Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Gage Only Analysis */
        pColorUseSet = new NamedColorUseSet("GAGEONLY", "Gage Only Analysis",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Precip */
        pColorUseSet = new NamedColorUseSet("SATPRE", "Satellite Precip",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        if (Boolean.TRUE
                .equals(AppsDefaultsConversionWrapper.getPropertyAsBoolean(
                        AppsDefaultsContants.APPS_DEFAULTS_USE_GOESR_PRECIP))) {
            /* Satellite Precip [GOES-R] */
            pColorUseSet = new NamedColorUseSet("GOESRSATPRE",
                    "Satellite Precip [GOES-R]", precip_levels, precip_colors,
                    "GRAY30", "GRAY10", 3600);

            pColorSetGroup.add(pColorUseSet);
        }

        /* Local Bias Satellite Precip */
        pColorUseSet = new NamedColorUseSet("LSATPRE",
                "Local Bias Satellite Precip", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Multi-sensor Mosaic or Multi-sensor Precip */
        pColorUseSet = new NamedColorUseSet("MMOSAIC", "Multisensor Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Local Bias Multi-sesnor Mosaic */
        pColorUseSet = new NamedColorUseSet("MLMOSAIC",
                "Local Bias Multi-sensor Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* P3 Local Bias Mosaic */
        pColorUseSet = new NamedColorUseSet("P3LMOSAIC", "P3 Local Bias Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Best Estimate QPE */
        pColorUseSet = new NamedColorUseSet("XMRG", "Best Estimate",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Local Field #1 */
        pColorUseSet = new NamedColorUseSet("LOCALFIELD1", "Local Field #1",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Local Field #2 */
        pColorUseSet = new NamedColorUseSet("LOCALFIELD2", "Local Field #3",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Local Field #3 */
        pColorUseSet = new NamedColorUseSet("LOCALFIELD3", "Local Field #3",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Multi-hour QPE */
        pColorUseSet = new NamedColorUseSet("MULTIHOUR", "Multi-Hour QPE",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Q2 Mosaic */
        pColorUseSet = new NamedColorUseSet("QMOSAIC", "Q2 Radar Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Q2 Local Bias Mosaic */
        pColorUseSet = new NamedColorUseSet("LQMOSAIC", "Q2 Local-Bias Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Q2 Multisensor Mosaic */
        pColorUseSet = new NamedColorUseSet("MLQMOSAIC",
                "Q2 Multi-Sensor Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Local Span Field */
        pColorUseSet = new NamedColorUseSet("LOCSPAN", "Local Span",
                locspan_levels, locspan_colors, "GRAY30", "BLACK", 0);

        pColorSetGroup.add(pColorUseSet);

        /* Local Bias Field */
        pColorUseSet = new NamedColorUseSet("LOCBIAS", "Local Bias",
                locbias_levels, locbias_colors, "GRAY30", "BLACK", 0);

        pColorSetGroup.add(pColorUseSet);

        /* Radar Height Field */
        pColorUseSet = new NamedColorUseSet("HEIGHT", "Height", height_levels,
                height_colors, "GRAY30", "BLACK", 0);

        pColorSetGroup.add(pColorUseSet);

        /* Radar Coverage Field */
        pColorUseSet = new NamedColorUseSet("INDEX", "Radar Coverage",
                index_levels, index_colors, "GRAY30", "BLACK", 0);

        pColorSetGroup.add(pColorUseSet);

        /* PRISM Field */
        pColorUseSet = new NamedColorUseSet("PRISM", "Prism", prism_levels,
                prism_colors, "GRAY30", "GRAY10", 0);

        pColorSetGroup.add(pColorUseSet);

        /* Max Temp PRISM Field */
        pColorUseSet = new NamedColorUseSet("MAXTEMPPRISM", "Max Temp PRISM",
                max_temp_prism_levels, max_temp_prism_colors, "GRAY30",
                "GRAY10", 0);

        pColorSetGroup.add(pColorUseSet);

        /* Min Temp PRISM Field */
        pColorUseSet = new NamedColorUseSet("MINTEMPPRISM", "Min Temp PRISM",
                min_temp_prism_levels, min_temp_prism_colors, "GRAY30",
                "GRAY10", 0);

        pColorSetGroup.add(pColorUseSet);

        /* RFC Mosaic */
        pColorUseSet = new NamedColorUseSet("RFCMOSAIC", "RFC Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* RFC Field Bias Mosaic */
        pColorUseSet = new NamedColorUseSet("RFCBMOSAIC",
                "RFC Field Bias Mosaic", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* RFC Mosaic */
        pColorUseSet = new NamedColorUseSet("RFCMMOSAIC",
                "RFC Multisensor Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Gage Mosaic */
        pColorUseSet = new NamedColorUseSet("SGMOSAIC", "Satellite Gage Mosaic",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("SRMOSAIC",
                "Satellite Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Radar Gage Mosaic */
        pColorUseSet = new NamedColorUseSet("SRGMOSAIC", "Satellite Radar Gage",
                precip_levels, precip_colors, "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Radclim */
        pColorUseSet = new NamedColorUseSet("RADCLIM", "Radclim",
                radclim_levels, radclim_colors, "GRAY30", "BLACK", 0);

        pColorSetGroup.add(pColorUseSet);

        /* Radcov */
        pColorUseSet = new NamedColorUseSet("RADCOV", "Radcov", radCov_levels,
                radCov_colors, "GRAY30", "BLACK", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Add color sets for the DailyQC portion of MPE Editor. */

        /* Gridded 6hr precipitation colormap */

        pColorUseSet = new NamedColorUseSet("6hGRID_PRECIP",
                "6hr Gridded Precip", gridded_precip_levels_6hr,
                gridded_precip_colors_6hr, "GRAY30", "GRAY10", 21_600);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded 24hr precipitation colormap */

        pColorUseSet = new NamedColorUseSet("24hGRID_PRECIP",
                "24hr Gridded Precip", gridded_precip_levels_24hr,
                gridded_precip_colors_24hr, "GRAY30", "GRAY10", 86_400);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded 6hr temperature colormap */

        pColorUseSet = new NamedColorUseSet("sixhGRID_TEMP",
                "6hr Gridded Temperature", gridded_temperature_levels_6hr,
                gridded_temperature_colors_6hr, "GRAY30", "GRAY10", 21_600);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded max/min daily temperature colormaps */

        pColorUseSet = new NamedColorUseSet("maxGRID_TEMP",
                "Max Gridded Temperature", gridded_temperature_levels_max,
                gridded_temperature_colors_max, "GRAY30", "GRAY10", 86_400);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("minGRID_TEMP",
                "Min Gridded Temperature", gridded_temperature_levels_min,
                gridded_temperature_colors_min, "GRAY30", "GRAY10", 86_400);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded 6hr freezing level colormap */

        pColorUseSet = new NamedColorUseSet("6hGRID_FREEZL",
                "6hr Gridded Freezing Level", gridded_freezing_levels_6hr,
                gridded_freezing_colors_6hr, "GRAY30", "GRAY10", 21_600);

        pColorSetGroup.add(pColorUseSet);

        /* 6hr MAP precipitation colormap */

        pColorUseSet = new NamedColorUseSet("6hMAREA_PRECIP",
                "6hr Mean Area Precip", mean_precip_levels_6hr,
                mean_precip_colors_6hr, "GRAY30", "GRAY10", 21_600);

        pColorSetGroup.add(pColorUseSet);

        /* 24hr MAP precipitation colormap */

        pColorUseSet = new NamedColorUseSet("24hMAREA_PRECIP",
                "24hr Mean Area Precip", mean_precip_levels_24hr,
                mean_precip_colors_24hr, "GRAY30", "GRAY10", 86_400);

        pColorSetGroup.add(pColorUseSet);

        /* 6hr MAT temperature colormap */

        pColorUseSet = new NamedColorUseSet("sixhMAREA_TEMP",
                "6hr Mean Area Temperature", mean_temperature_levels_6hr,
                mean_temperature_colors_6hr, "GRAY30", "GRAY10", 21_600);

        pColorSetGroup.add(pColorUseSet);

        /* 6hr MAZ freezing level colormap */

        pColorUseSet = new NamedColorUseSet("6hMAREA_FREEZL",
                "6hr Mean Area Freezing Level", mean_freezing_levels_6hr,
                mean_freezing_colors_6hr, "GRAY30", "GRAY10", 21_600);

        pColorSetGroup.add(pColorUseSet);

        return pColorSetGroup;
    }
}