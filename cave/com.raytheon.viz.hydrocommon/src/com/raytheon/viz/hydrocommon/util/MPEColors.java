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

import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2008            randerso     Initial creation
 * Aug 08, 2009 2675       mpduff       Added Radar Coverage.
 * Apr 04, 2012 8672       lbousaidi    changed color scale for 24h 
 * 										gridded precip.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MPEColors {
    static final String height_colors[] = { "GRAY30", "BLACK", "ORANGE",
            "YELLOW", "GREENYELLOW", "YELLOWGREEN", "GREEN", "TURQUOISE4",
            "TURQUOISE3", "TURQUOISE2", "DEEPSKYBLUE1", "DEEPSKYBLUE2",
            "DODGERBLUE1", "BLUE", "PURPLE2", "PURPLE3", "PURPLE4", "WHITE" };

    static final double height_levels[] = { -9999.0, -8888.0, 0.0, 250., 500.,
            750., 1000., 1500., 2000., 2500., 3000., 3500., 4000., 5000.,
            6000., 7000., 8000., 10000. };

    static final String index_colors[] = { "GRAY30", "BLACK", "YELLOW",
            "GREENYELLOW", "YELLOWGREEN", "GREEN", "TURQUOISE4", "TURQUOISE3",
            "TURQUOISE2", "DEEPSKYBLUE1", "DEEPSKYBLUE2", "DODGERBLUE1",
            "BLUE", "PURPLE2", "PURPLE3", "PURPLE4", "WHITE", "ORANGE" };

    static final double index_levels[] = { -9999.0, -8888.0, 1.0, 2.0, 3.0,
            4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0,
            16.0 };

    static final String locbias_colors[] = { "GRAY30", "GRAY20", "RED",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4" };

    static final double locbias_levels[] = { -9999.0, 0.0, 0.05, 0.4, 0.6, 0.8,
            1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.5, 3.0 };

    static final String locspan_colors[] = { "GRAY30", "GRAY20", "RED",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "WHITE" };

    static final double locspan_levels[] = { -9999.0, 0.0, 0.05, 1.0, 2.0, 3.0,
            4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };

    static final String precip_colors[] = { "GRAY30", "GRAY20", "GRAY10",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4",
            "MAGENTA1", "DARKORCHID", "WHITE" };

    static final double precip_levels[] = { -9999.0, -8888.0, 0.00, 0.01, 0.10,
            0.20, 0.30, 0.40, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00, 2.50,
            3.00 };

    static final String prism_colors[] = { "GRAY30", "GRAY20", "GRAY10",
            "DODGERBLUE1", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4",
            "MAGENTA1", "DARKORCHID", "WHITE" };

    static final double prism_levels[] = { -9999.0, 0.0, 0.05, 0.1, 0.2, 0.3,
            0.4, 0.5, 0.75, 1.0, 1.25, 1.50, 2.0, 2.5, 3.0, 4.0, 5.0 };

    static final String max_temp_prism_colors[] = { "GRAY30", "GRAY20",
            "PURPLE", "BLUE", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4" };

    static final double max_temp_prism_levels[] = { -9999.0, 0.0, 0.05, 10.0,
            20.0, 30.0, 40.0, 50.0, 60.0, 65.0, 70.0, 75.0, 80.0, 90.0 };

    static final String min_temp_prism_colors[] = { "GRAY30", "GRAY20",
            "PURPLE", "BLUE", "CYAN", "DARKGREEN", "GREEN", "GREENYELLOW",
            "YELLOW", "GOLD2", "DARKORANGE1", "RED", "RED3", "RED4" };

    static final double min_temp_prism_levels[] = { -9999.0, 0.0, 0.05, 10.0,
            20.0, 30.0, 40.0, 50.0, 60.0, 65.0, 70.0, 75.0, 80.0, 90.0 };

    static final String radclim_colors[] = { "GRAY30", "BLACK", "RED" };

    static final double radclim_levels[] = { -9999.0, 0.0, 1.0 };

    static final String radCov_colors[] = { "GRAY30", "BLACK", "RED" };

    static final double radCov_levels[] = { -9999.0, 0.0, 1.0 };

    /* Define the color sets for the DailyQC data. */

    /* 6h gridded precip */

    static final String gridded_precip_colors_6hr[] = { "GRAY69", "GRAY10",
            "GRAY30", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double gridded_precip_levels_6hr[] = { -9999.0, 0.0, 0.01,
            0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.3, 1.6, 2.0, 2.3, 2.5, 2.8, 3.0,
            3.5, 4.0 };

    /*
     * { 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3,
     * 1.4, 1.5 };
     */

    /* 24h gridded precip */
    static final String gridded_precip_colors_24hr [] = { "GRAY10", "GRAY69", "GRAY43",
            "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", "GREEN4", "MAGENTA1", "MAGENTA3",
    	    "BLUE1", "BLUE3", "GOLD3", "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double gridded_precip_levels_24hr [] = {0.0, 0.01, 0.1, 0.2,
    	   0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0};

    /*
     * { 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3,
     * 1.4, 1.5 };
     */

    /* 6hr mean precip */

    static final String mean_precip_colors_6hr[] = { "GRAY69", "GRAY10",
            "GRAY30", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double mean_precip_levels_6hr[] = { -9999.0, 0.0, 0.01, 0.1,
            0.2, 0.3, 0.5, 0.7, 1.0, 1.3, 1.6, 2.0, 2.3, 2.5, 2.8, 3.0, 3.5,
            4.0 };

    /* 24h mean precip */

    static final String mean_precip_colors_24hr[] = { "GRAY69", "GRAY10",
            "GRAY30", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double mean_precip_levels_24hr[] = { -9999.0, 0.0, 0.01, 0.1,
            0.2, 0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5,
            6.0 };

    /* 6hr gridded freezing level */

    static final String gridded_freezing_colors_6hr[] = { "GRAY69", "GRAY10",
            "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", "GREEN4",
            "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3", "YELLOW1",
            "ORANGE1", "RED1", "RED3", "WHITE" };

    static final double gridded_freezing_levels_6hr[] = { -99.0, 0.0, 1.0, 2.0,
            3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,
            15.0 };

    /* 6hr mean freezing level */

    static final String mean_freezing_colors_6hr[] = { "GRAY69", "GRAY10",
            "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", "GREEN4",
            "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3", "YELLOW1",
            "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double mean_freezing_levels_6hr[] = { -99.0, 0.0, 1.0, 2.0,
            3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,
            15.0 };

    /* 6hr gridded temperature */

    static final String gridded_temperature_colors_6hr[] = { "GRAY74",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    /*
     * { "GRAY74", "GRAY43", "MAGENTA4", "MAGENTA3", "MAGENTA2", "BLUEVIOLET",
     * "DARKVIOLET", "BLUE4", "BLUE3", "BLUE2", "CYAN3", "LIGHTSKYBLUE",
     * "GREEN4", "LIMEGREEN", "GREEN2", "GREENYELLOW", "YELLOW", "YELLOW2",
     * "GOLD", "GOLD2", "ORANGE", "ORANGE2", "ORANGE3", "RED3", "RED2",
     * "BROWN1", "PINK2", "LIGHTPINK", "WHITE" };
     */

    /*
     * { "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", "GREEN4",
     * "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3", "YELLOW1", "ORANGE1",
     * "RED1", "RED3", "GRAY100" };
     */

    static final double gridded_temperature_levels_6hr[] = { -9999.0, -30.0,
            -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0,
            90.0, 100.0, 110.0, 120.0 };

    /*
     * { -15.0, -10.0, -5.0, 0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0,
     * 45.0, 50.0, 55.0, 60.0 };
     */

    /*
     * { -20.0, -15.0, -10.0, -5.0, 0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0,
     * 35.0, 40.0, 45.0, 50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0,
     * 95.0, 100.0, 105.0, 110.0, 115.0, 120.0 };
     */

    /* 24h max, min temperature */

    static final String gridded_temperature_colors_max[] = { "GRAY74",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final String gridded_temperature_colors_min[] = { "GRAY74",
            "GRAY10", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
            "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
            "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double gridded_temperature_levels_max[] = { -9999.0, -30.0,
            -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0,
            90.0, 100.0, 110.0, 120.0 };

    static final double gridded_temperature_levels_min[] = { -9999.0, -30.0,
            -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0,
            90.0, 100.0, 110.0, 120.0 };

    /* 6hr mean temperature */

    static final String mean_temperature_colors_6hr[] = { "GRAY10", "GRAY74",
            "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", "GREEN4",
            "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3", "YELLOW1",
            "ORANGE1", "RED1", "RED3", "GRAY100" };

    static final double mean_temperature_levels_6hr[] = { -9999.0, -30.0,
            -20.0, -10.0, 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0,
            90.0, 100.0, 110.0, 120.0 };

    /**
     * Returns the list of default color sets for MPE
     * 
     * @return
     */
    public static List<NamedColorUseSet> build_mpe_colors() {
        NamedColorUseSet pColorUseSet = null;
        List<NamedColorUseSet> pColorSetGroup = new ArrayList<NamedColorUseSet>();

        /* Create a color use group for each of the MPE products. */

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
        pColorUseSet = new NamedColorUseSet("P3LMOSAIC",
                "P3 Local Bias Mosaic", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

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
                "RFC Field Bias Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* RFC Mosaic */
        pColorUseSet = new NamedColorUseSet("RFCMMOSAIC",
                "RFC Multisensor Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Gage Mosaic */
        pColorUseSet = new NamedColorUseSet("SGMOSAIC",
                "Satellite Gage Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Radar Mosaic */
        pColorUseSet = new NamedColorUseSet("SRMOSAIC",
                "Satellite Radar Mosaic", precip_levels, precip_colors,
                "GRAY30", "GRAY10", 3600);

        pColorSetGroup.add(pColorUseSet);

        /* Satellite Radar Gage Mosaic */
        pColorUseSet = new NamedColorUseSet("SRGMOSAIC",
                "Satellite Radar Gage", precip_levels, precip_colors, "GRAY30",
                "GRAY10", 3600);

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
                gridded_precip_colors_6hr, "GRAY30", "GRAY10", 21600);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded 24hr precipitation colormap */

        pColorUseSet = new NamedColorUseSet("24hGRID_PRECIP",
                "24hr Gridded Precip", gridded_precip_levels_24hr,
                gridded_precip_colors_24hr, "GRAY30", "GRAY10", 86400);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded 6hr temperature colormap */

        pColorUseSet = new NamedColorUseSet("sixhGRID_TEMP",
                "6hr Gridded Temperature", gridded_temperature_levels_6hr,
                gridded_temperature_colors_6hr, "GRAY30", "GRAY10", 21600);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded max/min daily temperature colormaps */

        pColorUseSet = new NamedColorUseSet("maxGRID_TEMP",
                "Max Gridded Temperature", gridded_temperature_levels_max,
                gridded_temperature_colors_max, "GRAY30", "GRAY10", 86400);

        pColorSetGroup.add(pColorUseSet);

        pColorUseSet = new NamedColorUseSet("minGRID_TEMP",
                "Min Gridded Temperature", gridded_temperature_levels_min,
                gridded_temperature_colors_min, "GRAY30", "GRAY10", 86400);

        pColorSetGroup.add(pColorUseSet);

        /* Gridded 6hr freezing level colormap */

        pColorUseSet = new NamedColorUseSet("6hGRID_FREEZL",
                "6hr Gridded Freezing Level", gridded_freezing_levels_6hr,
                gridded_freezing_colors_6hr, "GRAY30", "GRAY10", 21600);

        pColorSetGroup.add(pColorUseSet);

        /* 6hr MAP precipitation colormap */

        pColorUseSet = new NamedColorUseSet("6hMAREA_PRECIP",
                "6hr Mean Area Precip", mean_precip_levels_6hr,
                mean_precip_colors_6hr, "GRAY30", "GRAY10", 21600);

        pColorSetGroup.add(pColorUseSet);

        /* 24hr MAP precipitation colormap */

        pColorUseSet = new NamedColorUseSet("24hMAREA_PRECIP",
                "24hr Mean Area Precip", mean_precip_levels_24hr,
                mean_precip_colors_24hr, "GRAY30", "GRAY10", 86400);

        pColorSetGroup.add(pColorUseSet);

        /* 6hr MAT temperature colormap */

        pColorUseSet = new NamedColorUseSet("sixhMAREA_TEMP",
                "6hr Mean Area Temperature", mean_temperature_levels_6hr,
                mean_temperature_colors_6hr, "GRAY30", "GRAY10", 21600);

        pColorSetGroup.add(pColorUseSet);

        /* 6hr MAZ freezing level colormap */

        pColorUseSet = new NamedColorUseSet("6hMAREA_FREEZL",
                "6hr Mean Area Freezing Level", mean_freezing_levels_6hr,
                mean_freezing_colors_6hr, "GRAY30", "GRAY10", 21600);

        pColorSetGroup.add(pColorUseSet);

        return pColorSetGroup;
    }
}
