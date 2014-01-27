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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;

import com.raytheon.uf.common.dataplugin.radar.level3.TabularBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Parses the tabular block of the radar products
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 24, 2009           mnash     Initial creation
 * Oct 10, 2013  2376     bsteffen  Improve STI parsing.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarTabularBlockParser {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarTabularBlockParser.class);

    public static void parseTabularBlock(TabularBlock tabularBlock,
            int productNum,
            HashMap<MapValues, Map<String, Map<MapValues, String>>> bigMap,
            HashMap<MapValues, Map<MapValues, String>> recordMap) {
        HashMap<String, Map<MapValues, String>> data = new HashMap<String, Map<MapValues, String>>();
        Map<MapValues, String> map = new HashMap<MapValues, String>();
        /*
         * Hail tabular block, key on storm id
         */
        if (productNum == 59) {
            String[] eachPage = new String[tabularBlock.getPages().size()];
            eachPage = tabularBlock.toString().split("Page \\d");
            for (int i = 1; i < eachPage.length - 1; i++) {
                String[] temp = RadarConstants.hail_headerPattern
                        .split(eachPage[i].trim());
                temp[1].subSequence(1, temp[1].length());
                temp[1] = temp[1].replaceFirst("\\)\\s+\n\t\\s+\n\t\\s+", " ");
                Matcher m = RadarConstants.hail_productValues.matcher(temp[1]);
                while (m.find()) {
                    map.put(MapValues.HI_POSH, m.group(2).trim());
                    map.put(MapValues.HI_POH, m.group(3).trim());
                    map.put(MapValues.HI_MAX_HAIL_SIZE, m.group(4).trim());
                    data.put(m.group(1).trim(), map);
                    map = new HashMap<MapValues, String>();
                }
            }
            bigMap.put(MapValues.HAIL_TYPE, data);
        }

        /*
         * STI tabular block, key on storm id
         */
        else if (productNum == 58) {
            String sti_spd = "";
            String sti_dir = "";
            String[] eachPage = new String[tabularBlock.getPages().size()];
            eachPage = tabularBlock.toString().split("Page \\d");
            for (int i = 1; i < eachPage.length - 1; i++) {
                String[] temp = RadarConstants.sti_headerPattern
                        .split(eachPage[i].trim());
                Matcher m = RadarConstants.sti_headerValues.matcher(temp[0]
                        .trim());
                while (m.find()) {
                    sti_spd = m.group(1);
                    sti_dir = m.group(2);
                }
                temp = RadarConstants.sti_headerPattern.split(eachPage[i]
                        .trim());
                temp[6].subSequence(1, temp[6].length());
                temp[6] = temp[6].replaceFirst("\\)\\s+\n\t\\s+\n\t\\s+", " ");
                m = RadarConstants.sti_productValues.matcher(temp[6].trim());
                while (m.find()) {
                    parseStiToken(m.group(2), map,
                            MapValues.STI_AZIMUTH_DIRECTION,
                            MapValues.STI_AZIMUTH_RANGE);
                    parseStiToken(m.group(3), map,
                            MapValues.STI_MOVEMENT_DIRECTION,
                            MapValues.STI_MOVEMENT_SPEED);
                    parseStiToken(m.group(4), map,
                            MapValues.STI_FORECAST_15_DIRECTION,
                            MapValues.STI_FORECAST_15_RANGE);
                    parseStiToken(m.group(5), map,
                            MapValues.STI_FORECAST_30_DIRECTION,
                            MapValues.STI_FORECAST_30_RANGE);
                    parseStiToken(m.group(6), map,
                            MapValues.STI_FORECAST_45_DIRECTION,
                            MapValues.STI_FORECAST_45_RANGE);
                    parseStiToken(m.group(7), map,
                            MapValues.STI_FORECAST_60_DIRECTION,
                            MapValues.STI_FORECAST_60_RANGE);
                    parseStiToken(m.group(8), map, MapValues.STI_ERROR_FCST,
                            MapValues.STI_ERROR_MEAN);
                    data.put(m.group(1).trim(), map);
                    map = new HashMap<MapValues, String>();
                }
            }

            String[] temp = eachPage[eachPage.length - 1].trim().split("DATA ")[1]
                    .split("\\s+");
            map.put(MapValues.STI_DEF_DIR, temp[1]);
            map.put(MapValues.STI_THRESH_MIN_SPD, temp[5]);
            map.put(MapValues.STI_DEF_SPD, temp[10]);
            map.put(MapValues.STI_ALLOW_ERROR, temp[14]);
            map.put(MapValues.STI_MAX_TIME, temp[18]);
            map.put(MapValues.STI_FORECAST_INT, temp[22]);
            map.put(MapValues.STI_NUM_PAST_VOLS, temp[26]);
            map.put(MapValues.STI_NUM_INT, temp[31]);
            map.put(MapValues.STI_CORR_SPD, temp[35]);
            map.put(MapValues.STI_ERROR_INT, temp[39]);
            map.put(MapValues.STI_AVG_SPEED, sti_spd);
            map.put(MapValues.STI_AVG_DIRECTION, sti_dir);
            recordMap.put(MapValues.STI_TYPE, map);
            bigMap.put(MapValues.STI_TYPE, data);
        }

        /*
         * TVS tabular block
         */
        else if (productNum == 61) {
            String[] eachPage = new String[tabularBlock.getPages().size()];
            eachPage = tabularBlock.toString().split("Page \\d");
            for (int i = 1; i < eachPage.length - 1; i++) {
                String[] temp = RadarConstants.tvs_headerPattern
                        .split(eachPage[i].trim());
                temp[1].subSequence(1, temp[1].length());
                temp[1] = temp[1].replaceFirst("\\s+\n\t\\s+\n\t\\s+", " ");
                Matcher m = RadarConstants.tvs_productValues.matcher(temp[1]
                        .trim());
                Integer x = new Integer(1);
                while (m.find()) {
                    map.put(MapValues.TVS_FEATURE_TYPE, m.group(1).trim());
                    map.put(MapValues.STORM_ID, m.group(2).trim());
                    map.put(MapValues.TVS_AZIMUTH, m.group(3).trim());
                    map.put(MapValues.TVS_RANGE, m.group(4).trim());
                    map.put(MapValues.TVS_AVGDV, m.group(5).trim());
                    map.put(MapValues.TVS_LLDV, m.group(6).trim());
                    map.put(MapValues.TVS_MXDV, m.group(7).trim());
                    map.put(MapValues.TVS_DVHGT, m.group(8).trim());
                    map.put(MapValues.TVS_DEPTH, m.group(9).trim());
                    map.put(MapValues.TVS_BASE, m.group(10).trim());
                    map.put(MapValues.TVS_TOP, m.group(11).trim());
                    map.put(MapValues.TVS_MXSHR, m.group(12).trim());
                    map.put(MapValues.TVS_SHRHGT, m.group(13).trim());
                    data.put(x.toString(), map);
                    x++;
                    map = new HashMap<MapValues, String>();
                }
            }
            String[] temp = eachPage[eachPage.length - 1].trim().split("\\s+");
            map.put(MapValues.VAD_ANALYSIS_SLANT_RNG, temp[9]);
            map.put(MapValues.VAD_BEGIN_AZIMUTH_ANGLE, temp[15]);
            map.put(MapValues.VAD_END_AZIMUTH_ANGLE, temp[19]);
            map.put(MapValues.VAD_NUM_PASSES, temp[24]);
            map.put(MapValues.VAD_RMS_THRESHOLD, temp[27]);
            recordMap.put(MapValues.TVS_TYPE, map);
            bigMap.put(MapValues.TVS_TYPE, data);
        }

        /*
         * Meso tabular block, key on feature id
         */
        else if (productNum == 60) {
            String[] eachPage = new String[tabularBlock.getPages().size()];
            eachPage = tabularBlock.toString().split("Page \\d");
            for (int i = 1; i < eachPage.length - 1; i++) {
                String[] temp = RadarConstants.legacy_meso_headerPattern
                        .split(eachPage[i].trim());
                temp[1].subSequence(1, temp[1].length());
                temp[1] = temp[1].replaceFirst("\\)\\s+\n\t\\s+\n\t\\s+", " ");
                Matcher m = RadarConstants.legacy_meso_productValues
                        .matcher(temp[1].trim());
                while (m.find()) {
                    map.put(MapValues.MESO_STORM_ID, m.group(2).trim());
                    map.put(MapValues.MESO_FEATURE_TYPE, m.group(3).trim());
                    map.put(MapValues.MESO_BASE_KFT, m.group(4).trim());
                    map.put(MapValues.MESO_TOP_KFT, m.group(5).trim());
                    map.put(MapValues.MESO_AZIMUTH_DIRECTION, m.group(6).trim());
                    map.put(MapValues.MESO_AZIMUTH_RANGE, m.group(7).trim());
                    map.put(MapValues.MESO_HGT, m.group(8).trim());
                    map.put(MapValues.MESO_DIAM_RAD, m.group(9).trim());
                    map.put(MapValues.MESO_DIAM_AZ, m.group(10).trim());
                    map.put(MapValues.MESO_SHEAR, m.group(11).trim());
                    data.put(m.group(1).trim(), map);
                    map = new HashMap<MapValues, String>();
                }
            }
            bigMap.put(MapValues.LEG_MESO_TYPE, data);
        }

        /*
         * VAD tabular block, key on altitude
         */
        else if (productNum == 48) {
            String[] eachPage = new String[tabularBlock.getPages().size()];
            eachPage = tabularBlock.toString().split("Page \\d");
            for (int i = 1; i < eachPage.length - 2; i++) {
                String[] temp = RadarConstants.vad_headerPattern
                        .split(eachPage[i].trim());
                temp[1].subSequence(1, temp[1].length());
                temp[1] = temp[1].replaceFirst("\\s+\n\t\\s+", " ");
                Matcher m = RadarConstants.vad_productValues.matcher(temp[1]
                        .trim());
                while (m.find()) {
                    map.put(MapValues.VAD_U, m.group(2).trim());
                    map.put(MapValues.VAD_V, m.group(3).trim());
                    map.put(MapValues.VAD_W, m.group(4).trim());
                    map.put(MapValues.VAD_DIR, m.group(5).trim());
                    map.put(MapValues.VAD_SPD, m.group(6).trim());
                    map.put(MapValues.VAD_RMS, m.group(7).trim());
                    map.put(MapValues.VAD_DIV, m.group(8).trim());
                    map.put(MapValues.VAD_SRNG, m.group(9).trim());
                    map.put(MapValues.VAD_ELEV, m.group(10).trim());
                    data.put(m.group(1).trim(), map);
                    map = new HashMap<MapValues, String>();
                }
            }
            String[] temp = eachPage[eachPage.length - 1].trim().split("\\s+");
            map.put(MapValues.VAD_ANALYSIS_SLANT_RNG, temp[9]);
            map.put(MapValues.VAD_BEGIN_AZIMUTH_ANGLE, temp[15]);
            map.put(MapValues.VAD_END_AZIMUTH_ANGLE, temp[19]);
            map.put(MapValues.VAD_NUM_PASSES, temp[24]);
            map.put(MapValues.VAD_RMS_THRESHOLD, temp[27]);
            recordMap.put(MapValues.VAD_TYPE, map);
            bigMap.put(MapValues.VAD_TYPE, data);
        }

        /*
         * Meso tabular block, key on feature id
         */
        else if (productNum == 141) {
            String[] eachPage = new String[tabularBlock.getPages().size()];
            eachPage = tabularBlock.toString().split("Page \\d");
            for (int i = 1; i < eachPage.length; i++) {
                String[] temp = RadarConstants.meso_headerPattern
                        .split(eachPage[i].trim());
                temp[1].subSequence(1, temp[1].length());
                temp[1] = temp[1].replaceFirst("\\)\\s+\n\t\\s+\n\t", " ");
                Matcher m = RadarConstants.meso_productValues.matcher(temp[1]
                        .trim());
                while (m.find()) {
                    map.put(MapValues.MESO_CIRC_ID, m.group(1).trim());
                    map.put(MapValues.MESO_AZIMUTH_DIRECTION,
                            m.group(2).split("/")[0].trim());
                    map.put(MapValues.MESO_AZIMUTH_RANGE,
                            m.group(2).split("/")[1].trim());
                    map.put(MapValues.MESO_SR, m.group(3).trim());
                    map.put(MapValues.MESO_STORM_ID, m.group(4).trim());
                    map.put(MapValues.MESO_RV, m.group(5).trim());
                    map.put(MapValues.MESO_DV, m.group(6).trim());
                    map.put(MapValues.MESO_BASE, m.group(7).trim());
                    map.put(MapValues.MESO_DEPTH, m.group(8).trim());
                    map.put(MapValues.MESO_DEPTH_PERCENT, m.group(9).trim());
                    map.put(MapValues.MESO_MAX_RV, m.group(10).trim());
                    map.put(MapValues.MESO_MAX_RV_SPD, m.group(11).trim());
                    map.put(MapValues.MESO_TVS_TYPE, m.group(12).trim());
                    if (!"".equals(m.group(13).trim())) {
                        map.put(MapValues.MESO_MOTION_DIR,
                                m.group(13).split("/")[0].trim());
                        map.put(MapValues.MESO_MOTION_SPD,
                                m.group(13).split("/")[1].trim());
                    } else {
                        map.put(MapValues.MESO_MOTION_DIR, "");
                        map.put(MapValues.MESO_MOTION_SPD, "");
                    }
                    map.put(MapValues.MESO_MSI, m.group(14).trim());
                    data.put(m.group(1).trim(), map);
                    map = new HashMap<MapValues, String>();
                }
            }
            bigMap.put(MapValues.MESO_TYPE, data);
        }

        else if (productNum == 74) {
            String text = tabularBlock.getString();
            Matcher m = RadarConstants.rcm_productValues.matcher(text.trim());
            while (m.find()) {
                map.put(MapValues.RCM_PROD_CAT, m.group(1));
                map.put(MapValues.RCM_RDA_SITE, m.group(2));
                map.put(MapValues.RCM_TIME, m.group(4));
                map.put(MapValues.RCM_OPER_MODE, m.group(5));
                map.put(MapValues.RCM_SCAN_STRAT, m.group(6));
                map.put(MapValues.RCM_TOT_INTENS, m.group(7));
                map.put(MapValues.RCM_REFL_STRING, m.group(8).trim());
                map.put(MapValues.RCM_MT_HGT, m.group(9));
                map.put(MapValues.RCM_MT_LOC, m.group(10));
                map.put(MapValues.RCM_NCEN, m.group(11));
                map.put(MapValues.RCM_CENTROIDS, m.group(12).trim());
                map.put(MapValues.VAD_WINDS, m.group(15).trim());
                map.put(MapValues.RCM_NUM_TVS, m.group(18));
                map.put(MapValues.RCM_TVS, m.group(19));
                map.put(MapValues.RCM_NUM_MESO, m.group(20));
                map.put(MapValues.RCM_MESO, m.group(19));
                map.put(MapValues.RCM_NUM_CENTROIDS, m.group(22));
                map.put(MapValues.RCM_CENT, m.group(19));
            }
            recordMap.put(MapValues.RCM_TYPE, map);
        }
    }

    /**
     * Split a single STI token into two values. Within the tabular block for
     * STI table data is formatted as "NO DATA", "NEW", or a number followed by
     * a slash followed by a number("271/ 23"). This method will separate the
     * two values and assign them within the map. If the token is "NO DATA" or
     * "NEW" both MapValues will be assign "NO DATA". If the token is anything
     * else an exception is logged and the values are set to "NO DATA".
     * 
     * @param token
     *            String to parse
     * @param map
     *            Map where results are inserted
     * @param left
     *            the key for the value on the left side of the '/'
     * @param right
     *            the key for the value on the right side of the '/'
     */
    private static void parseStiToken(String token, Map<MapValues, String> map,
            MapValues left, MapValues right) {
        String trimToken = token.trim();
        if (RadarConstants.NO_DATA.equals(trimToken) || "NEW".equals(trimToken)) {
            map.put(left, RadarConstants.NO_DATA);
            map.put(right, RadarConstants.NO_DATA);
        } else if (trimToken.contains("/")) {
            String[] tmp = trimToken.split("/");
            map.put(left, tmp[0].trim());
            if (tmp.length > 1) {
                map.put(right, tmp[1].trim());
            } else {
                /* It is unknown whether this ever occurs in valid record */
                map.put(right, RadarConstants.NO_DATA);
            }
        } else {
            statusHandler
                    .error(String
                            .format("Error parsing radar tabular block: [%s] is not a valid token for %s/%s",
                                    token, left.toString(), right.toString()));
            map.put(left, RadarConstants.NO_DATA);
            map.put(right, RadarConstants.NO_DATA);
        }
    }

}
