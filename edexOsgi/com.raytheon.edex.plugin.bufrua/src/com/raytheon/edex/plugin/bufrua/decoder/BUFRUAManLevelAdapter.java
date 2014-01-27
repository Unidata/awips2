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
package com.raytheon.edex.plugin.bufrua.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.bufrua.LayerTools;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.Dimension;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Convert bufr packets into level data for bufrua mandatory levels.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  969      jkorman     Initial implementation.
 * Dec 05, 2013  2612     bsteffen    Fix max wind decoding.
 * Dec 17, 2013  2639     bsteffen    Validate mandatory level heights.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRUAManLevelAdapter extends AbstractBUFRUAAdapter {

    /** Mandatory pressure levels */
    private static final float[] VALID_PR = { 100000, 92500, 85000, 70000,
            50000, 40000, 30000, 25000, 20000, 15000, 10000, 5000 };

    /** Reasonable height levels corresponding to VALID_PR */
    private static final float[] VALID_HT = { 100, 750, 1450, 3000, 5550, 7150,
            9150, 10350, 11800, 13600, 16150, 20000 };

    /** Map VALID_PR to VALID_HT values. */
    private static final Map<Float, Float> VALID_HEIGHT_MAP = generateValidHeights();

    /** Reasonable range for reasonable heights in VALID_HT */
    private static final float VALID_HEIGHT_RANGE = 1000;

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public BUFRUAManLevelAdapter(PointDataDescription pdd,
            PointDataPluginDao<UAObs> dao, String pluginName) {
        super(pdd, dao, pluginName);
    }

    /**
     * 
     */
    @Override
    UAObs getSpecificData(UAObs obsData, PointDataView view,
            List<IBUFRDataPacket> dataList) {
        if (obsData != null) {
            obsData = getManLevels(obsData, dataList.get(12), view);
            obsData = getMaxWindLevels(obsData, dataList.get(13), view);
            int ii = wmoHeader.getIi() % 10;
            if (ii == 1) {
                obsData.setReportType(LayerTools.MANLVL_LO);
            } else if (ii == 3) {
                obsData.setReportType(LayerTools.MANLVL_HI);
            } else {
                logger.error("Unknown data type " + wmoHeader.getWmoHeader());
                obsData = null;
            }
        }

        return obsData;
    }

    /**
     * 
     * @param pointData
     * @param dataPoint
     *            A bufr data packet that should be a BUFRSublistPacket
     *            containing the mandatory level data.
     * @return
     */
    @SuppressWarnings("unchecked")
    private UAObs getManLevels(UAObs pointData, IBUFRDataPacket dataPoint,
            PointDataView view) {

        int maxManLevels = -1;
        int maxTropLevels = -1;
        float sfcPressure = PDV_FILL_INT;

        Dimension[] dims = getPointDataDescription().dimensions;
        for (Dimension d : dims) {
            if ("maxManLevels".equals(d.getDimensionName())) {
                maxManLevels = d.getDimensionLength();
            }
            if ("maxTropLevels".equals(d.getDimensionName())) {
                maxTropLevels = d.getDimensionLength();
            }
        }

        if ((dataPoint instanceof BUFRSublistPacket)
                && (RepSubList.getPacketType().equals(dataPoint.getUnits()))) {
            List<IBUFRDataPacket> datList = (List<IBUFRDataPacket>) dataPoint
                    .getValue();
            int manIdx = 0;
            int tropIdx = 0;
            for (IBUFRDataPacket packet : datList) {
                List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                        .getValue();
                int sig = getInt(p.get(1), IDecoderConstants.VAL_MISSING);
                double pres = getDouble(p.get(0), PDV_FILL_DBL);
                switch (sig) {

                case LayerTools.TROP_LEVEL: { // Tropopause level
                    if ((tropIdx < maxTropLevels) && (pres > 0)
                            && (pres != 99900.0)) {
                        setViewData("prTrop", view, p.get(0), tropIdx);
                        double t = getDouble(p.get(3), PDV_FILL_DBL);
                        if (t < PDV_FILL_DBL) {
                            t = PDV_FILL_DBL;
                        }
                        view.setFloat("tpTrop", (float) t, tropIdx);
                        t = getDouble(p.get(4), PDV_FILL_DBL);
                        if (t < PDV_FILL_DBL) {
                            t = PDV_FILL_DBL;
                        }
                        view.setFloat("tdTrop", (float) t, tropIdx);
                        setViewData("wdTrop", view, p.get(5), tropIdx);
                        setViewData("wsTrop", view, p.get(6), tropIdx);
                        tropIdx++;
                    }
                    break;
                }
                case LayerTools.SFC_LEVEL: {
                    sfcPressure = (float) getDouble(p.get(0), PDV_FILL_DBL);
                    // fall through
                }
                case LayerTools.MANPRE_LEVEL: {
                    // Surface and Mandatory levels
                    if ((manIdx < maxManLevels) && (pres > 0)) {
                        setViewData("prMan", view, p.get(0), manIdx);
                        setViewData("htMan", view, p.get(2), manIdx);
                        double t = getDouble(p.get(3), PDV_FILL_DBL);
                        if (t < PDV_FILL_DBL) {
                            t = PDV_FILL_DBL;
                        }
                        view.setFloat("tpMan", (float) t, manIdx);
                        t = getDouble(p.get(4), PDV_FILL_DBL);
                        if (t < PDV_FILL_DBL) {
                            t = PDV_FILL_DBL;
                        }
                        view.setFloat("tdMan", (float) t, manIdx);
                        setViewData("wdMan", view, p.get(5), manIdx);
                        setViewData("wsMan", view, p.get(6), manIdx);
                        manIdx++;
                    }
                    break;
                }
                // No default!
                } // switch
            } // for
            view.setInt("numMand", manIdx);
            view.setInt("numTrop", tropIdx);
            view.setFloat("sfcPressure", sfcPressure);
            removeInvalidHeights(view);
        }
        return pointData;
    }

    /**
     * 
     * @param pointData
     * @param dataPoint
     *            A bufr data packet that should be a BUFRSublistPacket
     *            containing the mandatory level data.
     * @return
     */
    @SuppressWarnings("unchecked")
    private UAObs getMaxWindLevels(UAObs pointData, IBUFRDataPacket dataPoint,
            PointDataView view) {

        int maxMaxWinds = -1;

        Dimension[] dims = getPointDataDescription().dimensions;
        for (Dimension d : dims) {
            if ("maxMaxWinds".equals(d.getDimensionName())) {
                maxMaxWinds = d.getDimensionLength();
            }
        }

        if ((dataPoint instanceof BUFRSublistPacket)
                && (RepSubList.getPacketType().equals(dataPoint.getUnits()))) {
            List<IBUFRDataPacket> datList = (List<IBUFRDataPacket>) dataPoint
                    .getValue();
            int maxWindIdx = 0;
            for (IBUFRDataPacket packet : datList) {
                List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                        .getValue();
                int sig = getInt(p.get(1), IDecoderConstants.VAL_MISSING);
                if (sig == LayerTools.MAXWND_LEVEL) {
                    double pres = getDouble(p.get(0), PDV_FILL_DBL);
                    if (pres > 0) {
                        setViewData("prMaxW", view, p.get(0), maxWindIdx);
                        setViewData("wdMaxW", view, p.get(2), maxWindIdx);
                        setViewData("wsMaxW", view, p.get(3), maxWindIdx);
                        maxWindIdx++;
                    }
                    if (maxWindIdx == maxMaxWinds) {
                        break;
                    }
                }
            } // for
            view.setInt("numMwnd", maxWindIdx);
        }
        return pointData;
    }

    /**
     * Check the heights for each reading, removing invalid readings. Check
     * that heights are within the range specified from the mean value and that
     * they are between the preceeding and following values.
     *
     * One reason this is needed is because there is a known error in the
     * encoded data when the height for the 250MB level is less than 10000. For
     * these cases the encoder is prepending a 1 so a height of 9990 becomes
     * 19990. It appears this may be an artifact of the compression used to
     * encode the heights. For this case it would be theoretically possible to
     * remove the extra 1 and treat the data as valid, but invalidating the
     * height is done because it is not clear if this would always be a safe
     * fix or if there are other possible errors to detect.
     *
     * @param view
     *            {@link PointDataView} which will be modified to have invalid
     *            mandataory hight data removed.
     */
    private void removeInvalidHeights(PointDataView view) {
        int numMand = view.getInt("numMand");
        if (numMand < 3) {
            return;
        }
        /* Convert pressure and height data into a map for easy access. */
        Number[] pr = view.getNumberAllLevels("prMan");
        Number[] ht = view.getNumberAllLevels("htMan");
        Map<Float, Float> heights = new HashMap<Float, Float>(numMand * 2);
        for (int i = 0; i < numMand; i += 1) {
            heights.put(pr[i].floatValue(), ht[i].floatValue());
        }
        /* Check each predefined level. */
        Set<Float> invalidPrLevels = new HashSet<Float>();
        for (int i = 1; i < VALID_PR.length - 1; i += 1) {
            float prLevel = VALID_PR[i];
            float validHt = VALID_HEIGHT_MAP.get(prLevel);
            float minHt = validHt - VALID_HEIGHT_RANGE;
            float maxHt = validHt + VALID_HEIGHT_RANGE;
            Float testHt = heights.get(prLevel);
            /* First detect values which don't look reasonable. */
            if (testHt != null && testHt > PDV_FILL_INT
                    && (minHt > testHt || maxHt < testHt)) {
                float prevPr = VALID_PR[i - 1];
                float nextPr = VALID_PR[i + 1];
                Float prevHt = heights.get(prevPr);
                Float nextHt = heights.get(nextPr);
                /* Next check if its at least ascending. */
                if (prevHt != null && prevHt > PDV_FILL_INT && nextHt != null
                        && nextHt > PDV_FILL_INT
                        && (testHt < prevHt || testHt > nextHt)) {
                    invalidPrLevels.add(prLevel);
                }
            }
        }

        if (invalidPrLevels.isEmpty()) {
            return;
        }

        for (int i = 0; i < numMand; i += 1) {
            if (invalidPrLevels.contains(pr[i].floatValue())) {
                view.setFloat("htMan", PDV_FILL_INT, i);
            }
        }
    }

    private static Map<Float, Float> generateValidHeights() {
        Map<Float, Float> validHeights = new HashMap<Float, Float>();
        for (int i = 0; i < VALID_HT.length; i += 1) {
            validHeights.put(VALID_PR[i], VALID_HT[i]);
        }
        return Collections.unmodifiableMap(validHeights);
    }
}
