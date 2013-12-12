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

import java.util.List;

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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRUAManLevelAdapter extends AbstractBUFRUAAdapter {

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
        float sfcPressure = -9999;

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
                double pres = getDouble(p.get(0), -9999);
                switch (sig) {

                case LayerTools.TROP_LEVEL: { // Tropopause level
                    if ((tropIdx < maxTropLevels) && (pres > 0)
                            && (pres != 99900.0)) {
                        setViewData("prTrop", view, p.get(0), tropIdx);
                        double t = getDouble(p.get(3), -9999);
                        if (t < -9999) {
                            t = -9999.0;
                        }
                        view.setFloat("tpTrop", (float) t, tropIdx);
                        t = getDouble(p.get(4), -9999);
                        if (t < -9999) {
                            t = -9999.0;
                        }
                        view.setFloat("tdTrop", (float) t, tropIdx);
                        setViewData("wdTrop", view, p.get(5), tropIdx);
                        setViewData("wsTrop", view, p.get(6), tropIdx);
                        tropIdx++;
                    }
                    break;
                }
                case LayerTools.SFC_LEVEL: {
                    sfcPressure = (float) getDouble(p.get(0), -9999);
                    // fall through
                }
                case LayerTools.MANPRE_LEVEL: {
                    // Surface and Mandatory levels
                    if ((manIdx < maxManLevels) && (pres > 0)) {
                        setViewData("prMan", view, p.get(0), manIdx);
                        setViewData("htMan", view, p.get(2), manIdx);
                        double t = getDouble(p.get(3), -9999);
                        if (t < -9999) {
                            t = -9999.0;
                        }
                        view.setFloat("tpMan", (float) t, manIdx);
                        t = getDouble(p.get(4), -9999);
                        if (t < -9999) {
                            t = -9999.0;
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
                    double pres = getDouble(p.get(0), -9999);
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
}
