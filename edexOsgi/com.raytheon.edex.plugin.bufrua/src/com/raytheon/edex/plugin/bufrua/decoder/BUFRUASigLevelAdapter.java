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

import com.raytheon.edex.plugin.bufrua.util.SigWindHeightConversionManager;
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
 * Convert bufr packets into level data for bufrua significant levels.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 21, 2009           jkorman     Initial creation
 * Dec 05, 2013  2612     bsteffen    Convert heights for sig wind layers.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRUASigLevelAdapter extends AbstractBUFRUAAdapter {

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public BUFRUASigLevelAdapter(PointDataDescription pdd,
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

            int ii = wmoHeader.getIi() % 10;
            if (ii == 2) {
                obsData = getSigTemperatureLevels(obsData, dataList.get(12),
                        view);
                obsData.setReportType(LayerTools.SIGTLVL_LO);
            } else if (ii == 4) {
                obsData = getSigTemperatureLevels(obsData, dataList.get(12),
                        view);
                obsData.setReportType(LayerTools.SIGTLVL_HI);
            } else if (ii == 6) {
                obsData = getSigWindLevels(obsData, dataList.get(12), view);
                obsData.setReportType(LayerTools.SIGWLVL_LO);
            } else if (ii == 8) {
                obsData = getSigWindLevels(obsData, dataList.get(12), view);
                obsData.setReportType(LayerTools.SIGWLVL_HI);
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
     *            containing the significant level data.
     * @return
     */
    @SuppressWarnings("unchecked")
    private UAObs getSigTemperatureLevels(UAObs pointData,
            IBUFRDataPacket dataPoint, PointDataView view) {

        if ((dataPoint instanceof BUFRSublistPacket)
                && (RepSubList.getPacketType().equals(dataPoint.getUnits()))) {
            List<IBUFRDataPacket> datList = (List<IBUFRDataPacket>) dataPoint
                    .getValue();
            int tempIdx = 0;

            int maxEntries = -1;
            Dimension[] dims = getPointDataDescription().dimensions;
            for (Dimension d : dims) {
                if ("maxSigWLevels".equals(d.getDimensionName())) {
                    maxEntries = d.getDimensionLength();
                }
            }

            for (IBUFRDataPacket packet : datList) {
                List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                        .getValue();
                int sig = getInt(p.get(0), IDecoderConstants.VAL_MISSING);
                double pres = getDouble(p.get(1), -1);
                if (sig == 4) {
                    if ((pres > 32700.0) && (pres < 32800.0)) {
                    } else {
                        if ((pres > 0) && (pres < 120000)) {
                            view.setFloat(LayerTools.PR_SIGT, (float) pres,
                                    tempIdx);
                            double t = getDouble(p.get(3), -9999);
                            if (t < -9999) {
                                t = -9999.0;
                            }
                            view.setFloat(LayerTools.TP_SIGT, (float) t,
                                    tempIdx);
                            t = getDouble(p.get(4), -9999);
                            if (t < -9999) {
                                t = -9999.0;
                            }
                            view.setFloat(LayerTools.TD_SIGT, (float) t,
                                    tempIdx);
                            tempIdx++;
                        }
                    }
                }
                if (tempIdx == maxEntries) {
                    break;
                }
            } // for
            view.setInt(LayerTools.NUM_SIGT, tempIdx);

        }
        return pointData;
    }

    /**
     * 
     * @param pointData
     * @param dataPoint
     *            A bufr data packet that should be a BUFRSublistPacket
     *            containing the significant level data.
     * @return
     */
    @SuppressWarnings("unchecked")
    private UAObs getSigWindLevels(UAObs pointData, IBUFRDataPacket dataPoint,
            PointDataView view) {

        if ((dataPoint instanceof BUFRSublistPacket)
                && (RepSubList.getPacketType().equals(dataPoint.getUnits()))) {
            List<IBUFRDataPacket> datList = (List<IBUFRDataPacket>) dataPoint
                    .getValue();
            int windIdx = 0;
            boolean haveSfc = false;

            int maxEntries = -1;
            Dimension[] dims = getPointDataDescription().dimensions;
            for (Dimension d : dims) {
                if ("maxSigWLevels".equals(d.getDimensionName())) {
                    maxEntries = d.getDimensionLength();
                }
            }

            for (IBUFRDataPacket packet : datList) {
                List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                        .getValue();
                int sig = getInt(p.get(1), IDecoderConstants.VAL_MISSING);
                double height = getDouble(p.get(0), -9999);
                height = SigWindHeightConversionManager.convertHeight(
                        pointData, height);
                if (sig == 2) {
                    if ((height > 0) && (height < 30000)) {
                        view.setFloat(LayerTools.HT_SIGW, (float) height,
                                windIdx);
                        setViewData(LayerTools.WD_SIGW, view, p.get(2), windIdx);
                        setViewData(LayerTools.WS_SIGW, view, p.get(3), windIdx);
                        windIdx++;
                    } else {
                        // need this to ensure we don't store -9999s
                        if ((height == 0) && (!haveSfc)) {
                            view.setFloat(LayerTools.HT_SIGW, (float) height,
                                    windIdx);
                            setViewData(LayerTools.WD_SIGW, view, p.get(2),
                                    windIdx);
                            setViewData(LayerTools.WS_SIGW, view, p.get(3),
                                    windIdx);
                            haveSfc = true;
                            windIdx++;
                        }
                    }
                }
                if (height == 30000) {
                    break;
                }
                if (windIdx == maxEntries) {
                    break;
                }
            } // for
            view.setInt(LayerTools.NUM_SIGW, windIdx);
        }
        return pointData;
    }
}
