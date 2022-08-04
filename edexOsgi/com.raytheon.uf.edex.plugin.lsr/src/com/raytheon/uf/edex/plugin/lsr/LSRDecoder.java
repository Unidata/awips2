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
package com.raytheon.uf.edex.plugin.lsr;

import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.lsr.LocalStormReport;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.lsr.decoder.LSRParser;

/**
 * Decoder for Local Storm Reports
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jan 21, 2009  1939        jkorman      Initial creation
 * Dec 09, 2013  2581        njensen      Updated javadoc
 * Jan 08, 2016  5226        tgurney      Reject ob with bad location
 * Mar 12, 2018  6824        randerso     Code cleanup
 * Mar 19, 2018  7246        randerso     Change LSRParser to have getReports()
 *                                        method instead of next/hasNext
 *
 * </pre>
 *
 * @author jkorman
 */
public class LSRDecoder {
    private IUFStatusHandler logger = UFStatus.getHandler(LSRDecoder.class);

    private final String pluginName;

    private PointDataDescription pdd = null;

    private LocalStormReportDao dao;

    private boolean failSafe = false;

    /**
     *
     * @param name
     */
    public LSRDecoder(String name) {
        pluginName = name;
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/lsr.xml"));

            logger.info("PointDataDescription loaded");

        } catch (Exception e) {
            logger.error("PointDataDescription failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
        try {
            createDAO(false);
        } catch (Exception e) {
            logger.error("Dao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }

    /**
     *
     * @param data
     * @param headers
     * @return
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {

        String traceId = null;

        PluginDataObject[] decodedData = null;

        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        if (isFailSafe()) {
            return new PluginDataObject[0];
        }

        logger.debug(traceId + " - Decoding data");

        if (data != null && data.length > 0) {
            List<LocalStormReport> obsList = null;
            try {
                LSRParser parser = new LSRParser(dao, pdd);
                parser.setData(data, traceId, headers);
                obsList = parser.getReports();
            } catch (Exception e) {
                logger.error(traceId + "-Error in decode", e);
            } finally {
                if ((obsList != null) && (!obsList.isEmpty())) {
                    decodedData = obsList
                            .toArray(new PluginDataObject[obsList.size()]);
                } else {
                    decodedData = new PluginDataObject[0];
                }
            }
        } else {
            logger.info(traceId + "- No data in file");
            decodedData = new PluginDataObject[0];
        }

        return decodedData;
    }

    public boolean isFailSafe() {
        return failSafe;
    }

    public void setFailSafe(boolean value) {
        failSafe = value;
    }

    /**
     *
     * @param recreate
     */
    protected void createDAO(boolean recreate) {
        if (recreate) {
            dao = null;
        }
        try {
            dao = new LocalStormReportDao(pluginName);
        } catch (Exception e) {
            logger.error("SSMIScanDataDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }

}
