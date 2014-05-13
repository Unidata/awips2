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

package com.raytheon.uf.edex.plugin.tcs;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.tcs.decoder.TCSDataAdapter;

/**
 * Decoder implementation for the tcs (Tropical Cyclone Summary) plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009             jsanchez     Initial creation
 * May 14, 2014 2536        bclement     moved WMO Header to common
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TCSDecoder {
    private Log logger = LogFactory.getLog(getClass());

    private final String pluginName;

    private PointDataDescription pdd = null;

    private TropicalCycloneSummaryDao dao;

    private boolean failSafe = false;

    /**
     * 
     * @param name
     */
    public TCSDecoder(String name) {
        pluginName = name;
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/tcs.xml"));

            logger.debug("PointDataDescription loaded");

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
            TropicalCycloneSummary tcs = null;
            try {
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                WMOHeader wmoHeader = new WMOHeader(data, fileName);
                TCSDataAdapter adapter = TCSDataAdapter.getAdapter(pdd, dao,
                        pluginName, wmoHeader);

                adapter.setData(data, traceId, headers);
                tcs = adapter.getDecodedData();
            } catch (Exception e) {
                logger.error(traceId + "-Error in decode", e);
            } finally {
                if ((tcs != null)) {
                    decodedData = new PluginDataObject[] { tcs };
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
            dao = new TropicalCycloneSummaryDao(pluginName);
        } catch (Exception e) {
            logger.error("TropicalCycloneSummaryDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }
}
