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
package com.raytheon.uf.edex.plugin.tcg;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.tcg.TropicalCycloneGuidance;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.tcg.decoder.TCGDataAdapter;

/**
 * Decoder implementation for the tcg (Tropical Cyclone Guidance) plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009             jsanchez     Initial creation
 * May 14, 2014 2536        bclement     moved WMO Header to common
 * Jun 24, 2014 3235        nabowle      Switch to slf4j.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TCGDecoder {
    private static final Logger logger = LoggerFactory
            .getLogger(TCGDecoder.class);

    private final String pluginName;

    private PointDataDescription pdd = null;

    private TropicalCycloneGuidanceDao dao;

    private boolean failSafe = false;

    /**
     * 
     * @param name
     */
    public TCGDecoder(String name) {
        pluginName = name;
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/tcg.xml"));

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
            List<TropicalCycloneGuidance> obsList = new ArrayList<TropicalCycloneGuidance>();
            try {
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                WMOHeader wmoHeader = new WMOHeader(data, fileName);
                TCGDataAdapter adapter = TCGDataAdapter.getAdapter(pdd, dao,
                        pluginName, wmoHeader);
                adapter.setData(data, traceId, headers);

                TropicalCycloneGuidance report;
                while (adapter.hasNext()) {
                    report = adapter.next();
                    if (report != null) {
                        obsList.add(report);
                    }
                }
            } catch (Exception e) {
                logger.error(traceId + "-Error in decode", e);
            } finally {
                if ((obsList != null) && (obsList.size() > 0)) {
                    decodedData = obsList.toArray(new PluginDataObject[obsList
                            .size()]);
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
            dao = new TropicalCycloneGuidanceDao(pluginName);
        } catch (Exception e) {
            logger.error("TropicalCycloneGuidanceDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }
}
