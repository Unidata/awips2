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
package com.raytheon.uf.edex.plugin.svrwx;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.svrwx.SvrWxRecord;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.svrwx.decoder.SvrWxParser;

/**
 * SvrWx Decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2010            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class SvrWxDecoder {
    private static IUFStatusHandler logger = UFStatus
            .getHandler(SvrWxDecoder.class);

    private final String pluginName;

    private PointDataDescription pdd = null;

    private SvrWxRecordDao dao;

    private boolean failSafe = false;

    /**
     * 
     * @param name
     */
    public SvrWxDecoder(String name) {
        pluginName = name;
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/svrwx.xml"));

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
     * Decoded input data.
     * 
     * @param data
     * @param headers
     * @return decodedData
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
            List<SvrWxRecord> obsList = new ArrayList<SvrWxRecord>();
            try {
                SvrWxParser parser = new SvrWxParser(dao, pdd, pluginName);
                parser.setData(data, traceId, headers);

                SvrWxRecord report;
                while (parser.hasNext()) {
                    report = parser.next();
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
            dao = new SvrWxRecordDao(pluginName);
        } catch (Exception e) {
            logger.error("SvrWxRecordDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }
}
