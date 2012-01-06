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
package com.raytheon.uf.edex.plugin.vaa;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;
import com.raytheon.uf.common.dataplugin.vaa.dao.VAARecordDao;
import com.raytheon.uf.edex.plugin.vaa.decoder.VAAParser;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 04, 2009       3267 jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class VAADecoder {
    private Log logger = LogFactory.getLog(getClass());

    private final String pluginName;

    private boolean failSafe = false;

    /**
     * 
     * @param name
     */
    public VAADecoder(String name) {
        pluginName = name;
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
            List<PluginDataObject> obsList = new ArrayList<PluginDataObject>();
            try {
                VAAParser parser = new VAAParser(pluginName, data, traceId,
                        headers);
                for (VAARecord record : parser) {
                    if (record != null) {
                        try {
                            record.constructDataURI();
                            if (!checkForDup(record)) {
                                obsList.add(record);
                            }
                        } catch (PluginException e) {
                            logger.error(traceId
                                    + "- Unable to construct dataURI", e);
                            record = null;
                        }
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

    /**
     * 
     * @return
     */
    public boolean isFailSafe() {
        return failSafe;
    }

    /**
     * 
     * @param value
     */
    public void setFailSafe(boolean value) {
        failSafe = value;
    }

    /**
     * 
     * @param vaa
     * @return
     */
    private boolean checkForDup(VAARecord vaa) {
        boolean isDup = false;

        try {
            VAARecordDao dao = new VAARecordDao(pluginName);
            Object[] res = dao.queryDataUriColumn(vaa.getDataURI());
            isDup = (res != null) && (res.length > 0);
        } catch (Exception e) {
            logger.error("Unable to create VAARecordDao", e);
        }
        return isDup;
    }
}
