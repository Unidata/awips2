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
package com.raytheon.uf.edex.plugin.cwa;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwa.CWARecord;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.cwa.decoder.CWAParser;
import com.raytheon.uf.edex.plugin.cwa.util.TableLoader;

/**
 * CWA Decoder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2010            jsanchez     Initial creation
 * Apr 19, 2012  #457      dgilling     Minor code cleanup.
 * Mar 25, 2014  2930      skorolev     Fixed error in distance.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class CWADecoder {
    private IUFStatusHandler logger = UFStatus.getHandler(CWADecoder.class);

    private final String pluginName;

    private PointDataDescription pdd = null;

    private TableLoader loader = null;

    private CWARecordDao dao;

    private boolean failSafe = false;

    /**
     * 
     * @param name
     */
    public CWADecoder(String name) {
        pluginName = name;
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/cwa.xml"));
            loader = new TableLoader();
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
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws MalformedDataException, IOException {
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
            List<CWARecord> obsList = new ArrayList<CWARecord>();
            try {
                CWAParser parser = new CWAParser(dao, pdd, pluginName, loader);
                parser.setData(data, traceId, headers);

                CWARecord report;
                while (parser.hasNext()) {
                    report = parser.next();
                    if (report != null) {
                        obsList.add(report);
                    }
                }
            } catch (Exception e) {
                logger.error(traceId + "-Error in decode", e);
            } finally {
                if (!obsList.isEmpty()) {
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
            dao = new CWARecordDao(pluginName);
        } catch (Exception e) {
            logger.error("CWARecordDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }

}
