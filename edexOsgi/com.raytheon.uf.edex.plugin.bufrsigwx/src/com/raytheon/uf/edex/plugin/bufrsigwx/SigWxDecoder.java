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
package com.raytheon.uf.edex.plugin.bufrsigwx;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.AbstractBUFRDecoder;
import com.raytheon.uf.edex.bufrtools.BUFRDataDocument;
import com.raytheon.uf.edex.bufrtools.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.plugin.bufrsigwx.decoder.SigWxDataAdapter;

/**
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009 1939       jkorman     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 04, 2016 5309       tgurney     Remove call to constructDataURI and
 *                                     check for null on dataURI component
 *                                     fields
 * Sep 23, 2021 8608       mapeters    Handle PDO.traceId changes
 *
 * </pre>
 *
 * @author jkorman
 */
public class SigWxDecoder extends AbstractBUFRDecoder {

    private SigWxDataDao dao;

    /**
     *
     * @param name
     */
    public SigWxDecoder(String name) {
        super(name);
        setFactoryDelegate(new DefaultDescriptorDelegate(this));
    }

    @Override
    public List<PluginDataObject> decodeData(List<BUFRDataDocument> document,
            String traceId, WMOHeader wmoHeader) {

        List<PluginDataObject> decodedData = null;
        if (document != null) {
            decodedData = new ArrayList<>();
            SigWxDataAdapter adapter = SigWxDataAdapter.getAdapter(null, dao,
                    traceId, wmoHeader);
            logger.debug(traceId + " - Document size = " + document.size());
            Iterator<BUFRDataDocument> iterator = document.iterator();
            while (iterator.hasNext()) {
                logger.debug(traceId + " - Entering createDataList");

                List<SigWxData> sigwx = adapter.createDataList(iterator,
                        wmoHeader);
                if (sigwx != null) {
                    for (SigWxData d : sigwx) {
                        d.setSourceTraceId(traceId);
                        if (d.getWxLayer() == null) {
                            logger.warn(
                                    "Discarding record with wxLayer == null: "
                                            + d.toString());
                        } else if (d.getWxType() == null) {
                            logger.warn(
                                    "Discarding record with wxType == null: "
                                            + d.toString());
                        } else if (d.getKey() == null) {
                            logger.warn("Discarding record with key == null: "
                                    + d.toString());
                        } else {
                            decodedData.add(d);
                        }
                    }
                } else {
                    logger.info("No data returned from createDataList");
                }
            }
        }
        return decodedData;
    }

    @Override
    protected void createDAO(boolean recreate) {
        if (recreate) {
            dao = null;
        }
        try {
            dao = new SigWxDataDao(pluginName);
        } catch (Exception e) {
            logger.error("SigWxDataDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }

}
