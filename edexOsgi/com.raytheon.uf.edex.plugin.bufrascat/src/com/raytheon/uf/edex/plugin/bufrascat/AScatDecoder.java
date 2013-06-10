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
package com.raytheon.uf.edex.plugin.bufrascat;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrascat.AScatObs;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.bufrtools.AbstractBUFRDecoder;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.plugin.bufrascat.dao.AScatObsDao;
import com.raytheon.uf.edex.plugin.bufrascat.decoder.AScatDataAdapter;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder for ascat data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009 1939       jkorman     Initial creation
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AScatDecoder extends AbstractBUFRDecoder {

    private AScatObsDao dao;

    private PointDataDescription pdd;

    /**
     * 
     * @param name
     */
    public AScatDecoder(String name) {
        super(name);
        try {
            pdd = dao.getPointDataDescription(null);

            logger.info("PointDataDescription loaded");

        } catch (Exception e) {
            logger.error("PointDataDescription failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
        setFactoryDelegate(new DefaultDescriptorDelegate(this));
    }

    /**
     * 
     */
    @Override
    public List<PluginDataObject> decodeData(List<BUFRDataDocument> document,
            String traceId, WMOHeader wmoHeader) {

        List<PluginDataObject> decodedData = null;
        if (document != null) {
            decodedData = new ArrayList<PluginDataObject>();
            AScatDataAdapter adapter = new AScatDataAdapter(pdd, dao,
                    pluginName);

            logger.debug("List contains " + document.size()
                    + " BUFRDataDocuments");

            Iterator<BUFRDataDocument> iterator = document.iterator();
            while (iterator.hasNext()) {

                logger.debug("Decoding one BUFRDataDocument");
                AScatObs ascatObs = adapter.createData(iterator, wmoHeader);

                if (ascatObs != null) {
                    ascatObs.setTraceId(traceId);
                    decodedData.add(ascatObs);
                }
            }
        }
        return decodedData;
    }

    /**
     * 
     * @param recreate
     */
    @Override
    protected void createDAO(boolean recreate) {
        if (recreate) {
            dao = null;
        }
        try {
            dao = new AScatObsDao(pluginName);
        } catch (Exception e) {
            logger.error("QUIKScatObsDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }
}
