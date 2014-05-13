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
package com.raytheon.uf.edex.plugin.bufrncwf;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrncwf.BUFRncwf;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.AbstractBUFRDecoder;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.plugin.bufrncwf.decoder.BUFRncwfDataAdapter;

/**
 * Decodes National Convective Weather Forecast Product in BUFR format
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2009            jkorman     Initial creation
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed constructDataURI() call
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRncwfDecoder extends AbstractBUFRDecoder {

    private PointDataDescription pdd;

    private BUFRncwfDao dao;

    /**
     * 
     * @param name
     */
    public BUFRncwfDecoder(String name) {
        super(name);
        try {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/bufrncwf.xml"));

            logger.info("PointDataDescription loaded");
        
        } catch(Exception e) {
            logger.error("PointDataDescription failed",e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
        setFactoryDelegate(new DefaultDescriptorDelegate(this));
    }

    /**
     * 
     */
    @Override
    public List<PluginDataObject> decodeData(List<BUFRDataDocument> document, String traceId, WMOHeader wmoHeader) {

        List<PluginDataObject> decodedData = null;  
        if(document != null) {
            decodedData = new ArrayList<PluginDataObject>();
            BUFRncwfDataAdapter adapter = new BUFRncwfDataAdapter(pdd, dao, pluginName);
            
            logger.debug("List contains " + document.size() + " BUFRDataDocuments");
            
            Iterator<BUFRDataDocument> iterator = document.iterator();
            while (iterator.hasNext()) {
                
                logger.debug("Decoding one BUFRDataDocument");
                List<BUFRncwf> ncwfReports = adapter.createDataList(iterator, wmoHeader);
                if (ncwfReports != null) {
                    for(BUFRncwf rpt : ncwfReports) {
                        rpt.setTraceId(traceId);
                        decodedData.add(rpt);
                    }
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
        if(recreate) {
            dao = null;
        }
        try {
            dao = new BUFRncwfDao(pluginName);
        } catch (Exception e) {
            logger.error("QUIKScatObsDao creation failed",e);
            logger.error("Plugin set to failSafe mode");
            setFailSafe(true);
        }
    }

}
