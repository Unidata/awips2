package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

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

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalEvent;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.response.WfsTranslator;
import com.raytheon.uf.edex.datadelivery.retrieval.util.WfsConnectionUtil;

/**
 * WFS OGC Provider Retrieval Adapter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky      Initial creation
 * Jul 25, 2012 955        djohnson     Moved to wfs specific package.
 * May 12, 2013 753        dhladky      implemented.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class WfsRetrievalAdapter extends RetrievalAdapter {

    private static final IUFStatusHandler statusHandler = UFStatus
    .getHandler(WfsRetrievalAdapter.class);
    
    private Pattern splitter = Pattern.compile("?");
    
    @Override
    public IRetrievalRequestBuilder createRequestMessage(
            RetrievalAttribute attXML) {

        WfsRequestBuilder reqBuilder = new WfsRequestBuilder(this, attXML);

        return reqBuilder;
    }

    @Override
    public Map<String, PluginDataObject[]> processResponse(
            IRetrievalResponse response) throws TranslationException {
        
        Map<String, PluginDataObject[]> map = new HashMap<String, PluginDataObject[]>();
        WfsTranslator translator;
        try {
            translator = getWfsTranslator(response.getAttribute());
        } catch (InstantiationException e) {
            throw new TranslationException(
                    "Unable to instantiate a required class!", e);
        }

        String payload = (String) response.getPayLoad();

        try {
            if (payload != null) {
                PluginDataObject[] pdos = translator
                        .asPluginDataObjects(payload);

                if (!CollectionUtil.isNullOrEmpty(pdos)) {
                    String pluginName = pdos[0].getPluginName();
                    map.put(pluginName,
                            CollectionUtil.combine(PluginDataObject.class,
                                    map.get(pluginName), pdos));
                }
            }
        } catch (ClassCastException e) {
            throw new TranslationException(e);
        }

        return map;
        
    }

    @Override
    public RetrievalResponse performRequest(
            IRetrievalRequestBuilder request) {

        String xmlMessage = null;
        try {
            // break url into CGI parameters and address
            splitter.split(request.getRequest());
            String[] parts = splitter.split(request.getRequest());
            xmlMessage = WfsConnectionUtil.wfsConnect(parts[0], parts[1]);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            EventBus.publish(new RetrievalEvent(e.getMessage()));
        }

        RetrievalResponse pr = new WfsRetrievalResponse(request.getAttribute());
        pr.setPayLoad(xmlMessage);

        return pr;
    }
    
    /**
     * @param attribute
     * @return
     */
    WfsTranslator getWfsTranslator(RetrievalAttribute attribute)
            throws InstantiationException {
        return new WfsTranslator(attribute);
    }

}
