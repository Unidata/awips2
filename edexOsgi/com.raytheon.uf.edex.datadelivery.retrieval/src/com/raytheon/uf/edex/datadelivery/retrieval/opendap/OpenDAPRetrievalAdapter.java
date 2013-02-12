package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

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
import com.raytheon.uf.edex.datadelivery.retrieval.response.OpenDAPTranslator;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ConnectionUtil;

import dods.dap.DConnect;
import dods.dap.DataDDS;

/**
 * OpenDAP Provider Retrieval Adapter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky     Initial creation
 * Jun 28, 2012 819        djohnson    Use utility class for DConnect.
 * Jul 25, 2012 955        djohnson    Make package-private.
 * Feb 05, 2013 1580       mpduff      EventBus refactor.
 * Feb 12, 2013 1543       djohnson    The payload can just be an arbitrary object, implementations can define an array if required.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

class OpenDAPRetrievalAdapter extends RetrievalAdapter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDAPRetrievalAdapter.class);

    @Override
    public OpenDAPRequestBuilder createRequestMessage(RetrievalAttribute attXML) {

        OpenDAPRequestBuilder reqBuilder = new OpenDAPRequestBuilder(this,
                attXML);

        return reqBuilder;
    }

    @Override
    public RetrievalResponse performRequest(
            IRetrievalRequestBuilder request) {

        DataDDS data = null;

        try {
            DConnect connect = ConnectionUtil.getDConnect(request.getRequest());
            data = connect.getData(null);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            EventBus.publish(new RetrievalEvent(e.getMessage()));
        }

        RetrievalResponse pr = new OpenDapRetrievalResponse(
                request.getAttribute());
        pr.setPayLoad(data);

        return pr;
    }

    @Override
    public Map<String, PluginDataObject[]> processResponse(
            IRetrievalResponse response) throws TranslationException {
        Map<String, PluginDataObject[]> map = new HashMap<String, PluginDataObject[]>();

        OpenDAPTranslator translator;
        try {
            translator = getOpenDapTranslator(response.getAttribute());
        } catch (InstantiationException e) {
            throw new TranslationException(
                    "Unable to instantiate a required class!", e);
        }

        final DataDDS payload;
        try {
            payload = DataDDS.class.cast(response.getPayLoad());
        } catch (ClassCastException e) {
            throw new TranslationException(e);
        }

        if (payload != null) {
            PluginDataObject[] pdos = translator.asPluginDataObjects(payload);

            if (!CollectionUtil.isNullOrEmpty(pdos)) {
                String pluginName = pdos[0].getPluginName();
                map.put(pluginName,
                        CollectionUtil.combine(PluginDataObject.class,
                                map.get(pluginName), pdos));
            }
        }

        return map;
    }

    /**
     * @param attribute
     * @return
     */
    OpenDAPTranslator getOpenDapTranslator(RetrievalAttribute attribute)
            throws InstantiationException {
        return new OpenDAPTranslator(attribute);
    }
}
