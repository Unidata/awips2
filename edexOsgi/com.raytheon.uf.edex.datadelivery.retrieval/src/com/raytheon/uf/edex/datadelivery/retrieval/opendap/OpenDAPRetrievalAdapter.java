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

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalEvent;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.response.OpenDAPTranslator;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ConnectionUtil;
import com.raytheon.uf.edex.event.EventBus;

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
    public OpenDAPRequestBuilder createRequestMessage(
            RetrievalAttribute attXML) {

        OpenDAPRequestBuilder reqBuilder = new OpenDAPRequestBuilder(this,
                attXML);

        return reqBuilder;
    }

    @Override
    public RetrievalResponse performRequest(IRetrievalRequestBuilder request) {

        DataDDS data = null;

        try {
            DConnect connect = ConnectionUtil.getDConnect(request.getRequest());
            data = connect.getData(null);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            EventBus.getInstance().publish(new RetrievalEvent(e.getMessage()));
        }

        RetrievalResponse pr = new RetrievalResponse(request.getAttribute());
        pr.setPayLoad(new Object[] { data });

        return pr;
    }
    @Override
    public HashMap<String, PluginDataObject[]> processResponse(
            IRetrievalResponse response) throws TranslationException {
        HashMap<String, PluginDataObject[]> map = new HashMap<String, PluginDataObject[]>();

        OpenDAPTranslator translator;
        try {
            translator = new OpenDAPTranslator(response.getAttribute());
        } catch (InstantiationException e) {
            throw new TranslationException(
                    "Unable to instantiate a required class!", e);
        }

            if (response.getPayLoad() != null
                    && response.getPayLoad().length > 0) {
                for (Object obj : response.getPayLoad()) {
                    PluginDataObject[] pdos = null;

                    if (obj instanceof DataDDS) {
                        pdos = translator.asPluginDataObjects((DataDDS) obj);
                    }

                    if (pdos != null && pdos.length > 0) {
                        String pluginName = pdos[0].getPluginName();
                        // TODO Need to check if pluginName already exists
                        map.put(pluginName, pdos);
                    }
                }
            }


        return map;
    }
}
