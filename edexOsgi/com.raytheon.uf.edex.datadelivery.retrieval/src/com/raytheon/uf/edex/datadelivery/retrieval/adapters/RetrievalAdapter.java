package com.raytheon.uf.edex.datadelivery.retrieval.adapters;

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

import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse;

/**
 * Abstract Provider Retrieval Adapter
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class RetrievalAdapter implements IRetrievalAdapter {

    protected Retrieval prxml;

    @Override
    public abstract IRetrievalRequestBuilder createRequestMessage(
            RetrievalAttribute prxml);

    @Override
    public abstract RetrievalResponse performRequest(
            IRetrievalRequestBuilder request);

    @Override
    public abstract HashMap<String, PluginDataObject[]> processResponse(
            IRetrievalResponse response) throws TranslationException;

    /**
     * Set the main providerRetrieval XML file
     * 
     * @param prxml
     */
    @Override
    public void setProviderRetrievalXML(Retrieval prxml) {
        this.prxml = prxml;
    }

    /**
     * Get the main provider retrieval XML file
     * 
     * @return
     */
    @Override
    public Retrieval getProviderRetrievalXMl() {
        return prxml;
    }

    public static class TranslationException extends Exception {

        private static final long serialVersionUID = -630731524436352713L;

        public TranslationException() {
            super();
        }

        public TranslationException(String message, Throwable cause) {
            super(message, cause);
        }

        public TranslationException(String message) {
            super(message);
        }

        public TranslationException(Throwable cause) {
            super(cause);
        }
    }
}
