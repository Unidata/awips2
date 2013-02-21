package com.raytheon.uf.edex.datadelivery.retrieval.interfaces;

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
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Interface for Provider Retrieval Adapter
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
 * /
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IRetrievalAdapter {

    public IRetrievalRequestBuilder createRequestMessage(
            RetrievalAttribute prxml);

    public com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse performRequest(
            IRetrievalRequestBuilder requestBuilder);

    public HashMap<String, PluginDataObject[]> processResponse(
            IRetrievalResponse response) throws Exception;

    public void setProviderRetrievalXML(Retrieval prxml);

    public Retrieval getProviderRetrievalXMl();

}
