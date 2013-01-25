package com.raytheon.uf.edex.datadelivery.retrieval.interfaces;

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
 * </pre>/
 * 
 * @author dhladky
 * @version 1.0
 */

import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Interface for Provider Retrieval Handlers
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

public interface IRetrievalHandler {

    public boolean store(RetrievalAttribute attXML, PluginDataObject[] pdos);

    public boolean process(Retrieval srxml);
}