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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalPersistUtil;

/**
 * Implementation of {@link IRetrievedDataProcessor} that stores the plugin data
 * objects to the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class StoreRetrievedData implements IRetrievalPluginDataObjectsProcessor {

    private final String generalDestinationUri;

    /**
     * Constructor.
     * 
     * @param generalDestinationUri
     *            the destination uri most plugin data will travel through
     */
    public StoreRetrievedData(String generalDestinationUri) {
        this.generalDestinationUri = generalDestinationUri;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processRetrievedPluginDataObjects(
            RetrievalPluginDataObjects retrievalPluginDataObjects) {
        final RetrievalRequestRecord requestRecord = retrievalPluginDataObjects
                .getRequestRecord();
        final List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects = retrievalPluginDataObjects
                .getRetrievalAttributePluginDataObjects();

        for (RetrievalAttributePluginDataObjects pluginDataObjectEntry : retrievalAttributePluginDataObjects) {
            PluginDataObject[] value = pluginDataObjectEntry
                    .getPluginDataObjects();

            if (value.length == 0) {
                continue;
            }

            sendToDestinationForStorage(requestRecord, value);
        }
    }

    /**
     * Sends the plugin data objects to their configured destination for storage
     * to the database.
     */
    public boolean sendToDestinationForStorage(
            RetrievalRequestRecord requestRecord,
            PluginDataObject[] pdos) {
        // do all of the PDO storage magic...
        boolean success = false;
        String pluginName = pdos[0].getPluginName();

        if (pluginName != null) {
            success = RetrievalPersistUtil.routePlugin(generalDestinationUri,
                    pluginName, pdos);
        }

        return success;
    }
}
