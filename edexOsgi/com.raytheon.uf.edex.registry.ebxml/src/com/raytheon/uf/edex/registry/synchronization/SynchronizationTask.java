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
package com.raytheon.uf.edex.registry.synchronization;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.RegistryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;

/**
 * 
 * Process synchronization events
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2016 5386       tjensen     Initial creation
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
public class SynchronizationTask implements Runnable {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SynchronizationTask.class);

    /** Query used for synchronizing registries */
    private static final String SYNC_QUERY = "FROM RegistryObjectType obj where obj.id in (%s) order by obj.id asc";

    /** Batch size for registry synchronization queries */
    protected static final int SYNC_BATCH_SIZE = Integer.parseInt(System
            .getProperty("ebxml-notification-batch-size"));

    private RegistryObjectDao registryObjectDao;

    private RegistrySOAPServices soapService;

    String objectType;

    String syncUrl;

    private final TransactionTemplate txTemplate;

    private RegistryRESTServices restClient;

    public SynchronizationTask(String objectType, String syncUrl,
            TransactionTemplate txTemplate, RegistryRESTServices restClient,
            RegistrySOAPServices soapService,
            RegistryObjectDao registryObjectDao) {
        this.objectType = objectType;
        this.syncUrl = syncUrl;
        this.txTemplate = txTemplate;
        this.restClient = restClient;
        this.soapService = soapService;
        this.registryObjectDao = registryObjectDao;
    }

    /**
     * The process succeeded
     */
    public void success() {
        statusHandler.info("Synchronized objectType: " + objectType);
    }

    /**
     * The process failed
     */
    public void fail() {
        statusHandler.error("failed to synchronize objectType: " + objectType);
    }

    @Override
    public void run() {

        try {

            /*
             * Get the list of remote object ids so we can check later to ensure
             * all objects were retrieved
             */
            RestCollectionResponse<String> response = restClient
                    .getRegistryDataAccessService(syncUrl)
                    .getRegistryObjectIdsOfType(objectType);
            if (response.getPayload() == null) {
                statusHandler.info("0 objects of type [" + objectType
                        + "] present on remote registry. Skipping.");

            } else {
                List<String> remoteIds = new ArrayList<>(response.getPayload());

                statusHandler.info("Synchronizing " + remoteIds.size()
                        + " objects of type [" + objectType + "]");
                int batches = remoteIds.size() / SYNC_BATCH_SIZE;
                int remainder = remoteIds.size() % SYNC_BATCH_SIZE;

                for (int currentBatch = 0; currentBatch < batches; currentBatch++) {

                    statusHandler.info("Processing " + objectType + " batch "
                            + (currentBatch + 1) + "/" + batches);

                    persistBatch(objectType, syncUrl, remoteIds.subList(
                            currentBatch * SYNC_BATCH_SIZE, (currentBatch + 1)
                                    * SYNC_BATCH_SIZE));
                }
                // Grab any remaining
                if (remainder > 0) {
                    persistBatch(objectType, syncUrl, remoteIds.subList(batches
                            * SYNC_BATCH_SIZE, remoteIds.size()));
                }
            }

            success();

        } catch (Exception e) {
            fail();
        }
    }

    /**
     * Persist the batch of objects being processed
     * 
     * @param myObjectType
     * @param remoteRegistryUrl
     * @param batch
     * @throws Exception
     */
    private void persistBatch(String myObjectType, String remoteRegistryUrl,
            List<String> batch) throws Exception {

        final String fobjectType = myObjectType;
        final String fremoteRegistryUrl = remoteRegistryUrl;
        final List<String> fbatch = batch;

        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            protected void doInTransactionWithoutResult(TransactionStatus status) {
                /*
                 * Average length of ids of registry object is 52. Add 3 for
                 * quotes and comma
                 */
                StringBuilder builder = new StringBuilder(55 * fbatch.size());
                for (int i = 0; i < fbatch.size(); i++) {
                    builder.append("'").append(fbatch.get(i)).append("'");
                    if (i != fbatch.size() - 1) {
                        builder.append(",");
                    }
                }

                SlotType queryLanguageSlot = new SlotType(
                        QueryConstants.QUERY_LANGUAGE, new StringValueType(
                                QueryLanguages.HQL));
                SlotType queryExpressionSlot = new SlotType(
                        QueryConstants.QUERY_EXPRESSION,
                        new StringValueType(""));
                QueryRequest queryRequest = new QueryRequest();
                QueryType query = new QueryType();
                query.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);
                query.getSlot().add(queryLanguageSlot);
                query.getSlot().add(queryExpressionSlot);
                queryRequest.setQuery(query);
                queryRequest.setResponseOption(new ResponseOptionType(
                        QueryReturnTypes.REGISTRY_OBJECT, true));
                queryRequest.setId("Synchronizing object type: " + fobjectType);
                StringValueType queryValue = new StringValueType(String.format(
                        SYNC_QUERY, builder.toString()));
                queryExpressionSlot.setSlotValue(queryValue);
                // query the remote registry
                QueryResponse queryResponse = null;
                try {
                    queryResponse = soapService.getQueryServiceForHost(
                            fremoteRegistryUrl).executeQuery(queryRequest);
                } catch (MsgRegistryException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Query to remote registry for batch has failed.",
                                    e);
                }
                if (queryResponse != null) {
                    List<RegistryObjectType> queryResult = queryResponse
                            .getRegistryObjects();
                    if (!CollectionUtil.isNullOrEmpty(queryResult)) {
                        registryObjectDao.persistAll(queryResult);
                        registryObjectDao.flushAndClearSession();
                    }
                }
            }
        });
    }
}
