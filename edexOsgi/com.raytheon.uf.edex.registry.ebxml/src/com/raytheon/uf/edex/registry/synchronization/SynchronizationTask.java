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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.RegistryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

/**
 * Process synchronization events
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------
 * Apr 29, 2016  5386     tjensen   Initial creation
 * Jun 20, 2017  6186     rjpeter   Fixed batch numbering.
 * Aug 02, 2017  6186     rjpeter   Cache soap service lookup and add retry.
 *
 * </pre>
 *
 * @author tjensen
 */
public class SynchronizationTask implements Runnable {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    /** Query used for synchronizing registries */
    private static final String SYNC_QUERY = "FROM RegistryObjectType obj where obj.id in (%s) order by obj.id asc";

    /** Batch size for registry synchronization queries */
    protected static final int SYNC_BATCH_SIZE = Integer
            .parseInt(System.getProperty("ebxml-notification-batch-size"));

    private final RegistryObjectDao registryObjectDao;

    private final RegistrySOAPServices soapService;

    private final String objectType;

    private final String syncUrl;

    private final TransactionTemplate txTemplate;

    private final RegistryRESTServices restClient;

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
                logger.info("0 objects of type [" + objectType
                        + "] present on remote registry. Skipping.");

            } else {
                QueryManager queryManager = soapService
                        .getQueryServiceForHost(syncUrl);

                List<String> remoteIds = new ArrayList<>(response.getPayload());

                logger.info("Synchronizing " + remoteIds.size()
                        + " objects of type [" + objectType + "] from ["
                        + syncUrl + "]");
                int batches = (int) Math
                        .ceil(((float) remoteIds.size()) / SYNC_BATCH_SIZE);
                int sIndex = 0;
                int tries = 0;
                boolean retry = false;
                for (int currentBatch = 1; currentBatch <= batches; currentBatch++) {
                    tries = 0;
                    logger.info("Processing " + objectType + " batch "
                            + currentBatch + "/" + batches);
                    int eIndex = sIndex + SYNC_BATCH_SIZE;
                    if (eIndex > remoteIds.size()) {
                        eIndex = remoteIds.size();
                    }

                    do {
                        retry = false;
                        try {
                            persistBatch(objectType, queryManager,
                                    remoteIds.subList(sIndex, eIndex));
                        } catch (Exception e) {
                            if (tries < 1) {
                                logger.error(
                                        "Error occurred synchronizing batch for objectType ["
                                                + objectType + "], retrying...",
                                        e);
                                tries++;
                                retry = true;
                            } else {
                                logger.error(
                                        "Error occurred synchronizing batch for objectType ["
                                                + objectType
                                                + "], skipping batch...",
                                        e);
                            }
                        }
                    } while (retry);
                    sIndex = eIndex;
                }
            }

            logger.info("Successfully synchronized objectType: " + objectType);
        } catch (Exception e) {
            logger.error("Failed to synchronize objectType: " + objectType, e);
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
    private void persistBatch(final String myObjectType,
            final QueryManager queryManager, final List<String> batch)
            throws Exception {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            protected void doInTransactionWithoutResult(
                    TransactionStatus status) {
                /*
                 * Average length of ids of registry object is 52. Add 3 for
                 * quotes and comma
                 */
                StringBuilder builder = new StringBuilder(55 * batch.size());
                for (int i = 0; i < batch.size(); i++) {
                    if (i != 0) {
                        builder.append(',');
                    }

                    builder.append('\'').append(batch.get(i)).append('\'');
                }

                SlotType queryLanguageSlot = new SlotType(
                        QueryConstants.QUERY_LANGUAGE,
                        new StringValueType(QueryLanguages.HQL));
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
                queryRequest.setId("Synchronizing object type: " + objectType);
                StringValueType queryValue = new StringValueType(
                        String.format(SYNC_QUERY, builder.toString()));
                queryExpressionSlot.setSlotValue(queryValue);
                // query the remote registry
                QueryResponse queryResponse = null;
                try {
                    queryResponse = queryManager.executeQuery(queryRequest);
                } catch (MsgRegistryException e) {
                    logger.error(
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
