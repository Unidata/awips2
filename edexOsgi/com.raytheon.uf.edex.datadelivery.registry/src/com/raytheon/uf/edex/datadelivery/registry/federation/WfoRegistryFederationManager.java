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
package com.raytheon.uf.edex.datadelivery.registry.federation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.RunnableWithTransaction;
import com.raytheon.uf.edex.datadelivery.registry.replication.RegistryReplicationManager;
import com.raytheon.uf.edex.datadelivery.registry.web.DataDeliveryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * 
 * WFO specific implementation of the federation manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/22/2013    1707        bphillip    Initial implementation
 * 7/29/2013    2191        bphillip    Implemented registry sync for registries that have been down for an extended period of time
 * 10/20/2013   1682        bphillip    Fixed query invocation
 * 10/30/2013   1538        bphillip    This class now uses non-static rest/soap clients
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Service
@Transactional
public class WfoRegistryFederationManager extends RegistryFederationManager
        implements RegistryInitializedListener {

    /** The transaction template used to manually handle transactions */
    private TransactionTemplate txTemplate;

    /** Data Delivery rest client services */
    private DataDeliveryRESTServices dataDeliveryRestClient;

    /**
     * Creates a new WfoRegistryFederationManager
     */
    protected WfoRegistryFederationManager() {

    }

    /**
     * Creates a new WfoRegistryFederationManager
     * 
     * @param federationEnabled
     *            Boolean denoting if this registry is participating in the
     *            federation
     * @param lcm
     *            The lifecycle manager that will be used
     * @param federationPropertiesFileName
     *            The name of the file containing the registry properties
     * @param ncfAddress
     *            The address of the NCF
     * @throws JAXBException
     *             If errors occur while creating the JAXB manager
     * @throws SerializationException
     *             If errors occur while deserializing the federation properties
     */
    protected WfoRegistryFederationManager(boolean federationEnabled,
            LifecycleManager lcm, String federationPropertiesFileName,
            RegistryReplicationManager replicationManager)
            throws JAXBException, IOException, SerializationException {
        super(federationEnabled, lcm, federationPropertiesFileName,
                replicationManager);
        if (this.replicationManager.getServers() == null
                || CollectionUtil.isNullOrEmpty(replicationManager.getServers()
                        .getRegistryReplicationServers())) {
            statusHandler
                    .warn("No servers configured for replication.  Federation functionality is disabled");
            this.federationEnabled = false;
            this.replicationManager.setSubscriptionProcessingEnabled(false);
        } else {
            scheduler = Executors.newSingleThreadScheduledExecutor();

        }
    }

    @Override
    public void executeAfterRegistryInit() throws EbxmlRegistryException {
        if (federationEnabled) {
            final RegisterWithFederationTask federationRegistrationTask = new RegisterWithFederationTask();
            final ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(
                    federationRegistrationTask, 0, 10, TimeUnit.SECONDS);
            scheduler.schedule(new Runnable() {
                @Override
                public void run() {
                    if (federationRegistrationTask.success) {
                        statusHandler
                                .info("Federation registration successful. Cancelling future registration attempts.");
                        future.cancel(false);
                    }

                }
            }, 5, TimeUnit.SECONDS);
        } else {
            statusHandler.info("Federation is disabled for this registry.");
        }
    }

    /**
     * Registers this registry with federation
     * 
     * @throws EbxmlRegistryException
     *             If errors occur while registering this registry with the
     *             federation
     */
    private void registerWithFederation() throws EbxmlRegistryException {
        List<RegistryObjectType> objects = new ArrayList<RegistryObjectType>(5);
        RegistryType registry = federationProperties.createRegistryObject();
        OrganizationType org = federationProperties.createOrganization();
        PersonType primaryContact = federationProperties
                .createPrimaryContactPerson();
        FederationType federation = getFederation();
        AssociationType federationAssociation = null;
        if (federation == null) {
            statusHandler
                    .error("Unable to locate Federation Object! Registry is unable to join the federation at this time.");
        } else {
            federationAssociation = getFederationAssociation(registry,
                    federation);
        }
        objects.add(registry);
        objects.add(org);
        objects.add(primaryContact);
        objects.add(federationAssociation);
        submitObjects(objects);
        replicationManager.setSubscriptionProcessingEnabled(true);
        replicationManager.submitRemoteSubscriptions(registry);
    }

    protected FederationType getFederation() throws EbxmlRegistryException {
        statusHandler
                .info("Attempting to acquire federation object from NCF...");
        QueryType query = new QueryType();
        query.setQueryDefinition(CanonicalQueryTypes.GET_OBJECT_BY_ID);
        query.getSlot().add(
                new SlotType(QueryConstants.ID, new StringValueType(
                        FEDERATION_ID)));
        QueryRequest request = new QueryRequest();
        request.setResponseOption(new ResponseOptionType(
                ResponseOptionType.RETURN_TYPE.RegistryObject.toString(), true));
        request.setId("Query For Federation");
        request.setQuery(query);
        QueryResponse response = null;
        try {
            response = registrySoapServices.getQueryServiceForHost(ncfAddress)
                    .executeQuery(request);
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error getting Federation from NCF!", e);
        }
        if (response.getRegistryObjectList() == null
                || response.getRegistryObjectList().getRegistryObject()
                        .isEmpty()) {
            throw new EbxmlRegistryException("Federation not found at NCF!");
        } else {
            List<RegistryObjectType> responseObjects = response
                    .getRegistryObjectList().getRegistryObject();
            return (FederationType) responseObjects.get(0);
        }
    }

    /**
     * 
     * Runnable task that continuously attempts to register this registry with
     * the federation until it succeeds
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#     Engineer    Description
     * ------------ ----------  ----------- --------------------------
     * 5/22/2013    1707        bphillip    Initial implementation
     * </pre>
     * 
     * @author bphillip
     * @version 1
     */
    private class RegisterWithFederationTask extends RunnableWithTransaction {

        /**
         * Denotes if this task has successfully registered this registry with
         * the federation
         */
        private boolean success = false;

        /**
         * Creates a new RegisterwithFederationTask
         */
        public RegisterWithFederationTask() {
            super(txTemplate);

        }

        @Override
        public void runWithTransaction() {
            if (!success) {
                try {
                    try {
                        if (dataDeliveryRestClient
                                .isRegistryAvailable(ncfAddress)) {
                            statusHandler
                                    .info("NCF Registry is available. Attempting to join federation...");
                        } else {
                            statusHandler
                                    .error("NCF is currently unreachable. Local registry is unable to join the federation at this time.  Retrying in 10 seconds...");
                            replicationManager
                                    .setSubscriptionProcessingEnabled(false);
                            success = false;
                            return;
                        }
                        registerWithFederation();
                        replicationManager.checkDownTime();
                        success = true;
                    } catch (EbxmlRegistryException e) {
                        statusHandler.error(
                                "Error registering with federation", e);
                        success = false;
                    }
                } catch (Throwable e) {
                    throw new RuntimeException(
                            "Error initializing EBXML database!", e);
                }
            }
        }
    }

    public void setTxTemplate(TransactionTemplate txTemplate) {
        this.txTemplate = txTemplate;
    }

    public void setDataDeliveryRestClient(
            DataDeliveryRESTServices dataDeliveryRestClient) {
        this.dataDeliveryRestClient = dataDeliveryRestClient;
    }

}
