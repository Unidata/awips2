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

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;

import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.datadelivery.registry.replication.RegistryReplicationManager;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.exception.NoReplicationServersAvailableException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;

/**
 * 
 * NCF specific implementation of the federation manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/22/2013    1707        bphillip    Initial implementation
 * 7/29/2013    2191        bphillip    Implemented registry sync for registries that have been down for an extended period of time
 * 10/30/2013   1538        bphillip    Override submitObjects method
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcfRegistryFederationManager extends RegistryFederationManager
        implements RegistryInitializedListener {

    /**
     * Creates a new NcfRegistryFederationManager
     */
    protected NcfRegistryFederationManager() {

    }

    /**
     * Creates a new NcfRegistryFederationManager
     * 
     * @param federationEnabled
     *            Boolean denoting if this registry is participating in the
     *            federation
     * @param lcm
     *            The lifecycle manager that will be used
     * @param federationPropertiesFileName
     *            The name of the file containing the registry properties
     * @throws JAXBException
     *             If errors occur while creating the JAXB manager
     * @throws SerializationException
     *             If errors occur while deserializing the federation properties
     */
    protected NcfRegistryFederationManager(boolean federationEnabled,
            LifecycleManager lcm, String federationPropertiesFileName,
            RegistryReplicationManager replicationManager)
            throws JAXBException, IOException, SerializationException {
        super(federationEnabled, lcm, federationPropertiesFileName,
                replicationManager);
    }

    @Override
    public void executeAfterRegistryInit() throws EbxmlRegistryException {
        if (federationEnabled) {
            List<RegistryObjectType> objects = new ArrayList<RegistryObjectType>(
                    5);
            FederationType federation = getFederation();
            RegistryType registry = federationProperties.createRegistryObject();
            OrganizationType org = federationProperties.createOrganization();
            PersonType primaryContact = federationProperties
                    .createPrimaryContactPerson();
            AssociationType federationAssociation = getFederationAssociation(
                    registry, getFederation());
            objects.add(federation);
            objects.add(registry);
            objects.add(org);
            objects.add(primaryContact);
            objects.add(federationAssociation);
            submitObjects(objects);
            replicationManager.submitRemoteSubscriptions(registry);
            try {
                replicationManager.checkDownTime();
            } catch (NoReplicationServersAvailableException e) {
                statusHandler
                        .warn("No replication servers have been specified!");
            } catch (Exception e) {
                throw new EbxmlRegistryException("Error checking down time!", e);
            }
        } else {
            statusHandler.info("Federation is disabled for this registry.");
        }
    }

    protected void submitObjects(List<RegistryObjectType> objects)
            throws EbxmlRegistryException {
        SubmitObjectsRequest submitObjectsRequest2 = new SubmitObjectsRequest(
                "Federation Objects submission",
                "Submitting federation related objects", null,
                new RegistryObjectListType(objects), false,
                Mode.CREATE_OR_REPLACE);
        try {
            lcm.submitObjects(submitObjectsRequest2);
        } catch (MsgRegistryException e) {
            throw new EbxmlRegistryException(
                    "Error submitting federation objects to registry", e);
        }
    }

    protected FederationType getFederation() throws EbxmlRegistryException {
        FederationType federation = new FederationType();
        federation.setId(FEDERATION_ID);
        federation.setLid(FEDERATION_ID);
        federation.setName(RegistryUtil
                .getInternationalString("NWS EBXML Registry Federation"));
        federation
                .setDescription(RegistryUtil
                        .getInternationalString("Federation object for NWS EBXML Registries"));
        federation.setOwner(RegistryUtil.DEFAULT_OWNER);
        federation.setStatus(StatusTypes.APPROVED);
        federation.setObjectType(RegistryObjectTypes.FEDERATION);
        federation.setReplicationSyncLatency(this.federationProperties
                .getFederationReplicationSyncLatency());
        return federation;
    }
}
