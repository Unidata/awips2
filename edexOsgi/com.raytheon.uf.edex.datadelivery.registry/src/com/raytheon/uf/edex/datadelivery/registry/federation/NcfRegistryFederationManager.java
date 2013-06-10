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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;

import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcfRegistryFederationManager extends RegistryFederationManager {

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
            LifecycleManager lcm, String federationPropertiesFileName)
            throws JAXBException, IOException, SerializationException {
        super(federationEnabled, lcm, federationPropertiesFileName);
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
            replicationManager.submitRemoteSubscriptions(registry.getBaseURL());
        } else {
            statusHandler.info("Federation is disabled for this registry.");
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
