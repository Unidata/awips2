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

import java.io.File;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.registry.replication.RegistryReplicationManager;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Abstract implementation of the federation manager. This class is responsible
 * for managing this registry's interactions with the registry federation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/22/2013    1707        bphillip    Initial implementation
 * 7/29/2013    2191        bphillip    Implemented registry sync for registries that have been down for an extended period of time
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Service
@Transactional
public abstract class RegistryFederationManager {

    /** The logger instance */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryFederationManager.class);

    /**
     * The scheduler service used for registering this registry with the
     * federation
     */
    protected ScheduledExecutorService scheduler;

    /** The federation identifier */
    public static final String FEDERATION_ID = "Registry Federation";

    /** The registry replication manager */
    protected RegistryReplicationManager replicationManager;

    /** Denotes if this registry is participating in the federation */
    protected boolean federationEnabled;

    /** The lifecycle manager */
    protected LifecycleManager lcm;

    /** The JAXB Manager for serializing registry objects */
    protected JAXBManager jaxbManager;

    /** The properties describing this registry in the federation */
    protected FederationProperties federationProperties;

    /** Data access object for registry objects */
    protected RegistryObjectDao registryObjectDao;

    /** Data Access object for RegistryType objects */
    protected RegistryDao registryDao;

    /**
     * Gets the federation object for this federation
     * 
     * @return The federation object
     * @throws EbxmlRegistryException
     *             If errors occur getting the federatino
     */
    protected abstract FederationType getFederation()
            throws EbxmlRegistryException;

    /**
     * Creates a new RegistryFederationManager
     */
    protected RegistryFederationManager() {

    }

    /**
     * Creates a new RegistryFederationManager
     * 
     * @param federationEnabled
     *            Boolean denoting if the federation is enabled
     * @param lcm
     *            The lifecycle manager to be used
     * @param federationPropertiesFileName
     *            The name of the file containing the properties for this
     *            registry
     * @throws JAXBException
     *             If errors occur when creating the jaxb manager
     * @throws SerializationException
     *             If errors occur when unmarshalling the federation properties
     */
    protected RegistryFederationManager(boolean federationEnabled,
            LifecycleManager lcm, String federationPropertiesFileName,
            RegistryReplicationManager replicationManager)
            throws JAXBException, SerializationException {
        this.federationEnabled = federationEnabled;
        this.lcm = lcm;
        this.replicationManager = replicationManager;
        jaxbManager = new JAXBManager(SubmitObjectsRequest.class,
                FederationProperties.class);
        File federationPropertiesFile = PathManagerFactory.getPathManager()
                .getStaticFile(federationPropertiesFileName);
        if (federationEnabled) {
            if (federationPropertiesFile == null) {
                statusHandler
                        .warn("Unable to locate federation configuration file: "
                                + federationPropertiesFileName
                                + ". Federation functionality is disabled");
                this.federationEnabled = false;
            } else {
                federationProperties = (FederationProperties) jaxbManager
                        .jaxbUnmarshalFromXmlFile(federationPropertiesFile);
            }
            if (this.replicationManager.getServers() == null
                    || CollectionUtil.isNullOrEmpty(replicationManager
                            .getServers().getRegistryReplicationServers())) {
                statusHandler
                        .warn("No servers configured for replication.  Federation functionality is disabled");
                this.federationEnabled = false;
                this.replicationManager.setSubscriptionProcessingEnabled(false);
            }
        }

    }

    /**
     * Creates the association object between this registry and the federation
     * 
     * @param registry
     *            The registry joining the federation
     * @param federation
     *            The federation the registry is joining
     * @return The association object
     */
    protected AssociationType getFederationAssociation(RegistryType registry,
            FederationType federation) {
        AssociationType association = new AssociationType();
        association.setId(registry.getId()
                + " Federation Membership Association");
        association.setLid(association.getId());
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setOwner(federationProperties.getSiteIdentifier());
        association.setType(AssociationTypes.HAS_FEDERATION_MEMBER);
        association.setStatus(StatusTypes.APPROVED);
        association.setName(RegistryUtil.getInternationalString(registry
                .getId() + " Federation Membership"));
        association.setDescription(association.getName());
        association.setTargetObject(registry.getId());
        association.setSourceObject(federation.getId());
        return association;
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

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

}
