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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.junit.Before;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Lists;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl.RETURN_TYPE;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Test {@link LifecycleManager} submit objects functionality.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2013 1914       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class AbstractRegistryTest {

    protected static final String MY_REGISTRY_OBJECT_ID = "myRegistryObjectId";

    protected static final String REGISTRY_OBJECT_TYPE = "myRegistryObjectType";

    @Autowired
    protected LifecycleManager lifecycleManager;

    protected QueryManager queryManager;

    @Before
    public void setUp() {
        this.queryManager = EDEXUtil.getESBComponent(QueryManager.class,
                "queryServiceImpl");
    }

    /**
     * Create the submit objects request.
     * 
     * @param registryObjectId
     *            the registry object id
     * @param mode
     * @return
     */
    protected SubmitObjectsRequest createSubmitObjectsRequest(
            String registryObjectId, String registryObjectType, Mode mode) {
        final RegistryObjectType registryObject = new RegistryObjectType();
        registryObject.setId(MY_REGISTRY_OBJECT_ID);
        registryObject.setLid(registryObject.getId());
        registryObject.setObjectType(registryObjectType);

        List<RegistryObjectType> registryObjects = Lists.newArrayList();
        registryObjects.add(registryObject);

        RegistryObjectListType registryObjectList = new RegistryObjectListType();
        registryObjectList.setRegistryObject(registryObjects);
        SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest();
        submitObjectsRequest.setCheckReferences(false);
        submitObjectsRequest.setComment("This is a comment.");
        submitObjectsRequest.setId("someId");
        submitObjectsRequest.setMode(mode);
        submitObjectsRequest.setRegistryObjectList(registryObjectList);

        return submitObjectsRequest;
    }

    protected QueryRequest createQueryForRegistryObjectByLid(
            String registryObjectId) {
        final ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                .createResponseOptionType();
        responseOption.setReturnType(RETURN_TYPE.RegistryObject.toString());
        responseOption.setReturnComposedObjects(false);

        final QueryType queryType = new QueryType();
        queryType
                .setQueryDefinition("urn:oasis:names:tc:ebxml-regrep:query:GetObjectsByLid");
        Set<SlotType> slots = new HashSet<SlotType>();
        final SlotType slot = new SlotType();
        slot.setName(QueryConstants.LID);
        final StringValueType slotValue = new StringValueType();
        slotValue.setStringValue(registryObjectId);
        slot.setSlotValue(slotValue);
        slots.add(slot);
        queryType.setSlot(slots);

        QueryRequest partQueryRequest = new QueryRequest();
        partQueryRequest.setResponseOption(responseOption);
        partQueryRequest.setFederated(false);
        partQueryRequest.setQuery(queryType);
        partQueryRequest.setMatchOlderVersions(true);
        partQueryRequest.setMaxResults(new BigInteger("9999"));
        return partQueryRequest;
    }

}
