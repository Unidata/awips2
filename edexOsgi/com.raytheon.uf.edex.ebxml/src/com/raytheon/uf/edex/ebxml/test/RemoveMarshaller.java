/**
 * 
 */
package com.raytheon.uf.edex.ebxml.test;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.edex.ebxml.util.EbxmlJaxbManager;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

/**
 * @author jsherida
 *
 */
public class RemoveMarshaller {

    /**
     * @param args none
     */
    public static void main(String[] args) {
        oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory lcmFactory = 
            new oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory();
        oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = 
            new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();
        
        // Construct an example message
        RemoveObjectsRequest request = lcmFactory.createRemoveObjectsRequest();
        request.setId("123");
        request.setComment("Test RemoveObjects");
        request.setDeleteChildren(true);
        request.setDeletionScope("DeleteRepositoryItemOnly");

        // List ID #2
        ObjectRefType ref = rimFactory.createObjectRefType();
        ref.setId("serviceUID0002");
        
        ObjectRefListType refList = rimFactory.createObjectRefListType();
        refList.getObjectRef().add(ref);
        
        request.setObjectRefList(refList);
        
        // Query for "metar" (ID #1)
        QueryType query = rimFactory.createQueryType();
        query.setQueryDefinition("urn:oasis:names:tc:ebxml-regrep:query:BasicQuery");
        
        SlotType descSlot = rimFactory.createSlotType();
        descSlot.setName("description");
        StringValueType descValue = rimFactory.createStringValueType();
        descValue.setValue("metar");
        descSlot.setSlotValue(descValue);
        
        query.getSlot().add(descSlot);
        
        request.setQuery(query);
        
        
        EbxmlJaxbManager manager = EbxmlJaxbManager.getInstance();
        try {
            // Marshal it
            String requestStr = manager.marshal(request);
            // Print it
            System.out.println(requestStr);
        } catch (JAXBException e) {
            // Or, print error
            e.printStackTrace();
        }

    }

}
