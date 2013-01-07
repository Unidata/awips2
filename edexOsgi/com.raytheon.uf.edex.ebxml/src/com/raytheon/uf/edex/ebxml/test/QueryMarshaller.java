/**
 * 
 */
package com.raytheon.uf.edex.ebxml.test;

import java.math.BigInteger;
import java.util.Collection;

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.edex.ebxml.util.EbxmlJaxbManager;

/**
 * @author jsherida
 */
public class QueryMarshaller {

    /**
     * @param args
     *            none
     */
    public static void main(String[] args) {
        oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory queryFactory = new oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory();
        oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();

        // Construct an example query
        QueryRequest request = queryFactory.createQueryRequest();
        request.setId("123");
        request.setComment("Test Request");
        request.setStartIndex(new BigInteger("0"));
        request.setMaxResults(new BigInteger("-1"));

        ResponseOptionType responseOption = queryFactory
                .createResponseOptionType();
        responseOption.setReturnComposedObjects(Boolean.TRUE);
        responseOption.setReturnType("RegistryObject");
        request.setResponseOption(responseOption);

        QueryType queryType = rimFactory.createQueryType();
        queryType
                .setQueryDefinition("urn:oasis:names:tc:ebxml-regrep:query:BasicQuery");

        SlotType descSlot = rimFactory.createSlotType();
        descSlot.setName("description");
        StringValueType descValue = rimFactory.createStringValueType();
        descValue.setValue("satellite");
        descSlot.setSlotValue(descValue);

        SlotType nameSlot = rimFactory.createSlotType();
        nameSlot.setName("name");
        StringValueType nameValue = rimFactory.createStringValueType();
        nameValue.setValue("GOES");
        nameSlot.setSlotValue(nameValue);

        SlotType expressionSlot = rimFactory.createSlotType();
        expressionSlot.setName("queryExpression");
        StringValueType expressionValue = rimFactory.createStringValueType();
        expressionValue.setValue("*:*");
        expressionSlot.setSlotValue(expressionValue);

        SlotType languageSlot = rimFactory.createSlotType();
        languageSlot.setName("queryLanguage");
        StringValueType languageValue = rimFactory.createStringValueType();
        languageValue.setValue("solr");
        languageSlot.setSlotValue(languageValue);

        Collection<SlotType> slots = queryType.getSlot();
        slots.add(descSlot);
        slots.add(nameSlot);
        slots.add(expressionSlot);
        slots.add(languageSlot);

        request.setQuery(queryType);

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
