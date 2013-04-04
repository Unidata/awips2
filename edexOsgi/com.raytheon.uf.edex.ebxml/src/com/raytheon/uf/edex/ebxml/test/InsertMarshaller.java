/**
 * 
 */
package com.raytheon.uf.edex.ebxml.test;

import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import com.raytheon.uf.edex.ebxml.util.EbxmlJaxbManager;
import com.raytheon.uf.edex.ebxml.util.EbxmlUtil;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DurationValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

/**
 * @author jsherida
 */
public class InsertMarshaller {

    /**
     * @param args none
     */
    public static void main(String[] args) {
        oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory lcmFactory = 
            new oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory();
        oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = 
            new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();
        
        // Construct an example message
        /* Based on solr schema:
 <fields>
   <field name="id" type="string" indexed="true" stored="true" required="true" /> 
   <field name="location" type="location" indexed="true" stored="true"/>
   <field name="ul_location" type="location" indexed="true" stored="true"/>
   <field name="lr_location" type="location" indexed="true" stored="true"/>
   <field name="name" type="text" indexed="true" stored="true"/>
   <field name="description" type="text" indexed="true" stored="true"/>
   <!-- catchall field, containing all other searchable text fields (implemented
        via copyField further on in this schema  -->
   <field name="text" type="text" indexed="true" stored="false" multiValued="true"/>
 </fields>
         */
        SubmitObjectsRequest request = lcmFactory.createSubmitObjectsRequest();
        request.setId("123");
        request.setComment("Test SubmitObjects");
        request.setMode(Mode.CREATE_OR_REPLACE);
        
        // Metar registry object
        RegistryObjectType metarRegObj = rimFactory.createRegistryObjectType();
        metarRegObj.setId("serviceUID0001");
        metarRegObj.setLid("serviceUID0001");
        metarRegObj.setName(EbxmlUtil.getIntlString("National Weather Service Metar"));
        metarRegObj.setDescription(EbxmlUtil.getIntlString(
                "Metar point data from the National Weather Service (NWS)."));
        metarRegObj.setObjectType("MetarService");
        
        ClassificationType pointClass = rimFactory.createClassificationType();
        pointClass.setClassificationNode("point");
        ClassificationType metarClass = rimFactory.createClassificationType();
        metarClass.setClassificationNode("metar");
        ClassificationType wfsClass = rimFactory.createClassificationType();
        wfsClass.setClassificationNode("wfs");
        metarRegObj.getClassification().add(pointClass);
        metarRegObj.getClassification().add(metarClass);
        metarRegObj.getClassification().add(wfsClass);
        
        SlotType locationSlot = rimFactory.createSlotType();
        locationSlot.setName("location");
        StringValueType locationValue = rimFactory.createStringValueType();
        locationValue.setValue("35.2452,-122.3845");
        locationSlot.setSlotValue(locationValue);

        SlotType metarLinkSlot = rimFactory.createSlotType();
        metarLinkSlot.setName("link");
        StringValueType metarLinkValue = rimFactory.createStringValueType();
        metarLinkValue.setValue("http://metarland.org:8080/wfs");
        metarLinkSlot.setSlotValue(metarLinkValue);

        metarRegObj.getSlot().add(locationSlot);
        metarRegObj.getSlot().add(metarLinkSlot);
        
        // Satellite registry object
        RegistryObjectType satelliteRegObj = rimFactory.createRegistryObjectType();
        satelliteRegObj.setId("serviceUID0002");
        satelliteRegObj.setLid("serviceUID0002");
        satelliteRegObj.setName(EbxmlUtil.getIntlString("NASA GOES Imagery"));
        satelliteRegObj.setDescription(EbxmlUtil.getIntlString(
                "Satellite raster data from NASA. Includes water vapor, infrared, and visible images."));
        satelliteRegObj.setObjectType("SatelliteService");

        ClassificationType rasterClass = rimFactory.createClassificationType();
        rasterClass.setClassificationNode("raster");
        ClassificationType satelliteClass = rimFactory.createClassificationType();
        satelliteClass.setClassificationNode("satellite");
        ClassificationType wcsClass = rimFactory.createClassificationType();
        wcsClass.setClassificationNode("wfs");
        satelliteRegObj.getClassification().add(rasterClass);
        satelliteRegObj.getClassification().add(satelliteClass);
        satelliteRegObj.getClassification().add(wcsClass);
        
        SlotType ulLocationSlot = rimFactory.createSlotType();
        ulLocationSlot.setName("location"); //"ulLocation");
        StringValueType ulValue = rimFactory.createStringValueType();
        ulValue.setValue("45.00,-122.00");
        ulLocationSlot.setSlotValue(ulValue);

//        SlotType lrLocationSlot = rimFactory.createSlotType();
//        lrLocationSlot.setName("lrLocation");
//        StringValueType lrValue = rimFactory.createStringValueType();
//        lrValue.setValue("20.00,-100.00");
//        lrLocationSlot.setSlotValue(lrValue);
        
        SlotType satLinkSlot = rimFactory.createSlotType();
        satLinkSlot.setName("link");
        StringValueType satLinkValue = rimFactory.createStringValueType();
        satLinkValue.setValue("http://satellicious.com:8080/wcs");
        satLinkSlot.setSlotValue(satLinkValue);
        
        // Testing output:
        SlotType timePeriodSlot = rimFactory.createSlotType();
        timePeriodSlot.setName("timePeriod");
        DurationValueType timePeriodValue = rimFactory.createDurationValueType();
        try {
            timePeriodValue.setValue(DatatypeFactory.newInstance().newDuration(30000));
        } catch (DatatypeConfigurationException e1) {
            e1.printStackTrace();
        }
        timePeriodSlot.setSlotValue(timePeriodValue);

        // Testing output:
        SlotType dateSlot = rimFactory.createSlotType();
        dateSlot.setName("timePeriod");
        DateTimeValueType dateValue = rimFactory.createDateTimeValueType();
        try {
            dateValue.setValue(DatatypeFactory.newInstance().newXMLGregorianCalendar());
        } catch (DatatypeConfigurationException e1) {
            e1.printStackTrace();
        }
        dateSlot.setSlotValue(dateValue);
        
        // Testing output:
        SlotType collSlot = rimFactory.createSlotType();
        collSlot.setName("timePeriod");
        CollectionValueType collValue = rimFactory.createCollectionValueType();
        collValue.setCollectionType("StringValueType");
        collValue.getElement().add(satLinkValue);
        collSlot.setSlotValue(collValue);
        
        satelliteRegObj.getSlot().add(ulLocationSlot);
//        satelliteRegObj.getSlot().add(lrLocationSlot);
        satelliteRegObj.getSlot().add(satLinkSlot);
        satelliteRegObj.getSlot().add(timePeriodSlot);
        satelliteRegObj.getSlot().add(dateSlot);
        satelliteRegObj.getSlot().add(collSlot);
        
        // Fill list
        RegistryObjectListType regObjList = rimFactory.createRegistryObjectListType();
        List<RegistryObjectType> objects = regObjList.getRegistryObject();
        objects.add(metarRegObj);
        objects.add(satelliteRegObj);
        
        request.setRegistryObjectList(regObjList);
        
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
