package com.raytheon.uf.viz.datadelivery.notification.xml;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
import com.raytheon.uf.viz.datadelivery.subscription.xml.SubscriptionManagerConfigXML;

public class TestXMLDriver {
    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;
    
    private String path = "/tmp/NotificationConfig.xml";

    public TestXMLDriver() {
        createContext();
    }
    
    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes =
                new Class[] { 
                NotificationConfigXML.class,
                MessageLoadXML.class,
                PrioritySettingXML.class
        };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();
        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
        }
    }

    private void setXml(NotificationConfigXML xml) {
        File file = new File(path);
        try {
            marshaller.marshal(xml, file);
            System.out.println(path + " Saved");
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }

    private void readXML() {
        File file = new File(path);
        if (file.exists()) {
            try {
                SubscriptionManagerConfigXML xml = (SubscriptionManagerConfigXML) unmarshaller.unmarshal(file);
                
            } catch (JAXBException e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        TestXMLDriver driver = new TestXMLDriver();
        NotificationConfigXML xml = new NotificationConfigXML();
        ColumnXML column = new ColumnXML();
        column.setName("Column Name 1");
        column.setSortAsc(true);
        column.setSortColumn(true);
        column.setVisible(true);
        xml.addColumn(column);
        
        column = new ColumnXML();
        column.setName("Column Name 2");
        column.setSortColumn(false);
        column.setVisible(true);
        xml.addColumn(column);
        
        MessageLoadXML messageLoad = new MessageLoadXML();
        messageLoad.setLoadAllMessages(true);
        xml.setMessageLoad(messageLoad);
        
        PrioritySettingXML prioritySetting = new PrioritySettingXML();
        prioritySetting.setColorNumName(true);
        xml.setPrioritySetting(prioritySetting);
        
        driver.setXml(xml);

    }
}
