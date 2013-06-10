package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import javax.xml.bind.JAXBException;

import net.opengis.wfs.v_1_1_0.FeatureCollectionType;

import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;

/**
 * 
 * Interface for creating WFS derived objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2013  753          dhladky    Initial javadoc
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IWfsMetaDataAdapter {
    
    public void configureMarshaller() throws JAXBException;
    
    public OgcJaxbManager getMarshaller();
    
    public FeatureCollectionType getFeatureCollection(String payload) throws JAXBException;

}
