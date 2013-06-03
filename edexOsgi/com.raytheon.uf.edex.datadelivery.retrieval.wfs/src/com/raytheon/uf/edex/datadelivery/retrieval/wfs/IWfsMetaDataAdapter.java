package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import javax.xml.bind.JAXBException;

import net.opengis.wfs.v_1_1_0.FeatureCollectionType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

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
 * June 11, 2013 1763        dhladky     Moved and updated.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IWfsMetaDataAdapter {

    public FeatureCollectionType getFeatureCollection(String payload) throws JAXBException;
    
    public PluginDataObject[] setPointData(PluginDataObject[] pdos) throws PluginException;

}
