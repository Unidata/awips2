package com.raytheon.uf.edex.datadelivery.retrieval.wfs.metadata;

import javax.xml.bind.JAXBException;

import net.opengis.wfs.v_1_1_0.FeatureCollectionType;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters.AbstractMetadataAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.wfs.IWfsMetaDataAdapter;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;
import com.raytheon.uf.edex.plugin.madis.MadisPointDataTransform;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.MadisObjectFactory;
import com.raytheon.uf.edex.wfs.reg.Unique;

/**
 * 
 * Convert RetrievalAttribute to MadisRecords.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013  753       dhladky      Initial javadoc
 * May 31, 2013 2038       djohnson     Move to correct git repo.
 * June 03, 2012 1763      dhladky      Made operational, moved to ogc plugin
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisMetadataAdapter extends AbstractMetadataAdapter<Madis>
        implements IWfsMetaDataAdapter {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisMetadataAdapter.class);

    private OgcJaxbManager marshaller = null;

    @Override
    public PluginDataObject getRecord(Madis madis) {
        return madis.getRecord();
    }

    @Override
    public void allocatePdoArray(int size) {
        pdos = new MadisRecord[size];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processAttributeXml(RetrievalAttribute attXML)
            throws InstantiationException {
        this.attXML = attXML;
    }

    public void configureMarshaller() throws JAXBException {
        if (marshaller == null) {
            Class<?>[] classes = new Class<?>[] {
                    net.opengis.gml.v_3_1_1.ObjectFactory.class,
                    net.opengis.wfs.v_1_1_0.ObjectFactory.class,
                    net.opengis.filter.v_1_1_0.ObjectFactory.class,
                    Unique.class, Madis.class, MadisObjectFactory.class };

            try {
                marshaller = new OgcJaxbManager(classes);
            } catch (JAXBException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        e1.getLocalizedMessage(), e1);
            }
        }
    }

    public OgcJaxbManager getMarshaller() {
        return marshaller;
    }

    @Override
    public FeatureCollectionType getFeatureCollection(String payload)
            throws JAXBException {
        // Madis is pointData
        setIsPointData(true);
        
        if (marshaller == null) {
            configureMarshaller();
        }

        return (FeatureCollectionType) getMarshaller().unmarshal(payload);
    }
    
    /**
     * populate pointdata views
     * @param pdos
     * @return
     * @throws PluginException
     */
    public PluginDataObject[] setPointData(PluginDataObject[] pdos) throws PluginException {
       
        MadisPointDataTransform mpdt = new MadisPointDataTransform();
        mpdt.toPointData(pdos);
        return pdos;
    }
    
}
