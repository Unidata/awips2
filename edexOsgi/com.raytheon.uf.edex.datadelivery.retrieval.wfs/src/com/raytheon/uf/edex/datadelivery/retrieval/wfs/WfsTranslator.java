package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;
import net.opengis.gml.v_3_1_1.FeaturePropertyType;
import net.opengis.wfs.v_1_1_0.FeatureCollectionType;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalTranslator;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.MadisObjectFactory;
import com.raytheon.uf.edex.wfs.reg.Unique;

/**
 * 
 * Translate WFS retrievals into PDOs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013  753       dhladky      Initial javadoc
 * May 31, 2013 2038       djohnson     Move to correct repo.
 * May 31, 2013 1763       dhladky      Fixed up merge.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */

public class WfsTranslator extends RetrievalTranslator {

    private static final IUFStatusHandler statusHandler = UFStatus
    .getHandler(WfsTranslator.class);
    
    WfsTranslator(RetrievalAttribute attXML, String className)
            throws InstantiationException {
 
        super(attXML, className);
    }

    public WfsTranslator(RetrievalAttribute attXML) throws InstantiationException {
        super(attXML);
    }

    @Override
    protected int getSubsetNumTimes() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    protected int getSubsetNumLevels() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    protected List<DataTime> getTimes() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * XML string into abstract features then to PDOs
     * @param payload
     * @return
     */
    public PluginDataObject[] asPluginDataObjects(String payload) {
        
        try {
            FeatureCollectionType featureCollection = ((IWfsMetaDataAdapter)metadataAdapter).getFeatureCollection(payload);
            if (featureCollection.getNumberOfFeatures().intValue() > 0) {
                int i = 0;
                metadataAdapter.allocatePdoArray(featureCollection.getNumberOfFeatures().intValue());
                for (FeaturePropertyType type : featureCollection.getFeatureMember()) {
                    JAXBElement<? extends AbstractFeatureType> feature = type.getFeature();
                    metadataAdapter.getPdos()[i] = metadataAdapter.getRecord(feature);
                    i++;
                }
            }
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, "Can't de-serialize FeatureCollection", e);
        }
        
        return metadataAdapter.getPdos();
    }
    
    
    public static void main(String[] args) {
        
        String fileName = args[0];
        OgcJaxbManager ctx = null;
        Class<?>[] classes = new Class<?>[] {
                net.opengis.gml.v_3_1_1.ObjectFactory.class,
                net.opengis.wfs.v_1_1_0.ObjectFactory.class,
                net.opengis.filter.v_1_1_0.ObjectFactory.class, 
                Unique.class, 
                Madis.class, 
                MadisObjectFactory.class};

        try {
            ctx = new OgcJaxbManager(classes);
        } catch (JAXBException e1) {
            e1.printStackTrace();
            statusHandler.handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);
        }
        
        try {
            
            try {
                File file = new File(fileName);
                FileReader reader = new FileReader(file);
                List<MadisRecord> featureList = new ArrayList<MadisRecord>();
                FeatureCollectionType features = (FeatureCollectionType) ctx.unmarshal(reader);
                for (FeaturePropertyType type: features.getFeatureMember()) {
                     Madis feature = (Madis)type.getFeature().getValue();
                     MadisRecord record = feature.getRecord();
                     featureList.add(record);
                }
                System.out.println("FeatureList size: "+featureList.size());
             } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
               
            }
           
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        
        
    }

}

