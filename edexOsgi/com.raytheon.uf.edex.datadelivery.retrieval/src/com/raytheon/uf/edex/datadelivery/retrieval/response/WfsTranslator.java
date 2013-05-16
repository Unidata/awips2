package com.raytheon.uf.edex.datadelivery.retrieval.response;

import java.util.ArrayList;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;
import net.opengis.gml.v_3_1_1.FeaturePropertyType;
import net.opengis.wfs.v_1_1_0.FeatureCollectionType;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;

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
 * May 12, 2013  753         dhladky     Initial javadoc
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */

public class WfsTranslator extends RetrievalTranslator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
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
    protected ArrayList<DataTime> getTimes() {
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
            FeatureCollectionType featureList = SerializationUtil.unmarshalFromXml(FeatureCollectionType.class, payload);
            if (featureList.getNumberOfFeatures().intValue() > 0) {
                int i = 0;
                metadataAdapter.setPdos(featureList.getNumberOfFeatures().intValue());
                for (FeaturePropertyType type : featureList.getFeatureMember()) {
                    JAXBElement<? extends AbstractFeatureType> feature = type.getFeature();
                    metadataAdapter.getPdos()[i] = metadataAdapter.getRecord(feature);
                    i++;
                }
            }
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, "Can't de-serialize FeatureCollection", e);
        }
        return null;
    }
   
}
