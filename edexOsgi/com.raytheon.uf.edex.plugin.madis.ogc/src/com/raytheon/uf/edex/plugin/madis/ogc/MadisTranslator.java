package com.raytheon.uf.edex.plugin.madis.ogc;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.edex.plugin.madis.MadisPointDataTransform;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.MadisObjectFactory;
import com.raytheon.uf.edex.wfs.reg.WfsTranslator;

/**
 * 
 * Madis Layer Translator Impl
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2013   1746       dhladky      Initial creation
 * 05/30/2013   753        dhladky      updated
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class MadisTranslator implements WfsTranslator {
    
    
    public MadisTranslator() {

    }

    @Override
    public List<JAXBElement<? extends AbstractFeatureType>> translate(
            PluginDataObject[] pdos) {
        ArrayList<JAXBElement<? extends AbstractFeatureType>> rval = new ArrayList<JAXBElement<? extends AbstractFeatureType>>(
                pdos.length);

        for (PluginDataObject pdo : pdos) {
            rval.add(translate(pdo));
        }
        return rval;
    }

    /**
     * @param pdo
     * @return
     */
    public JAXBElement<Madis> translate(PluginDataObject pdo) {
        
        MadisRecord record = null;
        record = MadisPointDataTransform.populatePointDataFields((MadisRecord) pdo);
        
        return new MadisObjectFactory().create(new Madis(record));
    }

}
