package com.raytheon.uf.edex.plugin.madis.ogc;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.MadisObjectFactory;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.DefaultWfsSource;
import com.raytheon.uf.edex.wfs.reg.WfsTranslator;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * 
 * Madis WFS Source
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

public class MadisWfsSource extends DefaultWfsSource {
    

    private static final String schemaloc = "META-INF/schema/madis.xsd";
    
    private WfsFeatureType feature;

    private static String schemaXml = null;

    private static final String spatialKey = "location.location";

    private static final String KEY_NAME = "madis";

    private static final String MADIS_NS = "http://madis.edex.uf.raytheon.com";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisWfsSource.class);
    
    private static final Map<String,String> fieldMap;
    
    static {
        Map<String, String> map = new HashMap<String, String>();
        map.put("obsLocation.location", spatialKey);
        map.put("obsLocation.stationId", "location.stationId");
        map.put("obsLocation.elevation", "location.elevation");
        fieldMap = Collections.unmodifiableMap(map);
    }

    public MadisWfsSource(PluginProperties props) {
        super(props, KEY_NAME, new MadisTranslator(), new MadisFeatureFactory());
        feature = new WfsFeatureType(new QualifiedName(MADIS_NS, key, key),
                key, getCRS(KEY_NAME), getBoundingBox(KEY_NAME));
    }

    @Override
    public Map<String, String> getFieldMap() {
        return fieldMap;
    }
    
    public MadisWfsSource(PluginProperties props, String key,
            WfsTranslator translator, FeatureFactory featFactory) {
        super(props, key, translator, featFactory);

    }

    
    @Override
    public List<WfsFeatureType> listFeatureTypes() {
        return Arrays.asList(feature);
    }

    @Override
    public String describeFeatureType(QualifiedName feature) {
        // we only advertise one feature
        String rval;
        try {
            if (schemaXml == null) {
                ClassLoader loader = MadisWfsSource.class.getClassLoader();
                schemaXml = getResource(loader, schemaloc);
            }
            rval = schemaXml;
        } catch (Exception e) {
            statusHandler.error("Problem reading madis schema", e);
            rval = "Internal Error"; 
        }
        return rval;
    }

    @Override
    public String getFeatureSpatialField(QualifiedName feature) {
        return spatialKey;
    }

    @Override
    public Class<?> getFeatureEntity(QualifiedName feature) {
        return MadisRecord.class;
    }

    @Override
    public Class<?>[] getJaxbClasses() {
        return new Class<?>[] { Madis.class, MadisObjectFactory.class };
    }

}
