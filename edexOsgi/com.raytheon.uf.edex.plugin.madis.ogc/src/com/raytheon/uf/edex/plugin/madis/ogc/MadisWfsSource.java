package com.raytheon.uf.edex.plugin.madis.ogc;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.db.SingleLayerCollector;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.Madis;
import com.raytheon.uf.edex.plugin.madis.ogc.feature.MadisObjectFactory;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.IPdoGmlTranslator;
import com.raytheon.uf.edex.wfs.reg.PluginWfsSource;
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
 * 12/11/2013   2625       mpduff       query by insertTime rather than obsTime.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisWfsSource extends PluginWfsSource {

    private static final String schemaloc = "META-INF/schema/madis.xsd";

    private final WfsFeatureType feature;

    private static String schemaXml = null;

    private static final String spatialKey = "location.location";

    private static final String KEY_NAME = "madis";

    private static final String MADIS_NS = "http://madis.edex.uf.raytheon.com";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisWfsSource.class);

    private static final Map<String, String> fieldMap;

    static {
        Map<String, String> map = new HashMap<String, String>();
        map.put("obsLocation.location", spatialKey);
        map.put("timeObs", "insertTime");
        map.put("obsLocation.stationId", "location.stationId");
        map.put("obsLocation.elevation", "location.elevation");
        fieldMap = Collections.unmodifiableMap(map);
    }

    public MadisWfsSource(PluginProperties props, IPdoGmlTranslator translator,
            SingleLayerCollector<?, SimpleLayer<?>, PluginDataObject> collector) {
        super(props, KEY_NAME, Arrays.asList(translator),
                new MadisFeatureFactory(), collector);
        feature = new WfsFeatureType(new QualifiedName(MADIS_NS, key, key),
                key, defaultCRS, fullBbox);
    }

    @Override
    public Map<String, String> getFieldMap() {

        return fieldMap;
    }

    @Override
    public List<WfsFeatureType> getFeatureTypes() {
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.WfsSource#getFeatureVerticalField(com.raytheon
     * .uf.edex.wfs.request.QualifiedName)
     */
    @Override
    public String getFeatureVerticalField(QualifiedName feature) {
        // surface obs don't have vertical fields
        return null;
    }

    @Override
    public String getFeatureIdField(QualifiedName feature) {
        return "id";
    }

}
