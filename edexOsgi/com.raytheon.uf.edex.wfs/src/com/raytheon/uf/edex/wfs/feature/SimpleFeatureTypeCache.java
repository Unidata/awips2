/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.wfs.feature;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.type.FeatureType;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchAuthorityCodeException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.spatial.CrsLookup;

/**
 * Creates goetools simple feature types and caches results internally.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SimpleFeatureTypeCache implements IFeatureTypeCache {

    protected ConcurrentHashMap<FeatureTypeConfig, FeatureType> cache = new ConcurrentHashMap<FeatureTypeConfig, FeatureType>();

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.feature.FeatureTypeCache#getFeatureType(java
     * .lang.Class)
     */
    @Override
    public FeatureType getFeatureType(FeatureTypeConfig config)
            throws NoSuchAuthorityCodeException, FactoryException, OgcException {
        if (config == null) {
            return null;
        }
        FeatureType rval = cache.get(config);
        if (rval == null) {
            rval = extractFeatureType(config);
            cache.put(config, rval);
        }

        return rval;
    }

    protected FeatureType extractFeatureType(FeatureTypeConfig config)
            throws NoSuchAuthorityCodeException, FactoryException, OgcException {
        switch (config.method) {
        case JAXB:
            return extractFromJaxb(config);
        default:
            throw new RuntimeException("Invalid featuretype extraction method");
        }
    }

    /**
     * Naive implementation. Does not account for embedded types, lists (arrays)
     * or adapters.
     * 
     * @param config
     * @return
     * @throws NoSuchAuthorityCodeException
     * @throws FactoryException
     * @throws OgcException
     */
    protected FeatureType extractFromJaxb(FeatureTypeConfig config)
            throws NoSuchAuthorityCodeException, FactoryException, OgcException {
        SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
        builder.setName(config.getName());
        builder.setNamespaceURI(config.getNamespace());
        CoordinateReferenceSystem crs = CrsLookup.lookup(config.getCrs());
        builder.setDefaultGeometry(config.getGeomName());
        builder.setCRS(crs);
        Class<?> binding = config.getBinding();
        Field[] fields = binding.getFields();
        for (Field f : fields) {
            if (hasJaxb(f)) {
                String name = f.getName();
                Class<?> c = f.getDeclaringClass();
                builder.add(name, c);
            }
        }
        return builder.buildFeatureType();
    }

    protected boolean hasJaxb(Field f) {
        XmlElement xmle = f.getAnnotation(XmlElement.class);
        if (xmle != null) {
            return true;
        }
        XmlAttribute xmla = f.getAnnotation(XmlAttribute.class);
        if (xmla != null) {
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.feature.FeatureTypeCache#removeClass(java.lang
     * .Class)
     */
    @Override
    public IFeatureTypeCache removeClass(FeatureTypeConfig config) {
        if (config != null) {
            cache.remove(config);
        }
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.feature.FeatureTypeCache#clear()
     */
    @Override
    public IFeatureTypeCache clear() {
        cache.clear();
        return this;
    }

}
