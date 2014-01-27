/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.wxxm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.CaseInsensitiveMap;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.db.SingleLayerCollector;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.IPdoGmlTranslator;
import com.raytheon.uf.edex.wfs.reg.PluginWfsSource;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * Abstract base for WFS sources that produce WXXM feature types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public abstract class WxxmWfsSource extends PluginWfsSource {

    private final Map<QualifiedName, Class<?>> classMap;

    private final Map<String, String> fieldMap;

    /**
     * @param props
     * @param key
     * @param translators
     * @param featFactory
     */
    @SuppressWarnings("unchecked")
    public WxxmWfsSource(PluginProperties props, String key,
            List<IPdoWxxmTranslator> translators, FeatureFactory featFactory,
            SingleLayerCollector<?, SimpleLayer<?>, PluginDataObject> collector) {
        super(props, key, convert(translators), featFactory, collector);
        classMap = new HashMap<QualifiedName, Class<?>>(translators.size());
        fieldMap = new CaseInsensitiveMap();
        for (IPdoWxxmTranslator trans : translators) {
            WfsFeatureType ft = trans.getFeatureType();
            classMap.put(ft.getName(), trans.getRecordClass());
            if (trans.getFieldMap() != null) {
                fieldMap.putAll(trans.getFieldMap());
            }
            
            //Map all the aliases to the same translator
            List<WfsFeatureType> aliases = getAliases();
            for(WfsFeatureType alias : aliases) {
                classMap.put(alias.getName(), trans.getRecordClass());
            }
        }
    }

    private static List<IPdoGmlTranslator> convert(List<IPdoWxxmTranslator> list) {
        return new ArrayList<IPdoGmlTranslator>(list);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.AbstractWfsSource#getJaxbClasses()
     */
    @Override
    public Class<?>[] getJaxbClasses() {
        // we are using WXXM jaxb classes
        return new Class<?>[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.AbstractWfsSource#listFeatureTypes()
     */
    @Override
    public List<WfsFeatureType> getFeatureTypes() {
        Collection<IPdoGmlTranslator> values = translatorMap.values();
        List<WfsFeatureType> rval = new ArrayList<WfsFeatureType>(values.size());
        for (IPdoGmlTranslator trans : values) {
            IPdoWxxmTranslator wxTrans = (IPdoWxxmTranslator) trans;
            rval.add(wxTrans.getFeatureType());
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.AbstractWfsSource#getFeatureEntity(com.raytheon
     * .uf.edex.wfs.request.QualifiedName)
     */
    @Override
    public Class<?> getFeatureEntity(QualifiedName feature) {
        return classMap.get(feature);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.AbstractWfsSource#getFieldMap()
     */
    @Override
    public Map<String, String> getFieldMap() {
        return fieldMap;
    }

}
