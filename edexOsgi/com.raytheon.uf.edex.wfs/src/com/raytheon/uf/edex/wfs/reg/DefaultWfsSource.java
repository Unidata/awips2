/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.reg;

import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;

import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfig;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfigurationManager;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.request.QualifiedName;


/**
 * 
 * Default WFS Source 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2013   1746       dhladky      initial creation, based on B Clements original
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */


public abstract class DefaultWfsSource extends
		AbstractWfsSource<PluginDataObject> {

	protected CoreDao _dao;

	protected PluginProperties props;

	protected WfsTranslator translator;

	protected FeatureFactory featFactory;
	
    protected HarvesterConfig config = null;
    
    protected ConfigLayer layer = null;
    
    private static final IUFStatusHandler statusHandler = UFStatus
    .getHandler(DefaultWfsSource.class);


	public DefaultWfsSource(PluginProperties props, String key,
			WfsTranslator translator, FeatureFactory featFactory) {
		super(key);
		this.props = props;
		this.translator = translator;
		this.featFactory = featFactory;
		this.config = HarvesterConfigurationManager.getOGCConfiguration();
	}

	@Override
	protected CoreDao getDao() throws PluginException {
		if (_dao == null) {
			_dao = PluginFactory.getInstance().getPluginDao(
					props.getPluginName());
		}
		return _dao;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#query(com.raytheon.uf.edex.wfs
	 * .request.QualifiedName, com.raytheon.uf.edex.db.api.DatabaseQuery)
	 */
	@Override
	public List<JAXBElement<? extends AbstractFeatureType>> query(
			QualifiedName feature, WfsQuery query) throws WfsException {
		List<PluginDataObject> pdos = queryInternal(feature, query);
		PluginDataObject[] arr = pdos
				.toArray(new PluginDataObject[pdos.size()]);
		try {
			return translator.translate(arr);
		} catch (Exception e) {
		    statusHandler.handle(Priority.ERROR, "Problem translating pdos to jaxb features", e);
			throw new WfsException(Code.INTERNAL_SERVER_ERROR);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wfs.reg.WfsSource#querySimple(com.raytheon.uf.edex
	 * .wfs.request.QualifiedName, com.raytheon.uf.edex.wfs.reg.WfsQuery)
	 */
	@Override
	public List<SimpleFeature> querySimple(QualifiedName feature, WfsQuery q)
			throws WfsException {
		List<PluginDataObject> pdos = queryInternal(feature, q);
		PluginDataObject[] arr = pdos
				.toArray(new PluginDataObject[pdos.size()]);
		return featFactory.convert(arr);
	}
	
    /**
     * Get the configuration for the layer (feature)
     * 
     * @param name
     * @return
     */
    public ConfigLayer getConfigLayer(String name) {
        ConfigLayer layer = null;
        if (config.getAgent() != null) {
            if (config.getAgent() instanceof OGCAgent) {
                layer = ((OGCAgent) config.getAgent()).getLayer(name);
            }
        }
        return layer;
    }

    /**
     * Gets the configured bounded box
     * 
     * @param layer
     * @return
     */
    public OgcGeoBoundingBox getBoundingBox(String name) {
        
        OgcGeoBoundingBox bbox = null;

        try {
            ConfigLayer layer = getConfigLayer(name);
            double upperLeftLon = layer.getMinx();
            double lowerRightLon = layer.getMaxx();
            double upperLeftLat = layer.getMaxy();
            double lowerRightLat = layer.getMiny();
            bbox = new OgcGeoBoundingBox(lowerRightLon, upperLeftLon,
                    upperLeftLat, lowerRightLat);

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Couldn't create Bounding Box for feature: "+name+", Using default");
            bbox = fullBbox;
        }
        
        return bbox;
    }
    
    /**
     * Override this if you want a different CRS from you config
     * @param name
     * @return
     */
    public String getCRS(String name) {
        
        String crs = null;
        
        try {
            ConfigLayer layer = getConfigLayer(name);
            crs = layer.getCrs();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Couldn't retrieve CRS for feature: "+name+", Using default");
            crs = defaultCRS;
        }

        return crs;
    }

}
