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
package com.raytheon.uf.edex.ogc.registry;

import java.util.List;

import com.raytheon.uf.common.datadelivery.harvester.Agent;
import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfig;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfigurationManager;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.AbstractWfsSource;
import com.raytheon.uf.edex.wfs.reg.IFeatureTypeModifier;

/**
 * Restricts feature crs and bounds by registry metadata
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      Adapted to AWIPS
 * Jan 13, 2014  #2679     dhladky      multiple layers mapping to single request window
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class RegistryFeatureTypeModifier implements IFeatureTypeModifier {

    protected HarvesterConfig config = null;

    protected ConfigLayer layer = null;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryFeatureTypeModifier.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.FeatureTypeModifier#modify(java.util.List)
     */
    @Override
    public List<WfsFeatureType> modify(List<WfsFeatureType> list) {
        for (WfsFeatureType ft : list) {
            modify(ft);
        }
        return list;
    }

    public void modify(WfsFeatureType ft) {
        String name = ft.getName().getName();
        OgcGeoBoundingBox boundingBox = getBoundingBox(name);
        if (boundingBox != null) {
            ft.setBbox(boundingBox);
        }
        String crs = getCRS(name);
        if (crs != null) {
            ft.setDefaultSRS(crs);
        }
    }

    /**
     * Get the configuration for the layer (feature)
     * 
     * @param name
     * @return
     */
    public ConfigLayer getConfigLayer(String name) {

        if (layer == null) {
            Agent agent = getHarvesterConfig().getAgent();
            if (agent != null) {
                if (agent instanceof OGCAgent) {
                    layer = ((OGCAgent) agent).getLayer(name);
                }
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
            // we default to whole earth now.
            // We don't really care about the DataSet specific areas for the feature
            // They are added together and treated as a whole for requests from the DPA
            bbox = AbstractWfsSource.fullBbox;

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't create Bounding Box for feature: " + name
                            + ", Using default", e);
        }

        return bbox;
    }

    /**
     * Override this if you want a different CRS from you config
     * 
     * @param name
     * @return
     */
    public String getCRS(String name) {
        String crs = null;

        try {
            // Default to WGS:84 
            crs = AbstractWfsSource.defaultCRS;
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't retrieve CRS for feature: " + name
                            + ", Using default", e);
        }

        return crs;
    }
    
    /**
     * Get the Harvester Configuration
     * @return
     */
    protected HarvesterConfig getHarvesterConfig() {
        
        if (config == null) {
            config = HarvesterConfigurationManager.getOGCConfiguration();
        }
        
        return config;
    }

}
