package com.raytheon.uf.edex.ogc.common.feature;

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

import java.util.List;

import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.common.datadelivery.harvester.ConfigLayer;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfig;
import com.raytheon.uf.common.datadelivery.harvester.HarvesterConfigurationManager;
import com.raytheon.uf.common.datadelivery.harvester.OGCAgent;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * 
 * Abstract Feature Factory
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/2013   1746      dhladky      initial
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class AbstractFeatureFactory implements FeatureFactory {

    public AbstractFeatureFactory() {
        configureFeature();
    }
    
    public void configureFeature() {
        getLayer();
    }
    
    protected ConfigLayer layer;
    
    @Override
    public abstract List<SimpleFeature> convert(PluginDataObject[] pdos);
    
    public abstract String getName();
    
    /**
     * Gets the ConfigLayer associated with this feature
     * @return
     */
    public ConfigLayer getLayer() {

        if (layer == null) {
            OGCAgent agent = null;
            HarvesterConfig config = HarvesterConfigurationManager
                    .getOGCConfiguration();
            if (config.getAgent() instanceof OGCAgent) {
                agent = (OGCAgent) config.getAgent();
                if (agent != null) {
                    layer = agent.getLayer(getName());
                }
            }
        }
        return layer;
    }

}
