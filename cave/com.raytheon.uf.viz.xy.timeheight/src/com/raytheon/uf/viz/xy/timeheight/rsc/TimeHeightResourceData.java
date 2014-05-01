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
package com.raytheon.uf.viz.xy.timeheight.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResourceData;
import com.raytheon.viz.grid.rsc.GridLoadProperties;

/**
 * Resource data for time height resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class TimeHeightResourceData extends VarHeightResourceData {

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AbstractTimeHeightResource rsc = null;
        if (objects.length > 0) {
            PluginDataObject pdo = objects[0];
            AbstractVarHeightAdapter<?> adapter = getAdapter(pdo);
            adapter.setResourceData(this);
            if (loadProperties instanceof GridLoadProperties) {
                GridLoadProperties gridLoad = (GridLoadProperties) loadProperties;
                switch (gridLoad.getCapabilities()
                        .getCapability(this, DisplayTypeCapability.class)
                        .getDisplayType()) {
                case IMAGE:
                    rsc = new TimeHeightImageResource(this, loadProperties,
                            adapter);
                    break;
                case BARB:
                    rsc = new TimeHeightVectorResource(this, loadProperties,
                            adapter);
                    break;
                }

            }
            if (rsc == null) {
                rsc = new TimeHeightContourResource(this, loadProperties,
                        adapter);
            }
            if (rsc != null) {
                for (PluginDataObject rec : objects) {
                    rsc.addRecord(rec);
                }
                return rsc;
            }
        }
        throw new VizException(
                "No records retrieved, unable to determine resource type");
    }

}
