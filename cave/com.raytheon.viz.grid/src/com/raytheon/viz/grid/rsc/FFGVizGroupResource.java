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
package com.raytheon.viz.grid.rsc;

import java.util.ArrayList;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * FFG Group Resource class.
 * 
 * Based off VizGroupResource.java
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 19, 2012  1162     mpduff    Initial creation.
 * Jun 21, 2013  15394    mgamazay  Implement IResourceDataChanged and override
 *                                  resourceChanged method.
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Jan 24, 2018  6758     njensen   Override project(CoordinateReferenceSystem)
 * 
 * </pre>
 * 
 * @author mpduff
 */
public class FFGVizGroupResource
        extends AbstractVizResource<FfgVizGroupResourceData, MapDescriptor>
        implements IResourceDataChanged {

    private static final String NO_DATA = "No Data";

    protected FFGVizGroupResource(FfgVizGroupResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        this.resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            if (rp.getResource() != null) {
                rp.getResource().dispose();
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            if (rp.getResource() != null) {
                rp.getResource().paint(target, paintProps);
            }
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        ResourceList rl = resourceData.getResourceList();
        String value = "No Data";
        Map<AbstractVizResource<?, ?>, DataTime[]> timeMap = descriptor
                .getTimeMatchingMap();
        for (ResourcePair pair : rl) {
            if (pair.getResource() != null) {
                AbstractVizResource<?, ?> rsc = pair.getResource();
                timeMap.put(rsc, timeMap.get(this));
                value = rsc.inspect(coord);
                if (!NO_DATA.equalsIgnoreCase(value)) {
                    return value;
                }
            }
        }

        return value;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        for (ResourcePair pair : resourceData.getResourceList()) {
            if (pair.getResource() != null) {
                pair.getResource().init(target);
            }
        }

        // If child resources have capabilities that this does not, steal them
        for (ResourcePair pair : getResourceData().getResourceList()) {
            for (AbstractCapability capability : pair.getResource()
                    .getCapabilities().getCapabilityClassCollection()) {
                this.getCapabilities().addCapability(capability);
                capability.setResourceData(resourceData);
            }
        }

        // Spread my master capability set to all my children
        for (AbstractCapability capability : getCapabilities()
                .getCapabilityClassCollection()) {
            for (ResourcePair pair : getResourceData().getResourceList()) {
                pair.getResource().getCapabilities().addCapability(capability);
            }
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (object instanceof Object[]) {
            this.resourceData.getResourceList().get(0).getResourceData()
                    .update(object);
        } else {
            ArrayList<Object> theObjectList = new ArrayList<>();
            theObjectList.add(object);
            this.resourceData.getResourceList().get(0).getResourceData()
                    .update(theObjectList.toArray());
        }
    }

    @Override
    public final void project(CoordinateReferenceSystem crs)
            throws VizException {
        for (ResourcePair pair : this.resourceData.getResourceList()) {
            pair.getResource().project(crs);
        }
    }

}
