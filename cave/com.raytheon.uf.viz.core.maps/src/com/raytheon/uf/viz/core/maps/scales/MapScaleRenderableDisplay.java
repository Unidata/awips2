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
package com.raytheon.uf.viz.core.maps.scales;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager.ManagedMapScale;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

/**
 * MapRenderableDisplay associated with a {@link MapScale}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 22, 2013           mschenke    Initial creation
 * Oct 10, 2013  2104     mschenke    Switched to use MapScalesManager
 * Nov 20, 2013  2492     bsteffen    Recycle resources in clear.
 * Mar 05, 2014  2843     bsteffen    Catch recycle errors in clear.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class MapScaleRenderableDisplay extends PlainMapRenderableDisplay
        implements IMapScaleDisplay {

    protected String scaleName = (String) VizGlobalsManager
            .getCurrentInstance().getPropery(VizConstants.SCALE_ID);

    public MapScaleRenderableDisplay() {
        super();
    }

    public MapScaleRenderableDisplay(IMapDescriptor desc) {
        super(desc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.maps.scales.IMapScaleDisplay#getScaleName()
     */
    @Override
    @XmlAttribute(name = "scale")
    public String getScaleName() {
        return scaleName;
    }

    @Override
    public void setScaleName(String scaleName) {
        this.scaleName = scaleName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.maps.scales.IMapScaleDisplay#changeScale(java
     * .lang.String)
     */
    @Override
    public void changeScale(String scaleName) {
        ManagedMapScale currentScale = MapScalesManager.getInstance()
                .getScaleByName(getScaleName());
        if (currentScale != null) {
            try {
                Bundle bundle = currentScale.getScaleBundle();
                for (AbstractRenderableDisplay display : bundle.getDisplays()) {
                    descriptor.getResourceList().removeAll(
                            display.getDescriptor().getResourceList());
                }
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting scale bundle for " + getScaleName(), e);
            }
        }
        loadScale(MapScalesManager.getInstance().getScaleByName(scaleName));
    }

    @Override
    public void clear() {
        ManagedMapScale scale = MapScalesManager.getInstance().getScaleByName(
                getScaleName());
        if (scale != null) {
            ResourceList list = descriptor.getResourceList();
            for (ResourcePair rp : list) {
                if (rp.getProperties().isSystemResource() == false) {
                    // Keep system resources
                    list.remove(rp);
                }
            }
            loadScale(scale);
        } else {
            // Map scale could not be found, default to remove all
            // non-map/system layers and reset display
            ResourceList list = descriptor.getResourceList();
            for (ResourcePair rp : list) {
                ResourceProperties props = rp.getProperties();
                if (props.isMapLayer() == false
                        && props.isSystemResource() == false) {
                    list.remove(rp);
                } else {
                    try {
                        props.setVisible(true);
                        rp.getResource().recycle();
                    } catch (Throwable e) {
                        props.setVisible(false);
                        statusHandler.handle(Priority.PROBLEM, "Clear error: "
                                + e.getMessage() + ":: The resource ["
                                + rp.getResource().getSafeName()
                                + "] has been disabled.", e);
                    }
                }
            }

            scaleToClientArea(getBounds());
        }
    }

    protected void loadScale(ManagedMapScale scale) {
        if (scale != null) {
            try {
                Bundle bundle = scale.getScaleBundle();
                for (AbstractRenderableDisplay ard : bundle.getDisplays()) {
                    try {
                        descriptor.setGridGeometry(ard.getDescriptor()
                                .getGridGeometry());
                        descriptor.getResourceList().addAll(
                                ard.getDescriptor().getResourceList());
                        ard.getDescriptor().getResourceList().clear();
                        break;
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
                descriptor.getResourceList().instantiateResources(descriptor,
                        true);
                scaleToClientArea(getBounds());
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            setScaleName(scale.getDisplayName());
        }
    }

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        globals.put(VizConstants.SCALE_ID, getScaleName());
        return globals;
    }

}
