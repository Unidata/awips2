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
package com.raytheon.uf.viz.d2d.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.xy.graph.AbstractXyRenderableDisplay;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public abstract class AbstractNonMapDisplay extends AbstractXyRenderableDisplay
        implements ID2DRenderableDisplay, ISerializableObject,
        IInsetMapContainer {

    /** The magnification */
    @XmlAttribute
    private double magnification = 1.0;

    /** The density */
    @XmlAttribute
    private double density = 1.0;

    /** The current display scale */
    @XmlAttribute
    private String scale;

    /**
     * The extent for the this display adjusted to leave room for the corner
     * map.
     */
    private IExtent adjustedExtent;

    public AbstractNonMapDisplay() {
        this(new PixelExtent(0, 1000, 0, 1000), null);
    }

    public AbstractNonMapDisplay(PixelExtent aPixelExtent,
            IDescriptor descriptor) {
        super(aPixelExtent, descriptor);
        this.descriptor.setTimeMatcher(new D2DTimeMatcher());
        adjustedExtent = aPixelExtent;
    }

    @Override
    public void setDescriptor(IDescriptor desc) {
        super.setDescriptor(desc);
        desc.setTimeMatcher(new D2DTimeMatcher());
    }

    @Override
    public void calcPixelExtent(Rectangle clientArea) {
        super.calcPixelExtent(clientArea);
    }

    /**
     * @return the magnification
     */
    public double getMagnification() {
        return magnification;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(double magnification) {
        this.magnification = magnification;
        List<ResourcePair> rps = new ArrayList<ResourcePair>(
                descriptor.getResourceList());
        for (int i = 0; i < rps.size(); i++) {
            AbstractVizResource<?, ?> resource = rps.get(i).getResource();
            if (resource != null) {
                if (resource instanceof IResourceGroup) {
                    rps.addAll(((IResourceGroup) resource).getResourceList());
                }
                if (resource.hasCapability(MagnificationCapability.class)) {
                    resource.getCapability(MagnificationCapability.class)
                            .setMagnification(magnification);
                }
            }
        }
    }

    /**
     * @return the density
     */
    public double getDensity() {
        return density;
    }

    /**
     * @param density
     *            the density to set
     */
    public void setDensity(double density) {
        this.density = density;
        List<ResourcePair> rps = new ArrayList<ResourcePair>(
                descriptor.getResourceList());
        for (int i = 0; i < rps.size(); i++) {
            AbstractVizResource<?, ?> resource = rps.get(i).getResource();
            if (resource != null) {
                if (resource instanceof IResourceGroup) {
                    rps.addAll(((IResourceGroup) resource).getResourceList());
                }
                if (resource.hasCapability(DensityCapability.class)) {
                    resource.getCapability(DensityCapability.class).setDensity(
                            density);
                }
            }
        }
    }

    /**
     * @return the scale
     */
    public String getScale() {
        return scale;
    }

    /**
     * @param scale
     *            the scale to set
     */
    public void setScale(String scale) {
        this.scale = scale;
    }

    /**
     * @return the adjustedExtent
     */
    public IExtent getAdjustedExtent() {
        return adjustedExtent;
    }

    @Override
    public FormData getInsetMapLocation() {
        FormData fd = new FormData();
        fd.right = new FormAttachment(100, 0);
        fd.left = new FormAttachment(81, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(19, 0);
        return fd;
    }

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        globals.put(VizConstants.SCALE_ID, getScale());
        globals.put(VizConstants.MAGNIFICATION_ID, getMagnification());
        globals.put(VizConstants.DENSITY_ID, getDensity());
        if (getDescriptor().getTimeMatcher() instanceof D2DTimeMatcher) {
            globals.put(VizConstants.LOADMODE_ID,
                    ((D2DTimeMatcher) getDescriptor().getTimeMatcher())
                            .getLoadMode());
        }
        return globals;
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        // Add the d2d colorbar resource
        resourceList.add(ResourcePair
                .constructSystemResourcePair(colorBarRscData));
        // Get the d2d legend resource
        resourceList.add(ResourcePair
                .constructSystemResourcePair(legendRscData));
        // Add the d2d select pane resource
        resourceList.add(ResourcePair
                .constructSystemResourcePair(selectedRscData));
        // Add the d2d sample resource
        resourceList.add(ResourcePair
                .constructSystemResourcePair(samplingRscData));
    }

}
