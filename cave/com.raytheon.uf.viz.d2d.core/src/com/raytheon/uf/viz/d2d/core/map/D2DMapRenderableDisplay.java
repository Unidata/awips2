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
package com.raytheon.uf.viz.d2d.core.map;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.scales.MapScaleRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.ImageCombiner;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;

/**
 * Implementation of a D2D-specific map renderable display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Feb 09, 2009  1960     njensen    Initial creation
 * Mar 21, 2013  1638     mschenke   Made map scales not tied to d2d
 * Mar 22, 2013  1638     mschenke   Moved map scale code to
 *                                   MapScaleRenderableDisplay
 * Apr 06, 2015  17215    dfriedman  Implement clear to avoid removing time
 *                                   match basis
 * Sep 03, 2015  4779     njensen    Removed DataScale references
 * Dec 03, 2015  5147     bsteffen   Reset TimeMatcher on clear
 * Nov 08, 2016  5976     bsteffen   Remove unused D2D paint properties
 * Feb 13, 2018  6664     bsteffen   Reset load mode on clear.
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class D2DMapRenderableDisplay extends MapScaleRenderableDisplay
        implements ID2DRenderableDisplay {

    /** The magnification */
    @XmlAttribute
    protected double magnification = ((Double) VizGlobalsManager
            .getCurrentInstance().getPropery(VizConstants.MAGNIFICATION_ID))
                    .doubleValue();

    /** The density */
    @XmlAttribute
    protected double density = ((Double) VizGlobalsManager.getCurrentInstance()
            .getPropery(VizConstants.DENSITY_ID)).doubleValue();

    protected ImageCombiner combinerListener = null;

    protected boolean scaleOnNextPaint = false;

    public D2DMapRenderableDisplay() {
        super();
    }

    public D2DMapRenderableDisplay(MapDescriptor desc) {
        super(desc);
        desc.setTimeMatcher(new D2DTimeMatcher());
    }

    @Override
    public double getMagnification() {
        return magnification;
    }

    @Override
    public double getDensity() {
        return density;
    }

    @Override
    public void setMagnification(double magnification) {
        this.magnification = magnification;
        List<ResourcePair> rps = new ArrayList<>(descriptor.getResourceList());
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

    @Override
    public void setDensity(double density) {
        this.density = density;
        List<ResourcePair> rps = new ArrayList<>(descriptor.getResourceList());
        for (int i = 0; i < rps.size(); i++) {
            AbstractVizResource<?, ?> resource = rps.get(i).getResource();
            if (resource != null) {
                if (resource instanceof IResourceGroup) {
                    rps.addAll(((IResourceGroup) resource).getResourceList());
                }
                if (resource.hasCapability(DensityCapability.class)) {
                    resource.getCapability(DensityCapability.class)
                            .setDensity(density);
                }
            }
        }
    }

    @Override
    public String getScale() {
        return getScaleName();
    }

    @Override
    public void setScale(String scale) {
        setScaleName(scale);
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        PaintProperties myProps = new PaintProperties(paintProps);

        if (scaleOnNextPaint) {
            scaleToClientArea(paintProps.getCanvasBounds());
            scaleOnNextPaint = false;
        }

        super.paint(target, myProps);
    }

    @Override
    public void setDescriptor(IDescriptor desc) {
        super.setDescriptor(desc);
        if (!(descriptor.getTimeMatcher() instanceof D2DTimeMatcher)) {
            descriptor.setTimeMatcher(new D2DTimeMatcher());
        }
    }

    @Override
    public IMapDescriptor getDescriptor() {
        return (IMapDescriptor) super.getDescriptor();
    }

    @Override
    protected void setAbstractDescriptor(AbstractDescriptor ad) {
        super.setAbstractDescriptor(ad);
        if (descriptor.getTimeMatcher() == null) {
            descriptor.setTimeMatcher(new D2DTimeMatcher());
        }
    }

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        globals.put(VizConstants.FRAMES_ID,
                new Integer(getDescriptor().getNumberOfFrames()));
        globals.put(VizConstants.DENSITY_ID, new Double(density));
        globals.put(VizConstants.MAGNIFICATION_ID, new Double(magnification));
        globals.put(VizConstants.LOADMODE_ID,
                ((D2DTimeMatcher) getDescriptor().getTimeMatcher())
                        .getLoadMode());
        return globals;
    }

    public void setScaleOnNextPaint(boolean scale) {
        this.scaleOnNextPaint = scale;
    }

    @Override
    public long getBlinkInterval() {
        return super.getBlinkInterval();
    }

    @Override
    public void setBlinkInterval(long blinkInterval) {
        super.setBlinkInterval(blinkInterval);
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        // Add the d2d colorbar resource
        resourceList
                .add(ResourcePair.constructSystemResourcePair(colorBarRscData));
        // Add d2d legend resource
        resourceList
                .add(ResourcePair.constructSystemResourcePair(legendRscData));
        // Add the d2d select pane resource
        resourceList
                .add(ResourcePair.constructSystemResourcePair(selectedRscData));
        // Add the d2d sample resource
        resourceList
                .add(ResourcePair.constructSystemResourcePair(samplingRscData));

        // Add the image combiner
        resourceList.addPostAddListener(getImageCombinerListener());
    }

    /**
     * Get the image combiner listener, instantiates if null
     * 
     * @return
     */
    private AddListener getImageCombinerListener() {
        if (combinerListener == null) {
            combinerListener = new ImageCombiner(getDescriptor());
        }
        return combinerListener;
    }

    /**
     * Replace the time matcher with a copy before clearing. The existing time
     * matcher is carefully tracking information for the resources(especially
     * the time match basis), replacing the timeMatcher saves time updating or
     * removing the saved information since the resources will all be removed
     * anyway.
     */
    @Override
    public void clear() {
        AbstractTimeMatcher timeMatcher = descriptor.getTimeMatcher();
        if (timeMatcher instanceof D2DTimeMatcher) {
            D2DTimeMatcher newTimeMatcher = new D2DTimeMatcher();
            newTimeMatcher.copyFrom(timeMatcher);
            newTimeMatcher.resetLoadMode();
            descriptor.setTimeMatcher(newTimeMatcher);
        }
        super.clear();
    }

}
