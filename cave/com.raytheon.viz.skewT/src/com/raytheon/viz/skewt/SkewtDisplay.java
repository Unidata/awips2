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

package com.raytheon.viz.skewt;

/**
 * SkewTGraph plot the skewt temperature, dewpoint in addition to all background
 * lines and wind data.
 * 
 * Used extensive work from SkewTGraph and associated codebase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15 Nov 2007  #371       ebabin     Initial Coding
 * 27 Nov 2007  #371       ebabin     Updates from initial code.
 * 04Jan2007    #670       ebabin     Updated to include sounding location on inset map.
 * 07Jan2007    #673       ebabin     Fix moving of hodograph points.
 * 09Jan2007    #672       ebabin     Fix for w parameter on sampling.
 * 09Jan2007    #729       ebabin     Update for CAVE crashing on SkewT zoom.
 * 14Jan2007   #682        ebabin     Update for sampling bug. 
 * 15Jan2008           682 ebabin      Updated to remove non calculated parameters. 
 * 16Jan2008           682 ebabin      Updates for grib model traps on multiple loads. 
 * 28sep2008   #1529       dhladky     redone.
 * 12May2010   #1952       snaples    Cleaned up again after refactor.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.skewt.rsc.SkewTBackgroundResource;
import com.raytheon.viz.skewt.rsc.SkewTResource;
import com.raytheon.viz.skewt.rscdata.SkewTBkgResourceData;
import com.raytheon.viz.skewt.ui.HodoBackground;
import com.raytheon.viz.skewt.ui.SkewTConstants;
import com.raytheon.viz.skewt.ui.SkewtBackground;
import com.raytheon.viz.skewt.ui.TempChangeBackground;
import com.raytheon.viz.ui.EditorUtil;
import com.vividsolutions.jts.geom.GeometryFactory;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SkewtDisplay extends AbstractRenderableDisplay implements
        ID2DRenderableDisplay, IInsetMapContainer, ISerializableObject {

    /** The magnification */
    @XmlAttribute
    protected double magnification = ((Double) VizGlobalsManager
            .getCurrentInstance().getPropery(VizConstants.MAGNIFICATION_ID))
            .doubleValue();

    /** The density */
    @XmlAttribute
    protected double density = ((Double) VizGlobalsManager.getCurrentInstance()
            .getPropery(VizConstants.DENSITY_ID)).doubleValue();

    /** The current display scale */
    @XmlAttribute
    protected String scale = (String) VizGlobalsManager.getCurrentInstance()
            .getPropery(VizConstants.SCALE_ID);

    private static GeometryFactory gf = new GeometryFactory();

    private PaintProperties paintProps = null;

    private IGraphicsTarget target = null;

    private SkewTBackgroundResource bkgRsc;

    private AbstractVizResource<?, ?> rsc;

    /**
     * Constructor
     */
    public SkewtDisplay() {
        this(new PixelExtent(SkewTConstants.skewTRectangle));
    }

    private SkewtDisplay(PixelExtent pixelExtent) {
        super(pixelExtent, new SkewTDescriptor(pixelExtent));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.AbstractRenderableDisplay#setDescriptor
     * (com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(IDescriptor desc) {
        super.setDescriptor(desc);

        if (!(desc.getTimeMatcher() instanceof D2DTimeMatcher)) {
            desc.setTimeMatcher(new D2DTimeMatcher());
        }
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);

        this.target = target;
        this.paintProps = paintProps;

        // If no loop properties, use the default values
        LoopProperties loopProperties = paintProps.getLoopProperties();
        if (loopProperties == null) {
            loopProperties = new LoopProperties();
        }

        drawTheData(target, paintProps);
    }

    /**
     * Draws the data on the screen.
     * 
     * @throws VizException
     */
    protected void drawTheData(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        ArrayList<ResourcePair> resourceList = new ArrayList<ResourcePair>(
                descriptor.getResourceList());
        PaintProperties myProps = new PaintProperties(paintProps);

        for (ResourcePair pair : resourceList) {
            if (pair.getProperties().isVisible()) {
                rsc = pair.getResource();
                if (rsc != null) {
                    myProps = calcPaintDataTime(myProps, rsc);

                    if (rsc instanceof SkewTResource) {
                        ((SkewTResource) rsc).setDisplay(this);
                    }
                    rsc.paint(target, myProps);
                }
            }
        }
    }

    /**
     * @return the rsc
     */
    public AbstractVizResource<?, ?> getRsc() {
        return rsc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.AbstractSkewTDisplay#getHodoWorld()
     */
    public WGraphics getHodoWorld() {
        return getHodoBackground().getWorld();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.AbstractSkewTDisplay#getSkewTWorld()
     */
    public WGraphics getSkewTWorld() {
        return getSkewTBackground().getWorld();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.AbstractSkewTDisplay#getTempChangeWorld()
     */
    public WGraphics getTempChangeWorld() {
        return getTempChangeBackground().getWorld();
    }

    public GeometryFactory getGeometry() {
        return gf;
    }

    public IGraphicsTarget getTarget() {
        return target;
    }

    public PaintProperties getPaintProperties() {
        return paintProps;
    }

    public SkewtBackground getSkewTBackground() {
        return bkgRsc.getSkewTBackground();
    }

    public HodoBackground getHodoBackground() {
        return bkgRsc.getHodoBackground();
    }

    public TempChangeBackground getTempChangeBackground() {
        return bkgRsc.getTempChangeBackground();
    }

    @Override
    public double getDensity() {
        return density;
    }

    @Override
    public double getMagnification() {
        return magnification;
    }

    @Override
    public String getScale() {
        return scale;
    }

    @Override
    public void setDensity(double density) {
        target.setNeedsRefresh(true);
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

    @Override
    public void setMagnification(double magnification) {
        target.setNeedsRefresh(true);
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

    @Override
    public void setScale(String scale) {
        this.scale = scale;
    }

    public SkewTEditor getEditor() {
        try {
            return (SkewTEditor) EditorUtil.getActiveEditor();
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public void clear() {
        super.clear();
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        page.closeEditor(getEditor(), false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.map.IInsetMapContainer#getInsetMapLocation()
     */
    @Override
    public FormData getInsetMapLocation() {
        FormData fd = new FormData();
        fd.right = new FormAttachment(15, 0);
        fd.left = new FormAttachment(0, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(10, 0);
        return fd;
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        resourceList.add(ResourcePair
                .constructSystemResourcePair(legendRscData));
        resourceList.add(ResourcePair
                .constructSystemResourcePair(samplingRscData));

        LoadProperties loadProperties = new LoadProperties();
        ColorableCapability colorable = new ColorableCapability();
        colorable.setColor(SkewTConstants.backgroundColor);
        loadProperties.getCapabilities().addCapability(colorable);
        bkgRsc = new SkewTBackgroundResource(new SkewTBkgResourceData(),
                loadProperties);
        ResourceProperties props = new ResourceProperties();
        props.setVisible(true);
        props.setMapLayer(true);
        ResourcePair rp = new ResourcePair();
        rp.setResource(bkgRsc);
        rp.setProperties(props);
        rp.setLoadProperties(bkgRsc.getLoadProperties());
        resourceList.add(rp);
    }

}