package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsPaneManager;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName.NcPaneName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * MapRenderableDisplay for NatlCntrs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/19/09                ghull        Initial creation
 * 01/28/10                ghull        Add predefinedAreaName
 * 04/01/10      238,239   archana      Altered the overloaded 
 *                                      constructor to accept the 
 *                                      NCMapDescriptor as its input
 *                                      parameter.  
 *  02/10/2011              Chin Chen   handle multiple editor copies dispose issue    
 *  03/07/2011   migration  ghull       call customizeResourceList
 *  11/18/2012   #630       ghull       construct from areaProvider
 *  04/10/2013   #958       qzhou       Added displayWidth = 1000; Added shouldDisplay.
 *  05/19/2013   #862       ghull       add paneName, implement IAreaProviderCapable 
 *  06/24/2013   2140       randerso    Changed to use standardized paint error handling
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NC-NonMapRenderableDisplay")
@XmlRootElement
public class NCNonMapRenderableDisplay extends AbstractRenderableDisplay
        implements AddListener, INatlCntrsRenderableDisplay,
        ISerializableObject {

    @XmlElement
    private NcPaneID paneId;

    // private String paneName; // the rbd/displayName + the paneId if multipane

    // either the RBD or the Display's paneManager
    private INatlCntrsPaneManager paneContainer;

    // the initial area that the display is set to. This is used for the unzoom.
    // after the display is loaded the user may pan/zoom in which case the
    // current
    // area(gridGeometry,zoom,mapcenter) will be different than the initial
    // area.
    //
    // @XmlElement
    private PredefinedArea initialArea;

    public static final GenericResourceData legendRscData = new GenericResourceData(
            NCLegendResource.class);

    public static final GenericResourceData selectedRscData = new GenericResourceData(
            NcSelectedPaneResource.class);

    public NCNonMapRenderableDisplay() {
        this(new NcPaneID(), new PixelExtent(0, 1000, 0, 1000));
    }

    public NCNonMapRenderableDisplay(NcPaneID pid, PixelExtent pe) {
        super(pe, new NCNonMapDescriptor());
        this.setPaneId(pid);
    }

    @Override
    public double[] getMapCenter() {
        return getExtent().getCenter();
    }

    // this shouldn't be called from NCP but override as a sanity check since
    // AbstractXYRenderableDisplay's setTabTitle() calls getEditor which assumes
    // an XyEditor
    public void setTabTitle(String tabTitle) {
        // tabTitle = tabTitle;
        // if (getEditor() != null) {
        // getEditor().setTabTitle(tabTitle);
        // }
    }

    @Override
    public void dispose() {
        if (this.descriptor != null) {// && editorInstanceNum <= 1) {
            descriptor.getResourceList().clear();
            this.descriptor.getResourceList().removePostAddListener(
                    this.listener);
            this.descriptor.getResourceList().removePostRemoveListener(
                    this.listener);
        }
    }

    // From MapRenderableDisplay with un-needed stuff removed.
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);
        float zoomLevel = paintProps.getZoomLevel();
        LoopProperties loopProperties = paintProps.getLoopProperties();
        // this.zoomLevel = zoomLevel;

        // If no loop properties, use the default values. sanity check?
        if (loopProperties == null) {
            loopProperties = new LoopProperties();
        }

        // Calculate the new map center
        // this.mapCenter = descriptor.pixelToWorld(paintProps.getView()
        // .getExtent().getCenter());

        // ???? do we need this
        // target.setupClippingPlane(getMapExtent());
        // paintProps.setClippingPane(getMapExtent());

        int displayWidth = 1000; // (int) (((MapDescriptor)
                                 // descriptor).getMapWidth() * zoomLevel);

        List<ResourcePair> renderingList = new ArrayList<ResourcePair>(
                descriptor.getResourceList());

        for (ResourcePair pair : renderingList) {
            AbstractVizResource<?, ?> rsc = pair.getResource();

            if (rsc == null) {
                continue;
            }

            // ResourceProperties properties = pair.getProperties();

            // if ((rsc.getStatus() == ResourceStatus.NEW ||
            // properties.isDisplayable(displayWidth))
            // && (!properties.isBlinking() || getCurrentBlinkState())) {
            if (shouldDisplay(pair, displayWidth)) {
                // always reset the alpha
                paintProps.setAlpha(1.0f);

                if (rsc.hasCapability(ImagingCapability.class)) {
                    paintProps.setAlpha(rsc.getCapability(
                            ImagingCapability.class).getAlpha());
                }

                paintProps = calcPaintDataTime(paintProps, rsc);
                paintResource(pair, target, paintProps);
            }
        }
        target.clearClippingPlane();
    }

    @Override
    public NcPaneName getPaneName() {
        if (getPaneManager().getPaneLayout().getNumberOfPanes() == 1) {
            return new NcPaneName(getPaneManager().getDisplayName());
        } else {
            return new NcPaneName(getPaneManager().getDisplayName(),
                    getPaneId());
        }
    }

    @Override
    public NcPaneID getPaneId() {
        if (paneId == null) {
            paneId = new NcPaneID();
        }
        return paneId;
    }

    @Override
    public void setPaneId(INcPaneID pid) {
        paneId = (NcPaneID) pid;
    }

    // TODO? if null then set to the descriptors gridGeom??
    @Override
    public NCNonMapDescriptor getDescriptor() {
        if (super.getDescriptor() instanceof NCNonMapDescriptor) {
            return (NCNonMapDescriptor) super.getDescriptor();
        }
        return null;
    }

    @Override
    public void setExtent(IExtent pe) {
        super.setExtent(pe);
    }

    //
    @Override
    public PredefinedArea getInitialArea() {
        if (initialArea == null) {
            try {
                initialArea = PredefinedAreaFactory
                        .getDefaultPredefinedAreaForDisplayType(NcDisplayType.NTRANS_DISPLAY);
            } catch (VizException e) {
            }

        }
        return initialArea;
    }

    private GeneralGridGeometry createGridGeometry(IExtent extent,
            CoordinateReferenceSystem crs) {
        // copied from AbstractDescriptor since it was protected
        GeneralEnvelope envelope = new GeneralEnvelope(2);
        envelope.setRange(0, extent.getMinX(), extent.getMaxX());
        envelope.setRange(1, extent.getMinY(), extent.getMaxY());
        envelope.setCoordinateReferenceSystem(crs);
        return new GridGeometry2D(
                new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                        (int) extent.getWidth(), (int) extent.getHeight() },
                        false), envelope);
    }

    @Override
    public double getZoomLevel() {
        return 1.0;
    }

    @Override
    public void setInitialArea(PredefinedArea area) {
        initialArea = area;

        try {
            // setPredefinedArea( initialArea );
            getDescriptor().setGridGeometry(area.getGridGeometry());

            // if( initialArea.getMapCenter() == null ) {
            // initialArea.setMapCenter( getMapCenter() );
            // }

        } catch (VizException e) {
            System.out
                    .println("Error setting initial area of renderable display:"
                            + e.getMessage());
        }
        // if this is actually called/needed then check that the crs is 2d
        // Cartesian
        // and set the extents.

        // System.out.println("setInitialArea not implemented for non-map display");
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        // resourceList. // check if already in the list???
        resourceList.add(ResourcePair
                .constructSystemResourcePair(legendRscData));
        resourceList.add(ResourcePair
                .constructSystemResourcePair(selectedRscData));
        resourceList.addPostAddListener(this);
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {

        // TODO : any checks on the type of resource here.
        AbstractNcPaneManager pm = NcEditorUtil
                .getNcPaneManager((AbstractEditor) container);
        if (pm != null) {
            pm.setDisplayAvailable(false);
        }
    }

    protected boolean shouldDisplay(ResourcePair pair, int displayWidth) {
        AbstractVizResource<?, ?> rsc = pair.getResource();
        ResourceProperties properties = pair.getProperties();

        if (rsc == null) {
            return false;
        }

        ResourceStatus status = rsc.getStatus();

        if (status == ResourceStatus.DISPOSED) {
            return false;
        }

        boolean doNotDrawBecauseOfBlinking = false;
        if (properties.isBlinking()) {
            if (!rsc.hasCapability(ColorMapCapability.class)) {
                // Not a colormapped image...
                doNotDrawBecauseOfBlinking = !getCurrentBlinkState();
            } else {
                ColorMapParameters params = rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters();
                params.setUseMask(!getCurrentBlinkState());
                // notify the resource it is blinking.
                rsc.issueRefresh();
            }
        }

        if (!doNotDrawBecauseOfBlinking) {
            if (pair.getResource() instanceof IResourceGroup) {
                for (ResourcePair rp : ((IResourceGroup) pair.getResource())
                        .getResourceList()) {
                    doNotDrawBecauseOfBlinking &= shouldDisplay(rp,
                            displayWidth);
                }
            }
        }

        boolean drawBecauseItsNew = status == ResourceStatus.NEW;

        if (!drawBecauseItsNew) {
            if (pair.getResource() instanceof IResourceGroup) {
                for (ResourcePair rp : ((IResourceGroup) pair.getResource())
                        .getResourceList()) {
                    if (rp.getResource() != null) {
                        drawBecauseItsNew |= rp.getResource().getStatus() == ResourceStatus.NEW;
                    }
                }
            }
        }

        return (drawBecauseItsNew || properties.isDisplayable(displayWidth))
                && !doNotDrawBecauseOfBlinking;
    }

    @Override
    public void setPaneManager(INatlCntrsPaneManager pm) {
        paneContainer = pm;
    }

    @Override
    public void setContainer(IDisplayPaneContainer container) {
        super.setContainer(container);

        if (container instanceof AbstractEditor) {
            INatlCntrsPaneManager pm = NcEditorUtil
                    .getNcPaneManager((AbstractEditor) container);
            setPaneManager(pm);
        }
    }

    @Override
    public INatlCntrsPaneManager getPaneManager() {
        return paneContainer;
    }
}
