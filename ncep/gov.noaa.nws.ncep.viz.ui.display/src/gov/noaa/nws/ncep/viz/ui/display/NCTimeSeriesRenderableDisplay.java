package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.area.PredefinedArea;
import gov.noaa.nws.ncep.viz.common.area.PredefinedAreaFactory;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsPaneManager;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName.NcPaneName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

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
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.xy.graph.AbstractXyRenderableDisplay;
import com.raytheon.uf.viz.xy.map.rsc.GraphResource;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * MapRenderableDisplay for NatlCntrs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/21/2014   #1136      qzhou       Initial creation
 * 07/28/2014   R4078      sgurung     Added code changes to support loading TimeSeriesResource in a new window.
 * 
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 * @param
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NCTimeSeriesRenderableDisplay")
@XmlRootElement
public class NCTimeSeriesRenderableDisplay extends AbstractXyRenderableDisplay
        implements AddListener, INatlCntrsRenderableDisplay,
        ISerializableObject {

    @XmlElement
    private NcPaneID paneId;

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

    public NCTimeSeriesRenderableDisplay() {
        this(new NcPaneID(), new PixelExtent(0, 1000, 0, 1000));
    }

    public NCTimeSeriesRenderableDisplay(NcPaneID pid, PixelExtent pe) {
        super(pe, new NCTimeSeriesDescriptor());
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
    public NCTimeSeriesDescriptor getDescriptor() {
        if (super.getDescriptor() instanceof NCTimeSeriesDescriptor) {       
            return (NCTimeSeriesDescriptor) super.getDescriptor();
        } else {          
            super.getDescriptor();
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
                        .getDefaultPredefinedAreaForDisplayType(NcDisplayType.GRAPH_DISPLAY);
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
        super.customizeResourceList(resourceList);

        // Add time series graph resource
        GraphResourceData grd = new GraphResourceData("Time series");
        GraphResource gr = null;
        LoadProperties lprops = new LoadProperties();
        ResourceProperties rprops = new ResourceProperties();
        rprops.setMapLayer(true);
        try {
            gr = grd.construct(lprops, getDescriptor());
            grd.setOverlayMode(GraphResourceData.OverlayMode.OVERLAY);
            ResourcePair rp = new ResourcePair();
            rp.setResourceData(grd);
            rp.setResource(gr);
            rp.setProperties(rprops);
            rp.setLoadProperties(lprops);
            resourceList.add(rp);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error constructing time series Graph", e);
        }

        resourceList.add(ResourcePair
                .constructSystemResourcePair(legendRscData));
        resourceList.add(ResourcePair
                .constructSystemResourcePair(selectedRscData));
        resourceList.addPostAddListener(this);

    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {

        if (container != null) {
            if (container instanceof AbstractEditor) {
                // TODO : any checks on the type of resource here.
                AbstractNcPaneManager pm = NcEditorUtil
                        .getNcPaneManager((AbstractEditor) container);
                if (pm != null) {
                    pm.setDisplayAvailable(false);
                }
            } else {
                AbstractNcPaneManager pm = (AbstractNcPaneManager) getPaneManager();
                if (pm != null) {
                    pm.setDisplayAvailable(false);
                }
            }
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
