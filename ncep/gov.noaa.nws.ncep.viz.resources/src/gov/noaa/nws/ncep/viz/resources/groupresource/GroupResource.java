package gov.noaa.nws.ncep.viz.resources.groupresource;

import gov.noaa.nws.ncep.viz.common.display.IPowerLegend;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

/**
 * GroupResource - Display resources in group
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/14        ?           B. Yin      Initial creation.
 * 09/14		?			B. Yin		Added updateTimeLine for auto-update.
 * </pre>
 * 
 * @author byin
 * @version 1.0
 */
public class GroupResource extends
        AbstractVizResource<GroupResourceData, NCMapDescriptor> implements
        IPowerLegend, INatlCntrsResource{

    private GroupResourceData groupResourceData;

    private boolean nameExpended = false;

    public GroupResource(GroupResourceData resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        groupResourceData = (GroupResourceData) resourceData;
    }

    @Override
    public String getName() {
        String name = groupResourceData.getGroupName() + "(F"
                + groupResourceData.getFuncKeyNum() + ")";

        return name;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        int displayWidth = (int) (descriptor.getMapWidth() * paintProps
                .getZoomLevel());

        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource == null)
                continue;
            ResourceProperties properties = rp.getProperties();

            if (properties.isDisplayable(displayWidth)) {
                PaintProperties newProps = new PaintProperties(paintProps);
                newProps.setDataTime(descriptor.getTimeForResource(resource));

                resource.paint(target, newProps);
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // this.getCapability(GroupNamingCapability.class);
        // this.getCapabilities();
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null) {
                rsc.init(target);
            }
        }
    }

    @Override
    protected void disposeInternal() {
        // lastTarget = null;
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            rp.getResource().dispose();
        }
    }

    @Override
    public ResourceList getResourceList() {
        if (resourceData != null) {
            return resourceData.getResourceList();
        } else {
            return null;
        }
    }

    @Override
    public int getFuncKeyNum() {
        return groupResourceData.getFuncKeyNum();
    }

    @Override
    public boolean isNameExpanded() {
        if (!this.getProperties().isVisible())
            setNameExpanded(false);
        return nameExpended;
    }

    @Override
    public void setNameExpanded(boolean flag) {
        nameExpended = flag;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        super.project(mapData);
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null) {
                rsc.project(mapData);
            }
        }
    }

    @Override
    public void propertiesChanged(ResourceProperties updatedProps) {

        // if (updatedProps.isVisible()) {
        // setVisibleForAllResources(true);
        // } else {
        // setVisibleForAllResources(false);
        // }
    }

    public void setVisibleForAllResources(boolean visible) {

        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource == null)
                continue;
            resource.getProperties().setVisible(visible);
        }
    }
    
    public Boolean updateTimeline() {
        for (ResourcePair rp : this.getResourceList()) {
            if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
                ((AbstractNatlCntrsResource<?,?>)rp.getResource()).updateTimeline();
            }
        }
        
        return true;
    }

    @Override
    public void resourceAttrsModified() {
        // TODO Auto-generated method stub
        
    }
}
