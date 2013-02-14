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
package com.raytheon.uf.viz.d2d.core.legend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.GroupNamingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.ui.actions.DummyAction;

/**
 * Legend decorator for d2d. Responsible for drawing and handling mouse input
 * actions on the legend
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2010            mschenke     Initial creation
 * Jul 1, 2010  #6146      bkowal       Will now create 20-character legend strings for the legends
 *                                      that are displayed in the smaller panes provided that the
 *                                      Legend Mode is set to 'SHORT_PRODUCT'. Also keeps track of
 *                                      the number of Graphics.
 * Jul 8, 2010  #6146      bkowal       No longer has a separate font size for legend text that is 
 *                                      drawn into the smaller pane.
 * Sep 4, 2012  15335      kshresth     Will now display lightning/wind fields
 *                                      when magnification set to 0
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DLegendResource extends
        AbstractLegendResource<GenericResourceData> {

    /** Color used for TIME mode */
    private static final RGB WHITE = new RGB(255, 255, 255);

    /** Color for non visible resources */
    private static final RGB GRAY = new RGB(127, 127, 127);

    /** String for when no data is available */
    private static final String NO_DATA = "No Data Available";

    /** String for when resource has data but not for current frame */
    private static final String NOT_LOADED = "Not Loaded";

    private final SetTimeMatchBasisAction TMB_ACTION = new SetTimeMatchBasisAction();

    /** Current legend mode */
    private LegendMode mode = LegendMode.PRODUCT;

    private IFont legendFont;

    private IInputHandler legendHandler = new D2DLegendClickHandler(this);

    private IInputHandler changeModeHandler = new D2DChangeLegendModeHandler(
            this);

    public static enum LegendMode {
        PRODUCT("Show Product Legends"), MAP("Show Map Legends"), NONE(
                "Show All Legends"), HIDE("Hide Legends"), SHORT_PRODUCT("");

        private String str;

        LegendMode(String str) {
            this.str = str;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return str;
        }
    }

    /**
     * @param resourceData
     * @param loadProperties
     */
    public D2DLegendResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {
        legendFont.setMagnification(getScaledMagnification());
        FramesInfo info = descriptor.getFramesInfo();
        List<LegendEntry> labels = new ArrayList<LegendEntry>();
        ResourceList resourceList = descriptor.getResourceList();
        if (resourceList != null) {
            boolean done = false;
            for (int i = 0; i < resourceList.size() && !done; i++) {
                ResourcePair resourcePair = resourceList.get(i);
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();

                // See if resource is a system resource (does not
                // participate in legend)
                boolean system = resourcePair.getProperties()
                        .isSystemResource();
                if (system || rsc == null || notInitialized(rsc)) {
                    continue;
                }

                LegendEntry entry = new LegendEntry();
                entry.font = legendFont;
                switch (mode) {
                case MAP: {
                    if (resourcePair.getProperties().isMapLayer()) {
                        entry.legendParts = getLegendData(info, mode,
                                resourcePair, null);
                        labels.add(entry);
                    }
                    break;
                }
                case PRODUCT:
                case SHORT_PRODUCT: {
                    if (resourcePair.getProperties().isMapLayer() == false) {
                        entry.legendParts = getLegendData(info, mode,
                                resourcePair, null);
                        labels.add(entry);
                    }
                    break;
                }
                case NONE: {
                    int idx = info.getFrameIndex();
                    boolean hasTime = idx > -1 && idx < info.getFrameCount();
                    if (!hasTime) {
                        entry.legendParts = getLegendData(info, mode,
                                resourcePair, null);
                        labels.add(entry);
                    } else {
                        if (resourcePair.getProperties().isMapLayer() == false) {
                            LegendData ld = new LegendData();
                            ld.color = WHITE;

                            if (hasTime) {
                                DataTime[] timeFrames = info.getFrameTimes();
                                if (timeFrames != null) {
                                    ld.label = timeFrames[idx]
                                            .getLegendString();
                                } else {
                                    ld.label = "";
                                }
                            } else {
                                ld.label = NO_DATA;
                            }
                            entry.legendParts = new LegendData[] { ld };
                            labels.add(entry);
                            done = true;
                        }
                    }
                    break;
                }
                }
            }
        }

        return labels.toArray(new LegendEntry[labels.size()]);
    }

    private LegendData[] getLegendData(FramesInfo frameInfo, LegendMode mode,
            ResourcePair rp, RGB color) {
        List<LegendData> legendData = new ArrayList<LegendData>();

        if (rp.getProperties().isMapLayer()) {
            LegendData ld = new LegendData();
            ld.resource = rp;
            ld.label = rp.getResource().getName();
            ld.color = rp.getProperties().isVisible() ? rp.getResource()
                    .getCapability(ColorableCapability.class).getColor() : GRAY;
            legendData.add(ld);
        } else if (rp.getResource().hasCapability(BlendableCapability.class)) {
            ResourceList list = rp.getResource()
                    .getCapability(BlendableCapability.class).getResourceList();
            boolean first = true;
            boolean visible = rp.getProperties().isVisible();
            for (ResourcePair rscPair : list) {
                String text = getLegendString(rscPair, descriptor, frameInfo);
                LegendData ld = new LegendData();
                ld.label = text;
                if (!visible || !rscPair.getProperties().isVisible()) {
                    ld.color = GRAY;
                } else {
                    ld.color = rp.getResource()
                            .getCapability(ColorableCapability.class)
                            .getColor();
                }
                if (!first) {
                    LegendData main = new LegendData();
                    main.resource = rp;
                    main.color = visible ? rp.getResource()
                            .getCapability(ColorableCapability.class)
                            .getColor() : GRAY;
                    main.label = " + ";
                    legendData.add(main);
                } else {
                    first = false;
                }
                ld.resource = rscPair;
                legendData.add(ld);

                if (this.mode == LegendMode.SHORT_PRODUCT) {
                    break;
                }
            }
        } else {
            LegendData ld = new LegendData();
            ld.resource = rp;
            RGB toUse = color == null ? rp.getResource()
                    .getCapability(ColorableCapability.class).getColor()
                    : color;
            ld.color = rp.getProperties().isVisible() ? toUse : GRAY;
            ld.label = getLegendString(rp, descriptor, frameInfo);
            if (ld.label != null) {
                legendData.add(ld);
            }
        }

        return legendData.toArray(new LegendData[legendData.size()]);
    }

    /**
     * Given a non IResourceGroup ResourcePair, give the legend string for the
     * resource
     * 
     * @param rsc
     * @param descriptor
     * @return
     */
    private String getLegendString(ResourcePair rsc, IDescriptor descriptor,
            FramesInfo info) {
        return getLegendString(rsc, descriptor, info, false);
    }

    /**
     * Given a non IResourceGroup ResourcePair, give the legend string for the
     * resource, also adds star for time match basis
     * 
     * @param rsc
     * @param descriptor
     * @param addStar
     * @return
     */
    private String getLegendString(ResourcePair rsc, IDescriptor descriptor,
            FramesInfo info, boolean fromResourceGroup) {
        return getLegendString(rsc, descriptor, info, fromResourceGroup, true);
    }

    private String getLegendString(ResourcePair rsc, IDescriptor descriptor,
            FramesInfo info, boolean fromResourceGroup, boolean includeTime) {
        if (rsc.getResource().getResourceData() instanceof IResourceGroup) {
            return getResourceGroupLegendString(rsc.getResource(), descriptor,
                    info);
        }

        if (rsc.getResource() == null) {
            return NO_DATA;
        }

        AbstractVizResource<?, ?> resource = rsc.getResource();
        String name = resource.getName();
        if (name == null) {
            return null;
        }

        DataTime time = getDataTimeForResource(rsc.getResource(), descriptor,
                info);

        if (!resource.isTimeAgnostic()) {
            boolean hasTimes = false;
            DataTime[] times = info.getTimeMap().get(resource);
            if (times != null) {
                for (DataTime dt : times) {
                    if (dt != null) {
                        hasTimes = true;
                        break;
                    }
                }
            }
            if (!hasTimes) {
                return NO_DATA;
            }

            if (time == null && resource instanceof BestResResource == false) {
                return name + " " + NOT_LOADED;
            } else if (time == null) {
                return NO_DATA;
            }
        }

        if (resource.hasCapability(EditableCapability.class)
                && resource.getCapability(EditableCapability.class)
                        .isEditable()) {
            name += " (Editable) ";
        }

        if (time != null && !resource.isTimeAgnostic() && includeTime) {
            name += "  " + time.getLegendString();
        }

        AbstractDescriptor desc = (AbstractDescriptor) descriptor;
        if (!fromResourceGroup
                && desc.getTimeMatcher() instanceof D2DTimeMatcher) {
            if (resource == ((D2DTimeMatcher) desc.getTimeMatcher())
                    .getTimeMatchBasis()) {
                name = "* " + name;
            }
        }

        if (this.mode == LegendMode.SHORT_PRODUCT && !fromResourceGroup
                && name.length() > 20) {
            int beginIndex = name.length() - 20;
            name = name.substring(beginIndex, name.length());
        }

        return name;
    }

    private DataTime getDataTimeForResource(AbstractVizResource<?, ?> rsc,
            IDescriptor descriptor, FramesInfo info) {
        return info.getTimeForResource(rsc);
    }

    private String getResourceGroupLegendString(
            AbstractVizResource<?, ?> rscGroup, IDescriptor descriptor,
            FramesInfo info) {
        String groupName = rscGroup.getName();
        String s = "";
        ResourceList list = ((IResourceGroup) rscGroup.getResourceData())
                .getResourceList();
        boolean tmb = false;
        AbstractVizResource<?, ?> basis = null;
        AbstractDescriptor desc = (AbstractDescriptor) descriptor;
        if (desc.getTimeMatcher() instanceof D2DTimeMatcher) {
            basis = ((D2DTimeMatcher) desc.getTimeMatcher())
                    .getTimeMatchBasis();
        }
        DataTime timeToUse = null;
        if (rscGroup.hasCapability(GroupNamingCapability.class) == false
                || groupName == null) {
            for (ResourcePair rp : list) {
                if (rp.getResource() != null && rp.getProperties().isVisible()) {
                    String name = getLegendString(rp, descriptor, info, true,
                            false);
                    if (timeToUse == null) {
                        timeToUse = getDataTimeForResource(rp.getResource(),
                                descriptor, info);
                    }
                    if (s.length() == 0) {
                        s += name;
                    } else if (s.equals(NO_DATA)) {
                        s = name;
                    } else if (!s.contains(name) && !name.equals(NO_DATA)) {
                        s += " + " + name;
                    }
                }

                if (!tmb && rp.getResource() != null
                        && rp.getResource() == basis) {
                    tmb = true;
                }
            }

        } else {
            for (ResourcePair rp : list) {
                if (rp.getResource() != null && rp.getResource() == basis) {
                    tmb = true;
                    break;
                }
            }

            if (rscGroup.isTimeAgnostic()) {
                // Group name is not null, we have the group naming capability,
                // and
                // we are time agnostic, set s to group name
                s = groupName;
            } else {
                // Group name is not null, we have the group naming capability,
                // and
                // we are NOT time agnostic look at each resource and see if
                // their
                // time is that of the descriptor's frame
                DataTime currTime = info.getFrameTimes()[info.getFrameIndex()];
                for (ResourcePair rp : list) {
                    DataTime rscTime = descriptor.getTimeForResource(rp
                            .getResource());
                    if (currTime.equals(rscTime)) {
                        s = groupName + " " + currTime.getLegendString();
                        break;
                    }
                }

                if ("".equals(s)) {
                    // None of our grouped resources have time as time match
                    // basis,
                    // grab first time that is not null in resource list
                    for (ResourcePair rp : list) {
                        DataTime rscTime = descriptor.getTimeForResource(rp
                                .getResource());
                        if (rscTime != null) {
                            s = groupName + " " + currTime.getLegendString();
                            break;
                        }
                    }
                }
            }
        }

        if ("".equals(s)) {
            s = NO_DATA;
        } else if (timeToUse != null) {
            s += " " + timeToUse.getLegendString();
        }

        if (tmb) {
            s = "* " + s;
        }

        return s;
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        if (legendFont != null) {
            legendFont.dispose();
        }
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(legendHandler);
            container.unregisterMouseHandler(changeModeHandler);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        legendFont = target.initializeFont(D2DLegendResource.class.getName());
        IDisplayPaneContainer rc = getResourceContainer();
        if (rc != null) {
            rc.registerMouseHandler(legendHandler,
                    InputPriority.SYSTEM_RESOURCE);
            rc.registerMouseHandler(changeModeHandler, InputPriority.RESOURCE);
        }
    }

    public void setLegendMode(LegendMode mode) {
        if (mode == LegendMode.HIDE) {
            this.mode = LegendMode.NONE;
        } else {
            this.mode = mode;
        }
    }

    public LegendMode getLegendMode() {
        return this.mode;
    }

    private boolean hasProducts(boolean graphics) {
        for (ResourcePair rp : descriptor.getResourceList()) {
            if (rp.getProperties().isMapLayer() == false
                    && rp.getProperties().isSystemResource() == false
                    && rp.getResource() != null
                    && rp.getResource().hasCapability(ImagingCapability.class) == !graphics) {
                return true;
            }
        }
        return false;
    }

    public boolean hasGraphics() {
        return hasProducts(true);
    }

    public boolean hasImages() {
        return hasProducts(false);
    }

    public boolean hasProducts() {
        return hasGraphics() || hasImages();
    }

    private boolean notInitialized(AbstractVizResource<?, ?> rsc) {
        if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
            return true;
        }

        if (rsc.getResourceData() instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) rsc.getResourceData())
                    .getResourceList()) {
                if (rp.getResource() == null
                        || rp.getProperties().isSystemResource()
                        || notInitialized(rp.getResource())) {
                    return true;
                }
            }
        }

        return false;
    }

    private float getScaledMagnification() {
        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();
        if (magnification < 0.6f) {
            magnification = 1.0f;
        } else if (magnification > 1.0f) {
            magnification = 1 + (magnification / 4.0f);
        }
        return magnification;
    }

    /**
     * @param none
     * @return
     */
    protected ChangeLegendModeAction getLegendAction(LegendMode mode) {
        return new ChangeLegendModeAction(mode, this);
    }

    @Override
    protected void fillContextMenu(IMenuManager menuManager,
            ResourcePair selectedResource) {
        if (getLegendMode() != LegendMode.SHORT_PRODUCT) {
            String name = selectedResource.getResource().getName();
            if (name != null && name.isEmpty() == false) {
                menuManager.add(new DummyAction(name));
            }
            TMB_ACTION.setSelectedRsc(selectedResource);
            TMB_ACTION.setContainer(getResourceContainer());
            if (TMB_ACTION.isHidden() == false) {
                menuManager.add(TMB_ACTION);
            }
            menuManager.add(new Separator());
            super.fillContextMenu(menuManager, selectedResource);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource#checkLabelSpace
     * (com.raytheon.uf.viz.core.drawables.IDescriptor,
     * com.raytheon.uf.viz.core.IGraphicsTarget, double, double)
     */
    @Override
    protected ResourcePair checkLabelSpace(IDescriptor descriptor,
            IGraphicsTarget target, double x, double y) {
        // NOTE: Overridden so legend handlers can call
        return super.checkLabelSpace(descriptor, target, x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource#checkYLabelSpace
     * (com.raytheon.uf.viz.core.drawables.IDescriptor,
     * com.raytheon.uf.viz.core.IGraphicsTarget, double, double)
     */
    @Override
    protected ResourcePair[] checkYLabelSpace(IDescriptor descriptor,
            IGraphicsTarget target, double y, double ratio) {
        // NOTE: Overridden so legend handlers can call
        return super.checkYLabelSpace(descriptor, target, y, ratio);
    }

}
