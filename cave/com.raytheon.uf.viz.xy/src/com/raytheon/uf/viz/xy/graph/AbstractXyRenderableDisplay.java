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
package com.raytheon.uf.viz.xy.graph;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.xy.VizXyEditor;
import com.raytheon.uf.viz.xy.map.rsc.GraphResource;

/**
 * Abstract class for xy renderable displays, will eventually handle common
 * graph drawing functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public abstract class AbstractXyRenderableDisplay extends
        AbstractRenderableDisplay {

    @XmlAttribute
    private String tabTitle;

    private IExtent worldExtent;

    public AbstractXyRenderableDisplay() {
        super();
    }

    public AbstractXyRenderableDisplay(IExtent extent, IDescriptor descriptor) {
        super(extent, descriptor);
        this.worldExtent = extent;
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);
        if (getDescriptor() instanceof XyGraphDescriptor == false) {
            return;
        }
        GraphProperties gProps = new GraphProperties(paintProps);
        gProps.setWorldExtent(worldExtent);
        GraphResource gRsc = ((XyGraphDescriptor) getDescriptor())
                .getGraphResource();
        if (gRsc != null && gRsc.getStatus() == ResourceStatus.NEW) {
            gRsc.init(target);
        }

        for (ResourcePair rp : getDescriptor().getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc == null || rp.getProperties().isVisible() == false) {
                continue;
            }
            if (rsc.hasCapability(ImagingCapability.class)) {
                gProps.setAlpha(rsc.getCapability(ImagingCapability.class)
                        .getAlpha());
            }
            gProps.setDataTime(descriptor.getTimeForResource(rsc));
            try {
                rsc.paint(target, gProps);
            } catch (Throwable e) {
                rp.getProperties().setVisible(false);
                throw new VizException("Paint error: " + e.getMessage()
                        + ":: The resource has been disabled.", e);
            }

        }
    }

    public String getTabTitle() {
        return tabTitle;
    }

    public void setTabTitle(String tabTitle) {
        this.tabTitle = tabTitle;
        if (getEditor() != null) {
            getEditor().setTabTitle(tabTitle);
        }

    }

    public VizXyEditor getEditor() {
        if (getContainer() instanceof VizXyEditor) {
            return (VizXyEditor) getContainer();
        } else {
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

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        globals.put(VizConstants.FRAMES_ID, new Integer(getDescriptor()
                .getNumberOfFrames()));
        return globals;
    }
}
