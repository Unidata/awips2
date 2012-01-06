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
package com.raytheon.uf.viz.xy.hodo;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class HodographRenderableDisplay extends AbstractRenderableDisplay
        implements AddListener, RemoveListener {
    public static final GenericResourceData backgroundRscData = new GenericResourceData(
            HodographBackgroundResource.class);

    private IRenderableDisplay parentDisplay;

    private List<IHodographResource> resources;

    public HodographRenderableDisplay() {
        super();
        resources = new ArrayList<IHodographResource>();
    }

    public IRenderableDisplay getParentDisplay() {
        return parentDisplay;
    }

    public void setParentDisplay(IRenderableDisplay parentDisplay) {
        this.resources.clear();
        if (this.parentDisplay != null) {
            this.parentDisplay.getDescriptor().getResourceList()
                    .removePostAddListener(this);
            this.parentDisplay.getDescriptor().getResourceList()
                    .removePreRemoveListener(this);
        }
        this.parentDisplay = parentDisplay;
        for (ResourcePair rp : parentDisplay.getDescriptor().getResourceList()) {
            addResource(rp);
        }
        this.parentDisplay.getDescriptor().getResourceList()
                .addPostAddListener(this);
        this.parentDisplay.getDescriptor().getResourceList()
                .addPreRemoveListener(this);
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        addResource(rp);
    }

    private void addResource(ResourcePair rp) {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc != null && rsc instanceof IHodographResource) {
            resources.add((IHodographResource) rsc);
        }
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        resources.remove(rp.getResource());
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);

        for (ResourcePair pair : descriptor.getResourceList()) {
            if (pair.getResource() == null) {
                continue;
            }
            pair.getResource().paint(target, paintProps);
        }
        for (IHodographResource rsc : resources) {
            if (rsc instanceof AbstractVizResource
                    && !((AbstractVizResource<?, ?>) rsc).getProperties()
                            .isVisible()) {
                continue;
            }
            rsc.paintHodograph(target, paintProps,
                    (HodographDescriptor) getDescriptor());
        }
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        resourceList.add(ResourcePair
                .constructSystemResourcePair(backgroundRscData));
        super.customizeResourceList(resourceList);

    }

}
