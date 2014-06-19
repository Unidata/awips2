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
package com.raytheon.uf.viz.xy.varheight.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.ui.AbstractHeightDisplay;
import com.raytheon.uf.viz.xy.map.rsc.GraphResource;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData.OverlayMode;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScales;

/**
 * Renderable display for var height
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2010            bsteffen    Initial creation
 * Jun 18, 2014 3242       njensen     Removed unused imports
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class VarHeightRenderableDisplay extends AbstractHeightDisplay {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VarHeightRenderableDisplay.class);

    public VarHeightRenderableDisplay() {
        this(new PixelExtent(0, 1000, 0, 1000));
    }

    public VarHeightRenderableDisplay(PixelExtent aPixelExtent) {
        super(aPixelExtent, new VarHeightDescriptor(aPixelExtent));
    }

    /**
     * @return the scale
     */
    @Override
    public String getScale() {
        if (getDescriptor() != null && getDescriptor().getHeightScale() != null) {
            return getDescriptor().getHeightScale().getName();
        }
        return null;
    }

    /**
     * @param scale
     *            the scale to set
     */
    @Override
    public void setScale(String scale) {
        setHeightScale(HeightScales.fromName(scale));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.ui.AbstractHeightDisplay#setHeightScale(com.raytheon
     * .viz.core.slice.request.HeightScale)
     */
    @Override
    public void setHeightScale(HeightScale scale) {
        getDescriptor().setHeightScale(scale);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#getDescriptor
     * ()
     */
    @Override
    public VarHeightDescriptor getDescriptor() {
        return (VarHeightDescriptor) super.getDescriptor();
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        super.customizeResourceList(resourceList);

        // Add graph resource
        GraphResourceData grd = new GraphResourceData("Var Height background");
        GraphResource gr = null;
        LoadProperties lprops = new LoadProperties();
        ResourceProperties rprops = new ResourceProperties();
        rprops.setMapLayer(true);
        try {
            gr = grd.construct(lprops, getDescriptor());
            grd.setOverlayMode(OverlayMode.OVERLAY);
            ResourcePair rp = new ResourcePair();
            rp.setResourceData(grd);
            rp.setResource(gr);
            rp.setProperties(rprops);
            rp.setLoadProperties(lprops);
            resourceList.add(rp);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error constructing var height Graph", e);
        }
    }

}
