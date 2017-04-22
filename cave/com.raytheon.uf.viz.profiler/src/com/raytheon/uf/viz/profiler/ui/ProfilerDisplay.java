package com.raytheon.uf.viz.profiler.ui;

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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.swt.layout.FormData;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.core.map.D2DColorBarResource;
import com.raytheon.uf.viz.d2d.ui.AbstractNonMapDisplay;
import com.raytheon.uf.viz.profiler.ProfilerDescriptor;
import com.raytheon.viz.core.graphing.GraphProperties;

/**
 * 
 * Draws background static structure of Profiler
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09 APR 2009  2129       dhladky     initial
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "profilerDisplay")
@XmlRootElement
public class ProfilerDisplay extends AbstractNonMapDisplay {

    IGraphicsTarget target = null;

    PaintProperties paintProps = null;

    public ProfilerDisplay() {
        super(new PixelExtent(0, 1000, 0, 1000), new ProfilerDescriptor());
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);
        GraphProperties graphProps = new GraphProperties(paintProps);

        for (ResourcePair rp : getDescriptor().getResourceList()) {
            graphProps = (GraphProperties) calcPaintDataTime(graphProps,
                    rp.getResource());
            if (rp.getResource() != null && rp.getProperties().isVisible()) {
                rp.getResource().paint(target, graphProps);
            }
        }
    }

    @Override
    public FormData getInsetMapLocation() {
        return null;
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        super.customizeResourceList(resourceList);
        // TODO: Make it so profiler uses d2d colorbar resource
        List<D2DColorBarResource> rscs = resourceList
                .getResourcesByTypeAsType(D2DColorBarResource.class);
        for (D2DColorBarResource rsc : rscs) {
            resourceList.removeRsc(rsc);
        }
    }

}
