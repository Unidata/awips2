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
package com.raytheon.viz.mpe.ui.displays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.AddPseudoGageResource;
import com.raytheon.viz.mpe.ui.rsc.IMpeResource;
import com.raytheon.viz.mpe.ui.rsc.MPEGageResource;
import com.raytheon.viz.mpe.ui.rsc.MPELegendResource;
import com.raytheon.viz.mpe.ui.rsc.MPEPolygonResource;

/**
 * Performs map display duties for MPE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2009            randerso     Initial creation
 * Feb 17, 2010  4141     snaples      Added annotations
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "mpeMapRenderableDisplay")
public class MPEMapRenderableDisplay extends PlainMapRenderableDisplay {

    public MPEMapRenderableDisplay() {
        super();
    }

    public MPEMapRenderableDisplay(MapDescriptor desc) {
        super(desc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#setDescriptor
     * (com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(IDescriptor desc) {
        super.setDescriptor(desc);
        if (desc.getTimeMatcher() == null) {
            desc.setTimeMatcher(new MPETimeMatcher());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        MPEDisplayManager.disposeDisplay(this);
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        super.customizeResourceList(resourceList);
        // Add the polygon resource
        resourceList.add(ResourcePair
                .constructSystemResourcePair(new GenericResourceData(
                        MPEPolygonResource.class)));

        // Add the legend resource:
        resourceList.add(ResourcePair
                .constructSystemResourcePair(new GenericResourceData(
                        MPELegendResource.class)));

        // Add the add pseudo gage resource
        resourceList.add(ResourcePair
                .constructSystemResourcePair(new GenericResourceData(
                        AddPseudoGageResource.class)));

        // Add the gage resource:
        resourceList.add(ResourcePair
                .constructSystemResourcePair(new GenericResourceData(
                        MPEGageResource.class)));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#clear(com
     * .raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public void clear() {
        ResourceList list = descriptor.getResourceList();
        for (AbstractVizResource<?, ?> rsc : list
                .getResourcesByType(IMpeResource.class)) {
            if (rsc.getProperties().isSystemResource() == false) {
                list.removeRsc(rsc);
            }
        }
    }

}
