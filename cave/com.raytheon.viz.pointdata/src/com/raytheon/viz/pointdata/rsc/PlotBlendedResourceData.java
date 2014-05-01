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
package com.raytheon.viz.pointdata.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Resource data for plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2009             jsanchez    Initial creation
 * ======================================
 * AWIPS2 DR Work
 * 20120913           1172 jkorman     Added code to call postAddListeners when
 *                                     creating sub-resources.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

@XmlRootElement(name = "plotBlendedResource")
@XmlAccessorType(XmlAccessType.NONE)
public class PlotBlendedResourceData extends AbstractResourceData implements
        IResourceGroup {

    @XmlElement(name = "resource")
    protected ResourceList resourceList;

    private static final String NO_DATA_AVAILABLE = "No Data Available";

    public PlotBlendedResourceData() {
        this.resourceList = new ResourceList();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                String s = NO_DATA_AVAILABLE;
                if (resourceList.size() > 0) {
                    s = resourceList.get(0).getResource().getName();
                }
                return s;
            }

        };

    }

    /**
     * Constructs a resource(s) from this resource data.
     * 
     * @param loadProperties
     *            The load properties
     * @param descriptor
     *            The descriptor that the resource will be loaded onto
     * @throws VizException
     *             if construction fails
     * @return The renderable capability. Will return null if any of the sub-resources
     * fail to construct.
     */
    @Override
    public PlotBlendedResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        PlotBlendedResource rsc = new PlotBlendedResource(this, loadProperties);

        for (ResourcePair rp : resourceList) {
            if (!rp.instantiateResource(descriptor, false)) {
                // failure to create any sub-resource is a failure to construct
                // the blended resource.
                return null;
            }
        }
        // All sub-resources have been instantiated, add the listeners. 
        for (ResourcePair rp : resourceList) {
            addChangeListener((IResourceDataChanged) rp.getResource());
            resourceList.firePostAddListeners(rp);
        }
        return rsc;
    }

    @Override
    public void update(Object updateData) {
        // Updating should be modified to update the children directly
    }

    /**
     * @return the resourceList
     */
    public ResourceList getResourceList() {
        return resourceList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#configure(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void configure(LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        super.configure(loadProperties, descriptor);
        for (ResourcePair rp : resourceList) {
            rp.getResourceData().configure(rp.getLoadProperties(), descriptor);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof PlotBlendedResourceData == false) {
            return false;
        }
        PlotBlendedResourceData other = (PlotBlendedResourceData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && this.resourceList.equals(other.resourceList) == false) {
            return false;
        }

        return true;
    }

}
