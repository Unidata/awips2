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
package com.raytheon.uf.viz.core.maps.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * ResourceData for Resource Groups
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MapResourceGroupData extends AbstractResourceData implements
        IResourceGroup {

    @XmlElement(name = "resource")
    private ResourceList resourceList;

    @XmlElement
    private String mapName;

    public MapResourceGroupData() {
        this.resourceList = new ResourceList();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public MapResourceGroup construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new MapResourceGroup(this, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IResourceGroup#getResourceList()
     */
    @Override
    public ResourceList getResourceList() {
        return resourceList;
    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof MapResourceGroupData == false) {
            return false;
        }
        MapResourceGroupData other = (MapResourceGroupData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && this.resourceList.equals(other.resourceList) == false) {
            return false;
        }

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && this.mapName.equals(other.mapName) == false) {
            return false;
        }

        return true;
    }

}
