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
/**
 * 
 */
package com.raytheon.uf.viz.core.rsc;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.ResourcePair;

/**
 * @author randerso
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class ResourceGroup implements IResourceGroup {

    protected ResourceList resourceList;

    public ResourceGroup() {
        resourceList = new ResourceList();
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

    @XmlElement(name = "resource")
    public ResourcePair[] getSerializableResources() {
        if (resourceList != null) {
            List<ResourcePair> rps = new ArrayList<ResourcePair>(
                    resourceList.size());
            for (ResourcePair rp : resourceList) {
                if (rp.getProperties().isSystemResource() == false) {
                    rps.add(rp);
                }
            }
            return rps.toArray(new ResourcePair[rps.size()]);
        }
        return null;
    }

    public void setSerializableResources(ResourcePair[] resources) {
        for (ResourcePair rp : resources) {
            resourceList.add(rp);
        }
    }
}
