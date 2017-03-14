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
package com.raytheon.uf.viz.daylight.transition.resource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.core.rsc.BlendedResourceData;
import com.raytheon.viz.satellite.rsc.SatBlendedResource;
import com.raytheon.viz.satellite.rsc.SatBlendedResourceData;
import com.raytheon.viz.satellite.rsc.SatResourceData;

/**
 * 
 * Serializable data for {@link DaylightTransitionBlendedResource}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DaylightTransitionBlendedResourceData extends BlendedResourceData {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(DaylightTransitionBlendedResourceData.class);

    @XmlElement
    private int transitionIndex = 1;

    @XmlElement
    private int sunDelta = 75;

    public DaylightTransitionBlendedResourceData() {

    }

    public DaylightTransitionBlendedResourceData(BlendedResourceData blended) {
        this.resourceList = blended.getResourceList();
        applyTransitionIndex();
    }

    private AbstractResourceData convertResourceData(AbstractResourceData data) {
        if (data instanceof SatBlendedResourceData) {
            SatBlendedResourceData sbrd1 = (SatBlendedResourceData) data;
            SatBlendedResourceData sbrd2 = new SatBlendedResourceData();
            for (ResourcePair pair : sbrd1.getResourceList()) {
                ResourcePair newPair = new ResourcePair();
                newPair.setResourceData(convertResourceData(pair
                        .getResourceData()));
                newPair.setProperties(pair.getProperties());
                newPair.setLoadProperties(pair.getLoadProperties());
                sbrd2.getResourceList().add(newPair);
            }
            return sbrd2;
        } else if (data instanceof SatResourceData) {
            return new DaylightTransitionSatResourceData((SatResourceData) data);
        } else {
            return data;
        }
    }

    public BlendedResourceData toBlended() {
        BlendedResourceData blended = new BlendedResourceData();
        ResourceList resourceList = blended.getResourceList();
        for (ResourcePair pair : this.getResourceList()) {
            ResourcePair newPair = new ResourcePair();
            newPair.setResourceData(unconvertResourceData(pair
                    .getResourceData()));
            resourceList.add(pair);
        }
        return blended;
    }

    private AbstractResourceData unconvertResourceData(AbstractResourceData data) {
        if (data instanceof SatBlendedResourceData) {
            SatBlendedResourceData sbrd1 = (SatBlendedResourceData) data;
            SatBlendedResourceData sbrd2 = new SatBlendedResourceData();
            for (ResourcePair pair : sbrd1.getResourceList()) {
                ResourcePair newPair = new ResourcePair();
                newPair.setResourceData(unconvertResourceData(pair
                        .getResourceData()));
                newPair.setProperties(pair.getProperties());
                newPair.setLoadProperties(pair.getLoadProperties());
                sbrd2.getResourceList().add(newPair);
            }
            return sbrd2;
        } else if (data instanceof DaylightTransitionSatResourceData) {
            return ((DaylightTransitionSatResourceData) data)
                    .toSatResourceData();
        } else {
            return data;
        }
    }

    @Override
    public DaylightTransitionBlendedResource construct(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        for (ResourcePair pair : resourceList) {
            updateSunDelta(pair);
        }
        return new DaylightTransitionBlendedResource(this, loadProperties);
    }

    private void updateSunDelta(ResourcePair pair) {
        if (pair.getResourceData() instanceof SatBlendedResourceData) {
            SatBlendedResourceData sbrd = (SatBlendedResourceData) pair
                    .getResourceData();
            for (ResourcePair rp : sbrd.getResourceList()) {
                updateSunDelta(rp);
            }
            SatBlendedResource resource = (SatBlendedResource) pair
                    .getResource();
            if (resource != null) {
                resource.recycle();
                TimeMatchingJob.scheduleTimeMatch(resource.getDescriptor());
            }
        } else if (pair.getResourceData() instanceof DaylightTransitionSatResourceData) {
            DaylightTransitionSatResourceData dtsrd = (DaylightTransitionSatResourceData) pair
                    .getResourceData();
            dtsrd.setSunDelta(sunDelta);
        }
    }

    public int getTransitionIndex() {
        return transitionIndex;
    }

    public void setTransitionIndex(int transitionIndex) {
        this.transitionIndex = transitionIndex;
    }

    public void applyTransitionIndex() {
        int index = 0;
        for (ResourcePair pair : this.resourceList) {
            if (index == transitionIndex) {
                pair.setResourceData(convertResourceData(pair
                        .getResourceData()));
            } else {
                pair.setResourceData(unconvertResourceData(pair
                        .getResourceData()));
            }
            if (pair.getResource() != null) {
                IDescriptor descriptor = pair.getResource().getDescriptor();
                pair.getResource().dispose();
                pair.setResource(null);
                try {
                    pair.instantiateResource(descriptor, false);
                } catch (VizException e) {
                    logger.error("Unable to create new resource.", e);
                }
            }

            index += 1;
        }
    }

    public int getSunDelta() {
        return sunDelta;
    }

    public void setSunDelta(int sunDelta) {
        this.sunDelta = sunDelta;
        if (resourceList != null) {
            for (ResourcePair pair : resourceList) {
                updateSunDelta(pair);
            }
        }
    }

}
