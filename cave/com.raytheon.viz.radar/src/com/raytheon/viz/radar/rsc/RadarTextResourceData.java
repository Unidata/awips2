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
package com.raytheon.viz.radar.rsc;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RadarTextResourceData extends AbstractResourceData {

    private static Map<IDescriptor, RadarTextResource> resourceMap = new HashMap<IDescriptor, RadarTextResource>();

    public static void addRadarTextResource(IDescriptor descriptor) {
        synchronized (RadarTextResourceData.class) {
            RadarTextResource rtr = resourceMap.get(descriptor);
            if (rtr == null) {
                RadarTextResourceData rtrd = new RadarTextResourceData();
                ResourcePair rp = new ResourcePair();
                ResourceProperties props = new ResourceProperties();
                props.setSystemResource(true);
                rp.setProperties(props);
                rp.setLoadProperties(new LoadProperties());
                rp.setResourceData(rtrd);
                descriptor.getResourceList().add(rp);
                descriptor.getResourceList().instantiateResources(descriptor,
                        true);
                resourceMap.put(descriptor,
                        (RadarTextResource) rp.getResource());
            } else {
                rtr.incrementRefCount();
            }
        }
    }

    public static void removeRadarTextResource(IDescriptor descriptor) {
        synchronized (RadarTextResourceData.class) {
            RadarTextResource rsc = resourceMap.get(descriptor);
            if (rsc != null) {
                rsc.decrementRefCount();
                if (rsc.getReferenceCount() == 0) {
                    descriptor.getResourceList().removeRsc(rsc);
                    resourceMap.remove(descriptor);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        synchronized (RadarTextResourceData.class) {
            RadarTextResource rsc = new RadarTextResource(this, loadProperties);
            resourceMap.put(descriptor, rsc);
            return rsc;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return true;
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
        // ignore updates
    }

}
