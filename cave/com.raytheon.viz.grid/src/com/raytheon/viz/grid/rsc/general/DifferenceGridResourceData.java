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
package com.raytheon.viz.grid.rsc.general;

import java.util.Arrays;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * 
 * ResourceData for constructing Difference Resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DifferenceGridResourceData extends AbstractResourceData implements
        IResourceGroup {

    @XmlElement
    private ResourcePair one;

    @XmlElement
    private ResourcePair two;

    private ResourceList resourceList;

    public DifferenceGridResourceData() {
        super();
    }

    public DifferenceGridResourceData(ResourcePair one, ResourcePair two) {
        super();
        this.one = one;
        this.two = two;
    }

    @Override
    public DifferenceGridResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        // Attempt to construct the one with the most frequent times first so it
        // will be the time match basis
        int oneInterval = getTimeInterval(one);
        int twoInterval = getTimeInterval(two);
        if (twoInterval < oneInterval) {
            AbstractGridResource<?> two = construct(this.two, loadProperties,
                    descriptor);
            AbstractGridResource<?> one = construct(this.one, loadProperties,
                    descriptor);
            return new DifferenceGridResource(this, loadProperties, one, two);
        } else {
            AbstractGridResource<?> one = construct(this.one, loadProperties,
                    descriptor);
            AbstractGridResource<?> two = construct(this.two, loadProperties,
                    descriptor);
            return new DifferenceGridResource(this, loadProperties, one, two);
        }
    }

    private int getTimeInterval(ResourcePair rscPair) throws VizException {
        if (rscPair.getResourceData() instanceof AbstractRequestableResourceData) {
            AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rscPair
                    .getResourceData();
            DataTime[] time = arrd.getAvailableTimes();
            long[] validTime = new long[time.length];
            for (int i = 0; i < time.length; i++) {
                validTime[i] = time[i].getMatchValid();
            }
            Arrays.sort(validTime);
            long timeInterval = Long.MAX_VALUE;
            for (int i = 1; i < time.length; i++) {
                long curInterval = validTime[i] - validTime[i - 1];
                if (curInterval > 0 && curInterval < timeInterval) {
                    timeInterval = curInterval;
                }
            }
            return (int) SI.SECOND.getConverterTo(NonSI.MINUTE).convert(
                    timeInterval);
        } else {
            return 0;
        }
    }

    private AbstractGridResource<?> construct(ResourcePair rscPair,
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        if (rscPair.getResource() == null) {
            rscPair.instantiateResource(descriptor);
        }
        AbstractVizResource<?, ?> rsc = rscPair.getResource();
        if (!(rsc instanceof AbstractGridResource)) {
            throw new VizException("Unable to difference resource of type: "
                    + rsc.getClass().getSimpleName());
        }
        return (AbstractGridResource<?>) rsc;
    }

    @Override
    public void update(Object updateData) {

    }

    @Override
    public ResourceList getResourceList() {
        if (resourceList == null) {
            resourceList = new ResourceList();
            resourceList.add(one);
            resourceList.add(two);
        }
        return resourceList;
    }

    public ResourcePair getOne() {
        return one;
    }

    public ResourcePair getTwo() {
        return two;
    }

    public void setOne(ResourcePair one) {
        this.one = one;
    }

    public void setTwo(ResourcePair two) {
        this.two = two;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((one == null) ? 0 : one.hashCode());
        result = prime * result + ((two == null) ? 0 : two.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DifferenceGridResourceData other = (DifferenceGridResourceData) obj;
        if (one == null) {
            if (other.one != null)
                return false;
        } else if (!one.equals(other.one))
            return false;
        if (two == null) {
            if (other.two != null)
                return false;
        } else if (!two.equals(other.two))
            return false;
        return true;
    }

}
