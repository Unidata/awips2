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

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.core.rsc.BlendedResource;
import com.raytheon.viz.core.rsc.BlendedResourceData;

/**
 * Use BlendedResourceData instead, it does the exact same thing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2010 684        mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Deprecated
public class BlendedRadarResourceData extends BlendedResourceData implements
        IResourceGroup {

    public BlendedRadarResourceData() {
        resourceList = new ResourceList();
        nameGenerator = new RadarNameGenerator();
    }

    @Override
    public BlendedResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        BlendedResourceData brd = new BlendedResourceData(this);
        return brd.construct(loadProperties, descriptor);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if ((obj == null)
                || ((obj instanceof BlendedRadarResourceData) == false)) {
            return false;
        }
        BlendedRadarResourceData other = (BlendedRadarResourceData) obj;

        if ((resourceList != null) && (other.resourceList == null)) {
            return false;
        } else if ((resourceList == null) && (other.resourceList != null)) {
            return false;
        } else if ((resourceList != null)
                && (resourceList.equals(other.resourceList) == false)) {
            return false;
        }

        return true;
    }
}
