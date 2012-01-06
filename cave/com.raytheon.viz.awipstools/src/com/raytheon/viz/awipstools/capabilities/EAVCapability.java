/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IPaintListener;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.viz.awipstools.common.EstimatedActualVelocity;
import com.raytheon.viz.awipstools.ui.layer.EstimatedActualVelocityLayer;

/**
 * Gives capability to display EAV sampling label to radial radars.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class EAVCapability extends AbstractCapability implements IPaintListener {

    private static final long MAX_LAST_LAYER_PAINT_MILLIS_DELTA = 1500;

    private volatile boolean isCapabilityActive = false;

    private EstimatedActualVelocity eav;

    private volatile long lastMillisPaintOfEAVLayer;

    /**
     * Default constructor.
     */
    public EAVCapability() {
        eav = new EstimatedActualVelocity();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability#clone()
     */
    @Override
    public AbstractCapability clone() {
        EAVCapability cap = new EAVCapability();
        return cap;
    }

    /**
     * @return the eav
     */
    public EstimatedActualVelocity getEav() {
        return eav;
    }

    /**
     * @return the isCapabilityActive
     */
    public boolean isCapabilityActive() {
        return eav != null
                && isCapabilityActive
                && ((System.currentTimeMillis() - lastMillisPaintOfEAVLayer) < MAX_LAST_LAYER_PAINT_MILLIS_DELTA);
    }

    /**
     * @param isCapabilityActive
     *            the isCapabilityActive to set
     */
    public void setCapabilityActive(boolean isCapabilityActive) {
        this.isCapabilityActive = isCapabilityActive;
    }

    @Override
    public void painted(AbstractVizResource<?, ?> resource) {
        if (resource instanceof EstimatedActualVelocityLayer) {
            lastMillisPaintOfEAVLayer = System.currentTimeMillis();
        }
    }
}
