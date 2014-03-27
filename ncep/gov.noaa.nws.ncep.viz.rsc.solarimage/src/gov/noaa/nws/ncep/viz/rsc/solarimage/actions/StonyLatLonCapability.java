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
package gov.noaa.nws.ncep.viz.rsc.solarimage.actions;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * implementation for describing persistable capabilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer             Description
 * ------------ ---------- -----------          --------------------------
 * 02/21/2013   958        qzhou, sgurung       Initial creation
 * 
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class StonyLatLonCapability extends AbstractCapability {

    private String interval = MenuConstants.latLonIntervals[0];

    /**
     * @return the outlineWidth
     */
    public String getInterval() {
        return interval;
    }

    /**
     * @param outlineWidth
     *            the outlineWidth to set
     */
    public void setInterval(String interval) {
        if (this.interval != interval) {
            this.interval = interval;
            this.capabilityChanged();
        }
    }

    @Override
    public AbstractCapability clone() {
        StonyLatLonCapability oc = new StonyLatLonCapability();
        oc.interval = interval;
        return oc;
    }

}
