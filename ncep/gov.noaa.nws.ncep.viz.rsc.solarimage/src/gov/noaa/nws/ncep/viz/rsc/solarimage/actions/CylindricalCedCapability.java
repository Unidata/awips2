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
 * Date         Ticket#    Engineer       Description
 * ------------ ---------- -----------    --------------------------
 * 04/02/2013   958        qzhou          Initial creation
 * 
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CylindricalCedCapability extends AbstractCapability {

    private String cylind = MenuConstants.projections[0];

    /**
     * @return the outlineWidth
     */
    public String getCylind() {
        return cylind;
    }

    /**
     * @param outlineWidth
     *            the outlineWidth to set
     */
    public void setCylindrical(String cylind) {
        if (this.cylind != cylind) {
            this.cylind = cylind;
            this.capabilityChanged();
        }
    }

    @Override
    public AbstractCapability clone() {
        CylindricalCedCapability oc = new CylindricalCedCapability();
        oc.cylind = cylind;
        return oc;
    }

}
