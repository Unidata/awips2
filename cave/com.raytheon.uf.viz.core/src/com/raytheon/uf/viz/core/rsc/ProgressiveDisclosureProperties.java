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

package com.raytheon.uf.viz.core.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Defines the progressive disclosure properties
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 30, 2007           randerso    Initial Creation.
 * Oct 22, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ProgressiveDisclosureProperties {
    /**
     * Minimum display width in meters for the associated resource to be
     * displayed
     */
    @XmlAttribute
    private int minDisplayWidth = 0;

    /**
     * Maximum display width in meters for the associated resource to be
     * displayed
     */
    @XmlAttribute
    private int maxDisplayWidth = 100000000;

    /**
     * @return the minDisplayWidth in meters
     */
    public int getMinDisplayWidth() {
        return minDisplayWidth;
    }

    /**
     * @return the maxDisplayWidth in meters
     */
    public int getMaxDisplayWidth() {
        return maxDisplayWidth;
    }

    /**
     * @param minDisplayWidth
     *            the minDisplayWidth in meters
     */
    public void setMinDisplayWidth(int minDisplayWidth) {
        this.minDisplayWidth = minDisplayWidth;
    }

    /**
     * @param maxDisplayWidth
     *            the maxDisplayWidth in meters
     */
    public void setMaxDisplayWidth(int maxDisplayWidth) {
        this.maxDisplayWidth = maxDisplayWidth;
    }

    /**
     * Determine if the resource should be displayed at the current display
     * width
     * 
     * @param displayWidth
     *            current display width in meters
     * @return true if the resource should be displayed
     */
    public boolean isDisclosed(int displayWidth) {
        return (displayWidth >= minDisplayWidth)
                && (displayWidth <= maxDisplayWidth);
    }
}
