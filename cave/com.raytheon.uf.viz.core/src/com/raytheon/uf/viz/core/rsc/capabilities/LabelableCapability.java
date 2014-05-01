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
package com.raytheon.uf.viz.core.rsc.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Capability for labeling or not labeling a resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class LabelableCapability extends AbstractCapability {
    @XmlAttribute(required = false)
    private String labelField;

    @XmlAttribute(required = false)
    private int xOffset;

    /**
     * @return the xOffset
     */
    public int getxOffset() {
        return xOffset;
    }

    /**
     * @param xOffset
     *            the xOffset to set
     */
    public void setxOffset(int xOffset) {
        this.xOffset = xOffset;
        this.capabilityChanged();
    }

    /**
     * @return the yOffset
     */
    public int getyOffset() {
        return yOffset;
    }

    /**
     * @param yOffset
     *            the yOffset to set
     */
    public void setyOffset(int yOffset) {
        this.yOffset = yOffset;
        this.capabilityChanged();
    }

    @XmlAttribute(required = false)
    private int yOffset;

    private String[] availableLabelFields;

    public String[] getAvailableLabelFields() {
        return availableLabelFields;
    }

    public void setAvailableLabelFields(String... availableLabelFields) {
        this.availableLabelFields = availableLabelFields;
    }

    /**
     * @return the labelField
     */
    public String getLabelField() {
        return labelField;
    }

    /**
     * @param labelField
     *            the labelField to set
     */
    public void setLabelField(String labelField) {
        if ((this.labelField == null && labelField != null)
                || (this.labelField != null && !this.labelField
                        .equals(labelField))) {
            this.labelField = labelField;
            this.capabilityChanged();
        }
    }

    @Override
    public AbstractCapability clone() {
        LabelableCapability lc = new LabelableCapability();
        lc.labelField = labelField;
        lc.availableLabelFields = availableLabelFields;
        return lc;
    }

}
