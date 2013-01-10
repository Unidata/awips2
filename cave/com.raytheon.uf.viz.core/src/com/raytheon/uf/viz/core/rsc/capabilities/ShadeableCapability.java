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
 * Capability for shading or not shading a resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ShadeableCapability extends AbstractCapability {
    @XmlAttribute(required = false)
    private String shadingField;

    @XmlAttribute(required = false)
    private float opacity = 1.0f;

    private String[] availableShadingFields;

    public String[] getAvailableShadingFields() {
        return availableShadingFields;
    }

    public void setAvailableShadingFields(String[] availableShadingFields) {
        this.availableShadingFields = availableShadingFields;
    }

    /**
     * @return the shadingField
     */
    public String getShadingField() {
        return shadingField;
    }

    /**
     * @param shadingField
     *            the shadingField to set
     */
    public void setShadingField(String shadingField) {
        if ((this.shadingField == null && shadingField != null)
                || (this.shadingField != null && !this.shadingField
                        .equals(shadingField))) {
            this.shadingField = shadingField;
            this.capabilityChanged();
        }
    }

    public float getOpacity() {
        return opacity;
    }

    public void setOpacity(float opacity) {
        if (this.opacity != opacity) {
            this.opacity = opacity;
            this.capabilityChanged();
        }
    }

    @Override
    public AbstractCapability clone() {
        ShadeableCapability sc = new ShadeableCapability();
        sc.shadingField = shadingField;
        sc.opacity = opacity;
        sc.availableShadingFields = availableShadingFields;
        return sc;
    }

}
