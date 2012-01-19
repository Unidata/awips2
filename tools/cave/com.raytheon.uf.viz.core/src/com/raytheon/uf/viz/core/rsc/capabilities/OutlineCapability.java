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

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

/**
 * TODO Add Description
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
public class OutlineCapability extends AbstractCapability {

    @XmlAttribute
    private int outlineWidth = 1;

    @XmlAttribute
    private boolean outlineOn = true;

    @XmlAttribute
    private LineStyle lineStyle = LineStyle.DEFAULT;

    /**
     * @return the outlineWidth
     */
    public int getOutlineWidth() {
        return outlineWidth;
    }

    /**
     * @param outlineWidth
     *            the outlineWidth to set
     */
    public void setOutlineWidth(int outlineWidth) {
        if (this.outlineWidth != outlineWidth) {
            this.outlineWidth = outlineWidth;
            this.capabilityChanged();
        }
    }

    /**
     * @return the outlineOn
     */
    public boolean isOutlineOn() {
        return outlineOn;
    }

    /**
     * @param outlineOn
     *            the outlineOn to set
     */
    public void setOutlineOn(boolean outlineOn) {
        if (this.outlineOn != outlineOn) {
            this.outlineOn = outlineOn;
            this.capabilityChanged();
        }
    }

    /**
     * @return the lineStyle
     */
    public LineStyle getLineStyle() {
        return lineStyle;
    }

    /**
     * @param lineStyle
     *            the lineStyle to set
     */
    public void setLineStyle(LineStyle lineStyle) {
        if ((this.lineStyle == null) && (lineStyle == null)) {
            return;
        }
        if ((this.lineStyle == null) || !this.lineStyle.equals(lineStyle)) {
            this.lineStyle = lineStyle;
            this.capabilityChanged();
        }
    }

    public void setLineStyle(String lineStyle) {
        setLineStyle(LineStyle.valueOf(lineStyle));
    }

    @Override
    public AbstractCapability clone() {
        OutlineCapability oc = new OutlineCapability();
        oc.outlineOn = outlineOn;
        oc.outlineWidth = outlineWidth;
        oc.lineStyle = lineStyle;
        return oc;
    }

}
