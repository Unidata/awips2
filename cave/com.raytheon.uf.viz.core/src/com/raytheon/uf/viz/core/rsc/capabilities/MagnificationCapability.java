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

import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MagnificationCapability extends AbstractCapability {

    private static final double[] DEFAULT_MAGNIFICATION_VALUES = { 0.0, 0.8,
            1.0, 1.25, 1.5, 2.0, 2.5 };

    @XmlAttribute
    private Double magnification;

    private transient double[] magnificationValues;

    public MagnificationCapability() {
        this.magnification = (Double) VizGlobalsManager.getCurrentInstance()
                .getPropery(VizConstants.MAGNIFICATION_ID);
        magnificationValues = DEFAULT_MAGNIFICATION_VALUES;
    }

    public MagnificationCapability(MagnificationCapability that) {
        this.magnification = that.magnification;
        this.magnificationValues = that.magnificationValues;
    }

    public MagnificationCapability(Double magnification) {
        this.magnification = magnification;
        magnificationValues = DEFAULT_MAGNIFICATION_VALUES;
    }

    public MagnificationCapability(Double magnification,
            double[] magnificationValues) {
        this.magnification = magnification;
        this.magnificationValues = magnificationValues;
    }

    public String getMagnificationString() {
        return getMagnificationAsString(magnification);
    }

    /**
     * @return the magnification
     */
    public Double getMagnification() {
        return magnification;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(Double magnification) {
        if (magnification != this.magnification) {
            this.magnification = magnification;
            capabilityChanged();
        }
    }

    /**
     * @return the magnificationValues
     */
    public double[] getMagnificationValues() {
        return magnificationValues;
    }

    /**
     * @param magnificationValues
     *            the magnificationValues to set
     */
    public void setMagnificationValues(double[] magnificationValues) {
        this.magnificationValues = magnificationValues;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability#clone()
     */
    @Override
    public MagnificationCapability clone() {
        return new MagnificationCapability(this);
    }

    public static String getMagnificationAsString(Double magnification) {
        if (magnification.intValue() == magnification.doubleValue()) {
            return "" + magnification.intValue();
        } else {
            return Double.toString(magnification);
        }
    }

}
