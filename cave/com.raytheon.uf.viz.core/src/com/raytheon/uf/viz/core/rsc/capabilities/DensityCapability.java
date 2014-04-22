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
 * A capability representing density of displayed items
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2010            bsteffen    Initial creation
 * Mar 03, 2014 2792       njensen     Enforce getDensity() will never return
 *                                        above MAX
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DensityCapability extends AbstractCapability {

    public static final Double MAX_THRESHOLD = 4.0;

    private static final double[] DEFAULT_DENSITY_VALUES = { 0.0, 0.33, 0.5,
            0.67, 1.0, 1.25, 1.5, 2.0, 99999.0 };

    @XmlAttribute
    private Double density;

    private transient double[] densityValues;

    public DensityCapability() {
        this.density = (Double) VizGlobalsManager.getCurrentInstance()
                .getPropery(VizConstants.DENSITY_ID);
        densityValues = DEFAULT_DENSITY_VALUES;
    }

    public DensityCapability(DensityCapability that) {
        this.density = that.density;
        this.densityValues = that.densityValues;
    }

    public DensityCapability(Double density) {
        this.density = density;
        densityValues = DEFAULT_DENSITY_VALUES;
    }

    public DensityCapability(Double density, double[] densityValues) {
        this.density = density;
        this.densityValues = densityValues;
    }

    public String getDensityString() {
        return getDensityAsString(density);
    }

    /**
     * @return the density
     */
    public Double getDensity() {
        if (density < MAX_THRESHOLD) {
            return density;
        } else {
            return MAX_THRESHOLD;
        }
    }

    /**
     * @param density
     *            the density to set
     */
    public void setDensity(Double density) {
        if (!this.density.equals(density)) {
            this.density = density;
            this.capabilityChanged();
        }
    }

    /**
     * @return the densityValues
     */
    public double[] getDensityValues() {
        return densityValues;
    }

    /**
     * @param densityValues
     *            the densityValues to set
     */
    public void setDensityValues(double[] densityValues) {
        this.densityValues = densityValues;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability#clone()
     */
    @Override
    public DensityCapability clone() {
        return new DensityCapability(this);
    }

    public static String getDensityAsString(Double density) {
        if (density < MAX_THRESHOLD) {
            if (density.intValue() == density.doubleValue()) {
                return "" + density.intValue();
            } else {
                return Double.toString(density);
            }
        } else {
            return "Max";
        }
    }

}
