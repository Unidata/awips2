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
package com.raytheon.viz.gfe.core.wxvalue;

import java.text.DecimalFormat;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * A WxValue encapsulates a value at a single gridpoint of scalar type.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial creation of skeleton.
 * 02/20/2008              chammack    Initial implementation.
 * 03/11/2008   879        rbell       Cleanup.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class ScalarWxValue extends WxValue {
    public static ScalarWxValue defaultValue(final Parm parm) {
        float value = parm.getGridInfo().getMinValue();
        String key = parm.getParmID().compositeNameUI() + "_defaultValue";
        if (Activator.getDefault() != null
                && Activator.getDefault().getPreferenceStore().contains(key)) {
            value = Activator.getDefault().getPreferenceStore().getFloat(key);
        }
        return new ScalarWxValue(value, parm);
    }

    protected final float value;

    /**
     * Constructor
     * 
     * @param value
     * @param parm
     */
    public ScalarWxValue(float aValue, final Parm aParm) {
        super(aParm);
        this.value = aValue;
    }

    /**
     * @return the value
     */
    public float getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Float.floatToIntBits(value);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ScalarWxValue other = (ScalarWxValue) obj;
        if (Float.floatToIntBits(value) != Float.floatToIntBits(other.value)) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (Float.isNaN(this.value)) {
            return "Missing";
        }

        if (this.parm == null) {
            return Float.toString(this.value);
        }

        int precision = this.parm.getGridInfo().getPrecision();
        DecimalFormat df = new DecimalFormat();
        df.setMinimumFractionDigits(precision);
        df.setMaximumFractionDigits(precision);
        return df.format(this.value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.wxvalue.WxValue#isValid()
     */
    @Override
    public boolean isValid() {
        if (getValue() >= this.parm.getGridInfo().getMinValue()
                && getValue() <= this.parm.getGridInfo().getMaxValue()) {
            return true;
        }
        return false;
    }

}
