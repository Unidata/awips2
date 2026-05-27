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

import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * A WxValue encapsulates a value at a single gridpoint of scalar type.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 29, 2008           chammack  Initial creation of skeleton.
 * Feb 20, 2008           chammack  Initial implementation.
 * Mar 11, 2008  879      rbell     Cleanup.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */
public class ScalarWxValue extends WxValue {
    public static ScalarWxValue defaultValue(final Parm parm) {
        String key = parm.getParmID().compositeNameUI() + "_defaultValue";
        float value = GFEPreference.getFloat(key,
                parm.getGridInfo().getMinValue());
        return new ScalarWxValue(value, parm);
    }

    protected final float value;

    /**
     * Constructor
     *
     * @param value
     * @param parm
     */
    public ScalarWxValue(float value, final Parm parm) {
        super(parm);
        this.value = value;
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
        result = (prime * result) + Float.floatToIntBits(value);
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

    @Override
    public boolean isValid() {
        if ((getValue() >= this.parm.getGridInfo().getMinValue())
                && (getValue() <= this.parm.getGridInfo().getMaxValue())) {
            return true;
        }
        return false;
    }

}
