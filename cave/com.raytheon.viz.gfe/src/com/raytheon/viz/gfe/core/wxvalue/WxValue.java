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

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.sampler.HistValue;

/**
 * The WxValue encapsulates a data value of any type. The types can be scalar or
 * vector. A none-type or undefined type is also permitted.
 * 
 * Constructors let the programmer create a WxValue of any type. Copy and
 * assignments routines permit the initialization/assignment of any type to any
 * type (although a warning will be logged). Accessors permit getting the data
 * back out from a WxValue. Error checking is included with the accessors. A
 * static function (defaultValue()) can be used to obtain the default value for
 * a parm.
 * 
 * Equality and comparision operators: two WxValues are equal if the data type
 * is the same, and the values represented by the data type are the same. A
 * WxValue is less than another WxValue if the float value is less (for scalar
 * data), the magnitude component is less (for vector data), and if ordering of
 * the weather key is less (for weather).
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial creation.
 * 03/11/2008   879        rbell       Cleanup.
 * 05/20/2009   #2159      rjpeter     Added Factory method for HistValue.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public abstract class WxValue {

    protected final Parm parm;

    /**
     * Protected constructor
     * 
     * @param parm
     *            the parm associated with the value
     */
    protected WxValue(final Parm aParm) {
        this.parm = aParm;
    }

    /**
     * @return the parm
     */
    public Parm getParm() {
        return this.parm;
    }

    /**
     * WxValue::defaultValue() Returns a default value for the given parm.
     * 
     * 
     * Figures out the parm type, uses the parm's max/min to come up with a
     * reasonable default value. If defined in serverConfig/localConfig, will
     * use that value, rather than the default value. Format is:
     * parmName_level_defaultValue.
     * 
     */
    public static WxValue defaultValue(final Parm parm) {
        if (parm == null || parm.getDataManager() == null
                || parm.getDataManager().getParmManager() == null) {
            return null;
        }

        switch (parm.getGridInfo().getGridType()) {
        case SCALAR: {
            return ScalarWxValue.defaultValue(parm);
        }
        case VECTOR: {
            return VectorWxValue.defaultValue(parm);
        }
        case DISCRETE: {
            return DiscreteWxValue.defaultValue(parm);
        }
        case WEATHER: {
            return WeatherWxValue.defaultValue(parm);
        }
        case NONE: {
            return null;
        }
        default:
            throw new IllegalArgumentException("Unknown data type: "
                    + parm.getGridInfo().getGridType());

        }
    }

    public static WxValue getValue(HistValue value, Parm parm) {
        WxValue ret = null;
        switch (value.dataType()) {
        case SCALAR:
            ret = new ScalarWxValue(value.scalar(), parm);
            break;
        case VECTOR:
            ret = new VectorWxValue(value.magnitude(), value.direction(), parm);
            break;
        case DISCRETE:
            ret = new DiscreteWxValue(value.discrete(), parm);
            break;
        case WEATHER:
            ret = new WeatherWxValue(value.weather(), parm);
            break;
        }

        return ret;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((parm == null) ? 0 : parm.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        WxValue other = (WxValue) obj;
        if (parm == null) {
            if (other.parm != null) {
                return false;
            }
        } else if (!parm.equals(other.parm)) {
            return false;
        }
        return true;
    }

    public abstract boolean isValid();
}
