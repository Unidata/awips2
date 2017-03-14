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

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * VectorWxValue is a vector implementation of WxValue containing a magnitude
 * and direction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial creation of skeleton.
 * 03/11/2008   879        rbell       Cleanup.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class VectorWxValue extends ScalarWxValue {
    public static VectorWxValue defaultValue(Parm parm) {
        float magvalue = parm.getGridInfo().getMinValue();
        float dirvalue = 0.0f;
        if (Activator.getDefault() != null
                && Activator
                        .getDefault()
                        .getPreferenceStore()
                        .contains(
                                parm.getParmID().compositeNameUI()
                                        + "_magDefaultValue")) {
            magvalue = Activator
                    .getDefault()
                    .getPreferenceStore()
                    .getFloat(
                            parm.getParmID().compositeNameUI()
                                    + "_magDefaultValue");
        }
        if (Activator.getDefault() != null
                && Activator
                        .getDefault()
                        .getPreferenceStore()
                        .contains(
                                parm.getParmID().compositeNameUI()
                                        + "_dirDefaultValue")) {
            dirvalue = Activator
                    .getDefault()
                    .getPreferenceStore()
                    .getFloat(
                            parm.getParmID().compositeNameUI()
                                    + "_dirDefaultValue");
        }
        return new VectorWxValue(magvalue, dirvalue, parm);
    }

    private static final String dirToStringDir8[] = { "N", "NE", "E", "SE",
            "S", "SW", "W", "NW", "N" };

    private static final String dirToStringDir16[] = { "N", "NNE", "NE", "ENE",
            "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW",
            "NNW", "N" };

    protected String format;

    protected final float dir;

    /**
     * Construct a vector wx value
     * 
     * @param aMag
     * @param aDir
     * @param aParm
     */
    public VectorWxValue(float aMag, float aDir, final Parm aParm) {
        super(aMag, aParm);
        dir = aDir;
        format = "";

        if (Activator.getDefault() != null) {
            IPreferenceStore prefs = Activator.getDefault()
                    .getPreferenceStore();
            String pname = aParm.getParmID().getParmName();
            String parmSetting = pname
                    + PreferenceConstants.GFE_WIND_FORMAT_SUFFIX;
            format = prefs.getString(parmSetting);
            if ("".equals(format)) {
                format = prefs
                        .getString(PreferenceConstants.GFE_WIND_FORMAT_STR);
            }
        }

        if ("".equals(format)) {
            format = "ddff";
        }
    }

    /**
     * @return the mag
     */
    public float getMag() {
        return this.getValue();
    }

    /**
     * @return the dir
     */
    public float getDir() {
        return this.dir;
    }

    /**
     * @return magnitude as a string
     */
    public String magToString() {
        String rVal = "";
        // ddff formatting
        if (this.format.equals("ddff")) {
            float umag = getMag() + 0.5f;
            if (getMag() < 0.5f) {
                rVal += "00";
            } else if (umag < 10.0) {
                rVal += "0" + (int) umag;
            } else {
                rVal += (int) umag;
            }
        }

        // all other formats
        else {
            int precision = this.parm.getGridInfo().getPrecision();
            DecimalFormat df = new DecimalFormat();
            df.setMaximumFractionDigits(precision);
            rVal = df.format(getMag());
        }
        return rVal;
    }

    /**
     * Return direction as a string
     * 
     * @return
     */
    public String dirToString() {
        String rVal = "";

        // ddff formatting
        if (this.format.equals("ddff")) {
            // round the direction to the nearest 10 degrees
            int intDir = (int) (getDir() + (10 / 2)) / 10;
            if (getMag() < (float) 0.5) {
                intDir = 0;
            } else if (intDir == 0) {
                intDir = 36;
            }

            DecimalFormat df = new DecimalFormat();
            df.setMinimumIntegerDigits(2);
            df.setMaximumFractionDigits(2);
            rVal = df.format(intDir);
        }

        // 8 point formatting
        else if (this.format.equals("8pt")) {
            if (getMag() < (float) 0.5) {
                rVal += "calm";
            } else {
                float d = getDir() + 22.5f;
                while (d >= 360.0) {
                    d -= 360.0;
                }
                while (d < 0.0) {
                    d += 360.0;
                }
                if (d == 360.0) {
                    d = 0.0f;
                }
                int sector = (int) (d / 45.0);
                rVal += dirToStringDir8[sector];
            }
        }

        // 16 point formatting
        else if (this.format.equals("16pt")) {
            if (getMag() < (float) 0.5) {
                rVal += "calm";
            } else {
                float d = getDir() + 11.25f;
                while (d >= 360.0) {
                    d -= 360.0;
                }
                while (d < 0.0) {
                    d += 360.0;
                }
                if (d == 360.0) {
                    d = 0.0f;
                }
                int sector = (int) (d / 22.5);
                rVal += dirToStringDir16[sector];
            }
        }

        // d/f formatting (freeform)
        else if (this.format.equals("d/f")) {
            rVal += getDir();
        }

        return rVal;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Float.floatToIntBits(dir);
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
        VectorWxValue other = (VectorWxValue) obj;
        if (Float.floatToIntBits(dir) != Float.floatToIntBits(other.dir)) {
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
        String rVal = "";

        if (this.format.equals("ddff")) {
            if (this.getMag() < 0.5) {
                rVal += "0000";
            } else {
                rVal += dirToString() + magToString();
            }
        }

        else if (this.format.equals("8pt") || this.format.equals("16pt")) {
            if (getMag() < (float) 0.5) {
                rVal += "calm";
            } else {
                rVal += dirToString() + magToString();
            }
        }

        else if (this.format.equals("d/f")) {
            rVal += dirToString() + '/' + magToString();
        }

        return rVal;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.wxvalue.WxValue#isValid()
     */
    @Override
    public boolean isValid() {
        if (getMag() >= 0 && getMag() <= this.parm.getGridInfo().getMaxValue()
                && getDir() >= 0 && getDir() <= 360.0) {
            return true;
        }
        return false;
    }

}
