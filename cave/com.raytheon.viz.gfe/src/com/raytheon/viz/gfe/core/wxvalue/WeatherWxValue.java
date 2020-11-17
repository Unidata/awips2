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

import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.IConfigurationChangeListener;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * A WxValue encapsulates a value at a single gridpoint of scalar type.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 15, 2010           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class WeatherWxValue extends WxValue {
    private static List<String> altPrettyString;

    private static List<String> altLabels;

    static {
        GFEPreference.addConfigurationChangeListener(
                new IConfigurationChangeListener() {

                    @Override
                    public void configurationChanged(String config) {
                        altPrettyString = null;
                        altLabels = null;
                    }
                });
    }

    public static WeatherWxValue defaultValue(Parm parm) {
        if (parm.getGridInfo().getGridType().equals(GridType.WEATHER)) {
            String key = parm.getParmID().compositeNameUI() + "_defaultValue";
            String defaultV = GFEPreference.getString(key,
                    "<NoCov>:<NoWx>:<NoInten>:<NoVis>:");

            String siteId = parm.getParmID().getDbId().getSiteId();
            return new WeatherWxValue(new WeatherKey(siteId, defaultV), parm);
        } else {
            throw new IllegalArgumentException("parm must be type WEATHER");
        }
    }

    protected final WeatherKey weatherKey;

    /**
     * Construct a weather wx value
     *
     * @param key
     * @param aParm
     */
    public WeatherWxValue(final WeatherKey key, final Parm aParm) {
        super(aParm);
        this.weatherKey = key;
    }

    /**
     * @return the weatherKey
     */
    public WeatherKey getWeatherKey() {
        return this.weatherKey;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = (prime * result)
                + ((weatherKey == null) ? 0 : weatherKey.hashCode());
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
        WeatherWxValue other = (WeatherWxValue) obj;
        if (weatherKey == null) {
            if (other.weatherKey != null) {
                return false;
            }
        } else if (!weatherKey.equals(other.weatherKey)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {

        synchronized (WeatherWxValue.class) {
            if (altPrettyString == null) {
                altPrettyString = Arrays.asList(GFEPreference
                        .getStringArray("AltWxSampleLabels_prettyWx"));
                altLabels = Arrays.asList(GFEPreference
                        .getStringArray("AltWxSampleLabels_label"));
            }
        }

        // get pretty string
        String ps = this.weatherKey.toPrettyString();

        // alternate label?
        if (!altLabels.isEmpty()) {
            int index = altPrettyString.indexOf(ps);
            if ((index != -1) && (index < altLabels.size())) {
                ps = altLabels.get(index);
            }
        }

        return ps;
    }

    @Override
    public boolean isValid() {
        return getWeatherKey().isValid();
    }
}
