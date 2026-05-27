/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
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
package com.raytheon.viz.satellite;

import java.text.ParseException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.datalisting.impl.DefaultDataListing;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.util.VariableSubstitutor;

/**
 *
 * A {@link DataListing} that can format satellite values to be slightly more
 * user friendly.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jun 26, 2018  6615     bsteffen  Initial creation
 * Jul 20, 2018  7378     mapeters  Handle SatelliteLegendLookup being
 *                                  extracted from SatelliteConstants
 *
 * </pre>
 *
 * @author bsteffen
 */
public class SatDataListing extends DefaultDataListing {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatDataListing.class);

    public static final String PHYSICAL_ELEMENT = "physicalElement";

    public static final String CREATING_ENTITY = "creatingEntity";

    public static final String SECTOR_ID = "sectorID";

    public SatDataListing(List<String> keys) {
        super(SatelliteRecord.PLUGIN_NAME, keys);
    }

    @Override
    public Map<String, String> getFormattedValues(String key,
            Map<String, String> keyVals) throws Exception {
        if (!key.equals(PHYSICAL_ELEMENT)
                || !keyVals.containsKey(CREATING_ENTITY)) {
            return super.getFormattedValues(key, keyVals);
        }
        Map<String, String> result = new TreeMap<>();
        for (String value : getValues(key, keyVals)) {
            result.put(value, formatPhysicalElement(value, keyVals));
        }
        return result;
    }

    /**
     * Find a more user friendly alias for the specified physical element. The
     * best current source of friendliness is from the legends used in the
     * satellite resource so this method emulates that legend lookup and then
     * does a bit of cleanup to try to get a nice label for the physical
     * element.
     *
     * @param physicalElement
     *            the physicalElement to format.
     * @param keyVals
     *            the other fields the user has selected
     * @return a friendlier description of the physical element.
     */
    private String formatPhysicalElement(String physicalElement,
            Map<String, String> keyVals) {
        String creatingEntity = keyVals.get(CREATING_ENTITY);
        if (creatingEntity == null) {
            /*
             * Legends are generally customized based off creating entity so if
             * it isn't available don't bother formating.
             */
            return physicalElement;
        }
        String legend = getLegendFromStyleRule(physicalElement, creatingEntity);
        if (legend == null) {
            legend = SatelliteLegendLookup.getLegend(physicalElement,
                    creatingEntity);
        }
        if (legend.indexOf('$') >= 0) {

            Map<String, String> subs = new HashMap<>(keyVals);
            /*
             * Don't really need units info in the display so just use empty
             * string
             */
            subs.put("units", "");
            subs.put(PHYSICAL_ELEMENT, physicalElement);
            try {
                legend = VariableSubstitutor.processVariables(legend, subs);
            } catch (ParseException e) {
                /*
                 * This is likely to happen if a legend needs something the user
                 * hasn't selected yet, in this case the it doesn't make sense
                 * to use the legend so return an unformatted value.
                 */
                statusHandler.debug(e.getLocalizedMessage(), e);
                return physicalElement;
            }
        }
        /*
         * Removing the units often leaves an empty pair of parenthesis that can
         * be removed.
         */
        legend = legend.replaceAll("\\(\\s*\\)", "");

        /*
         * Most legends start with the creating entity, since creating entity is
         * already selected by the user it shouldn't be displayed for physical
         * element so remove it.
         */
        if (legend.startsWith(creatingEntity)) {
            legend = legend.substring(creatingEntity.length() + 1);
        }

        return legend.trim();
    }

    private String getLegendFromStyleRule(String physicalElement,
            String creatingEntity) {
        try {
            ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
            match.setParameterName(Arrays.asList(physicalElement));
            match.setCreatingEntityNames(Arrays.asList(creatingEntity));
            StyleRule sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.IMAGERY, match);
            if (sr != null && sr.getPreferences() instanceof ImagePreferences) {
                return ((ImagePreferences) sr.getPreferences()).getLegend();
            }
        } catch (StyleException e) {
            statusHandler.debug("Error getting legend from style rule for "
                    + creatingEntity + " & " + physicalElement, e);
        }
        return null;
    }

}
