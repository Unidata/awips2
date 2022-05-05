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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.IConfigurationChangeListener;
import com.raytheon.viz.gfe.PreferenceConstants;

/**
 * Provides a preferred order of displayed parms
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 11, 2008           chammack  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */

public class ParmSortPreference {

    private static final char[] DFT_SORT_CODES = { 'm', 'N', 'M', 't', 'o',
            'l' };

    private static char[] parmDisplayAlgorithm;

    private static String[] parmDisplayOrder;

    static {
        GFEPreference.addConfigurationChangeListener(
                new IConfigurationChangeListener() {

                    @Override
                    public void configurationChanged(String config) {
                        parmDisplayAlgorithm = null;
                        parmDisplayOrder = null;
                    }
                });
    }

    // No instantiation
    private ParmSortPreference() {

    }

    /**
     * Parm.compareTo() uses two settings to control the comparison. This
     * setting is used as part of the by-name comparison.
     *
     * @return the parmDisplayOrder
     */
    public static synchronized String[] getParmSortPreference() {
        if (parmDisplayOrder == null) {
            parmDisplayOrder = GFEPreference.getStringArray(
                    PreferenceConstants.GFE_GRIDMANAGER_SORT_ORDER);
        }
        return parmDisplayOrder;
    }

    /**
     * Parm.compareTo() uses two settings to control the comparison. This
     * setting controls the order of "columns" in the comparison. If the setting
     * isn't in the preference store, comparison order is as defined in
     * DFT_SORT_CODES.
     *
     * @return the parm sort algorithm
     */
    public static synchronized char[] getParmSortAlgorithm() {
        if (parmDisplayAlgorithm == null) {
            String sortAlg = GFEPreference
                    .getString("GridManagerSortAlgorithm");
            List<Character> sortOrder = new ArrayList<>(DFT_SORT_CODES.length);
            // Validate the setting.
            // Only keep chars in DFT_SORT_CODES, no duplicates.
            for (char chr : sortAlg.toCharArray()) {
                for (int i = 0; i < DFT_SORT_CODES.length; i++) {
                    if (chr == DFT_SORT_CODES[i]) {
                        Character cvChr = Character.valueOf(chr);
                        if (!sortOrder.contains(cvChr)) {
                            sortOrder.add(chr);
                        }
                        break;
                    }
                }
            }
            // Make sure all codes are included
            for (char chr : DFT_SORT_CODES) {
                Character cvChr = Character.valueOf(chr);
                if (!sortOrder.contains(cvChr)) {
                    sortOrder.add(Character.valueOf(cvChr));
                }
            }

            // Convert the List<Character> to a char[].
            // This reduces auto-unboxing delays in the client.
            parmDisplayAlgorithm = new char[DFT_SORT_CODES.length];
            int j = 0;
            for (Character chr : sortOrder) {
                parmDisplayAlgorithm[j] = chr.charValue();
                j++;
            }
        }
        return parmDisplayAlgorithm;
    }
}
