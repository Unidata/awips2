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

package com.raytheon.uf.viz.d2d.core.time;


/**
 * Provides a list of load modes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2007            chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public enum LoadMode {
    STD("Standard"), LATEST("Latest"), VALID_TIME_SEQ("Valid time seq"), PREVIOUS_MODEL_RUN(
            "Previous run"), PREVIOUS_VALID_TIME_SEQ("Prev valid time seq"), PROG_LOOP(
            "Prognosis loop"), ANALYSIS_LOOP("Analysis loop"), DPROG_DT(
            "dProg/dt"), FORCED("Forced"), FCST_TIME_MATCH("Forecast match"), INVENTORY(
            "Inventory"), SLOT("Slot"), NO_BACKFILL("No Backfill"), RANGE_MATCH(
            "Range match");

    private String text;

    LoadMode(String text) {
        this.text = text;
    }

    public String getLabel() {
        return text;
    }

    public static String[] labels() {
        LoadMode[] loadModes = LoadMode.values();
        String[] labels = new String[loadModes.length];

        for (int i = 0; i < loadModes.length; ++i) {
            labels[i] = loadModes[i].getLabel();
        }
        return labels;
    }

    public static LoadMode valueOfLabel(String label) {
        for (LoadMode lm : LoadMode.values()) {
            if (lm.getLabel().equals(label)) {
                return lm;
            }
        }
        return null;
    }

}
