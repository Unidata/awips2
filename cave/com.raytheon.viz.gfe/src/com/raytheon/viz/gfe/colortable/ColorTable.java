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
package com.raytheon.viz.gfe.colortable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class ColorTable {
    public static class ImageAttr {
        private String colorName;

        private String fillPatternName;

        public ImageAttr() {
            this("Black", "WHOLE");
        }

        public ImageAttr(String colorName, String fillName) {
            this.colorName = colorName;
            this.fillPatternName = fillName;
        }

        public String getColorName() {
            return colorName;
        }

        public String getFillPatternName() {
            return fillPatternName;
        }

        @Override
        public String toString() {

            return "[" + String.valueOf(colorName) + ", "
                    + String.valueOf(fillPatternName) + "]";
        }
    }

    public static final List<ImageAttr> NOT_IN_TABLE_ENTRY = Arrays
            .asList(new ImageAttr("gray50", "TRANS_25PC_45DEG"));

    protected static PythonPreferenceStore prefs = Activator.getDefault()
            .getPreferenceStore();

    private Map<WxValue, List<ImageAttr>> entries;

    public ColorTable() {
        entries = new HashMap<WxValue, List<ImageAttr>>();
    }

    protected Map<WxValue, List<ImageAttr>> getEntries() {
        return entries;
    }

    public abstract List<ImageAttr> map(final WxValue wxValue);

    protected void addEntry(WxValue wxValue, List<ImageAttr> attrs) {
        entries.put(wxValue, attrs);
    }

    protected void addEntry(ColorEntry... colorTableEntries) {
        for (ColorEntry entry : colorTableEntries) {
            addEntry(entry.getValue(), entry.getAttributes());
        }
    }

    protected void resetEntries() {
        entries.clear();
    }
}
