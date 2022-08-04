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

import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * Abstract base class for Wx/Discrete color tables
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 18, 2010           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public abstract class ColorTable {
    /** Image Attributes */
    public static class ImageAttr {
        private String colorName;

        private String fillPatternName;

        /**
         * Default constructor
         */
        public ImageAttr() {
            this("Black", "WHOLE");
        }

        /**
         * Constructor
         *
         * @param colorName
         * @param fillName
         */
        public ImageAttr(String colorName, String fillName) {
            this.colorName = colorName;
            this.fillPatternName = fillName;
        }

        /**
         * @return the colorName
         */
        public String getColorName() {
            return colorName;
        }

        /**
         * @return the fillPatternName
         */
        public String getFillPatternName() {
            return fillPatternName;
        }

        @Override
        public String toString() {

            return "[" + String.valueOf(colorName) + ", "
                    + String.valueOf(fillPatternName) + "]";
        }
    }

    /** Default value for entries not found in the color table */
    protected static final List<ImageAttr> NOT_IN_TABLE_ENTRY = Arrays
            .asList(new ImageAttr("gray50", "TRANS_25PC_45DEG"));

    private Map<WxValue, List<ImageAttr>> entries;

    /**
     * Default constructor
     */
    public ColorTable() {
        entries = new HashMap<>();
    }

    protected Map<WxValue, List<ImageAttr>> getEntries() {
        return entries;
    }

    /**
     * Map a wxValue to a list of Image Attributes
     *
     * @param wxValue
     * @return list of Image Attributes
     */
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
