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
package com.raytheon.viz.hydrocommon.whfslib.colorthreshold;

public class NamedColorUseSet {
    String color_use_db_name;

    String color_use_display_string;

    int default_duration;

    ColorThresholdArray threshold_array;

    /**
     * Initializes a color use set which can then be added to a group of color
     * use sets.
     * 
     * @param color_use_db_name
     *            The color use string as it appears in the ColorValue table.
     * @param color_use_display_string
     *            The color use string as it appears on the color thresholds
     *            window.
     * @param threshold_values
     *            The color thresholds.
     * @param color_names
     *            The color name associated with each threshold.
     * @param missing_color_name
     *            The color to use for missing data.
     * @param default_color_name
     *            The color to use for default.
     * @param default_duration
     *            The duration in seconds of the data represented by this color
     *            set.
     */
    public NamedColorUseSet(final String color_use_db_name,
            final String color_use_display_string,
            final double threshold_values[], final String color_names[],
            final String missing_color_name, final String default_color_name,
            int default_duration) {

        this.color_use_db_name = color_use_db_name;
        this.color_use_display_string = color_use_display_string;
        this.default_duration = default_duration;

        threshold_array = new ColorThresholdArray(missing_color_name,
                default_color_name, threshold_values, color_names);
    }

//    /* Initializes a color use set which can then be added to
//     * a group of color use sets.
//     */
//    public NamedColorUseSet initializeNamedColorUseSet( 
//            String color_use_db_name,
//            String color_use_display_string,
//            double threshold_values [ ],
//            String color_names [ ],
//            String missing_color_name, 
//            String default_color_name,
//            int default_duration ) {
//        NamedColorUseSet namedColorUseSet = new NamedColorUseSet();
//
//        namedColorUseSet.color_use_db_name = color_use_db_name;
//        namedColorUseSet.color_use_display_string = color_use_display_string;
//        namedColorUseSet.default_duration = default_duration;
//
//        threshold_array = new ColorThresholdArray(missing_color_name,
//                default_color_name, threshold_values, color_names);
//        
////        threshold_array = loadColorThresholdSet(
////            missing_color_name,
////            default_color_name,
////            threshold_values,
////            color_names);
//
//        if (threshold_array == null) {
//            return null;
//        }
//        namedColorUseSet.threshold_array = threshold_array;
//        
//        return namedColorUseSet;
//    }
//
    public ColorThresholdArray loadColorThresholdSet(
            String missingColorName,
            String defaultColorName,
            double threshold_values[],
            String color_names[]) {
        
        if (threshold_array == null )
        {
           return null;
        }

        for (int i = 0; i < color_names.length; i++) {
            threshold_array.getThresholds()[i].setValue(threshold_values[i]);
            threshold_array.getThresholds()[i].setColorName(color_names[i]);
        }

        threshold_array.setMissingColorName(missingColorName);
        threshold_array.setDefaultColorName(defaultColorName);

        return threshold_array;
    }
    
    public String getColor_use_db_name() {
        return color_use_db_name;
    }

    public String getColor_use_display_string() {
        return color_use_display_string;
    }

    public int getDefault_duration() {
        return default_duration;
    }

    public ColorThresholdArray getThreshold_array() {
        return threshold_array;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n[DB Name = " + color_use_db_name + "\n");
        sb.append("Display String = " + color_use_display_string + "\n");
        sb.append("Duration = " + default_duration + "]\n");
        
        return sb.toString();
    }
}