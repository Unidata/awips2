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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThreshold;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThresholdArray;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

public abstract class ColorManager {
    protected static final String HARDCODED = "HARDCODED";

    /**
     * Label color.
     */
    private static final RGB labelColor = RGBColors.getRGBColor("White");

    protected Map<String, NamedColorUseSet> namedColorUseSets;

    protected Map<String, NamedColorUseSet> defaultNamedColorUseSets;

    protected static final String USERS_QUERY = "SELECT DISTINCT userid FROM colorvalue WHERE userid != 'default' AND userid != 'factory_default' and application_name='%s'";

    protected static final String USER_DATATYPES = "select distinct color_use_name from colorvalue where userid ='%s' and application_name='%s'";

    protected Map<String, String> colorNameMap = new HashMap<String, String>();

    protected static final String DATATYPE_DURATIONS = "select distinct duration from colorvalue where userid = '%s' and application_name='%s' and color_use_name='%s'";

    protected String applicationName = null;

    public abstract String getApplicationName();

    public ArrayList<String> getDataTypes(String source) {
        String sql = String
                .format(USER_DATATYPES, source, getApplicationName());
        ArrayList<String> dataTypes = new ArrayList<String>();
        try {
            List<Object[]> rval = DirectDbQuery.executeQuery(sql,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] record : rval) {
                dataTypes.add(record[0].toString());
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
        return dataTypes;
    }

    public ArrayList<String> getDefaultDataTypes() {
        ArrayList<String> rval = new ArrayList<String>();

        Iterator<Map.Entry<String, NamedColorUseSet>> i = defaultNamedColorUseSets
                .entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry<String, NamedColorUseSet> entry = i.next();
            rval.add(entry.getKey());
        }

        return rval;
    }

    public ArrayList<String> getUsers() {
        ArrayList<String> users = new ArrayList<String>();
        try {
            List<Object[]> rval = DirectDbQuery.executeQuery(
                    String.format(USERS_QUERY, getApplicationName()),
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] record : rval) {
                users.add(record[0].toString());
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
        return users;
    }

    public ArrayList<ColorScaleData> getDefaultColorScaleData(String dataType) {
        ArrayList<ColorScaleData> rval = new ArrayList<ColorScaleData>();
        NamedColorUseSet set = defaultNamedColorUseSets.get(dataType);
        ColorThresholdArray arr = set.getThreshold_array();

        /* Add missing scale data */
        ColorScaleData data = new ColorScaleData();
        data.missingScaleData(RGBColors.getRGBColor(arr.getMissingColorName()));
        rval.add(data);

        /* Add default scale data */
        data = new ColorScaleData();
        data.lessThanMinScaleData(RGBColors.getRGBColor(arr
                .getDefaultColorName()));
        rval.add(data);

        ColorThreshold[] thresholds = arr.getThresholds();
        for (int i = 0; i < thresholds.length; ++i) {
            ColorThreshold ct = thresholds[i];
            double tmpVal = ct.getValue();
            data = new ColorScaleData();
            // We already added the default and missing values... replace them?
            if (tmpVal == -8888) {
                data.missingScaleData(RGBColors.getRGBColor(ct.getColorName()));
            } else if (tmpVal == -9999) {
                data.lessThanMinScaleData(RGBColors.getRGBColor(ct
                        .getColorName()));
            } else {
                data.setColor(RGBColors.getRGBColor(ct.getColorName()));
                data.setValueLbl(tmpVal);
                rval.add(data);
            }
        }
        return rval;
    }

    public String getDataTypeName(String description) {
        String rval = null;

        Iterator<Map.Entry<String, NamedColorUseSet>> i = namedColorUseSets
                .entrySet().iterator();
        while (i.hasNext() && (rval == null)) {
            Map.Entry<String, NamedColorUseSet> entry = i.next();
            if (description.equals(entry.getValue()
                    .getColor_use_display_string())) {
                return entry.getValue().getColor_use_db_name();
            } else if (description.equals(entry.getValue()
                    .getColor_use_db_name())) {
            	// if passing in the data type name then just return it
                return description;
            }
        }

        return rval;
    }

    protected void populateColorUseSets(List<NamedColorUseSet> tmp) {
        namedColorUseSets = new HashMap<String, NamedColorUseSet>();

        for (NamedColorUseSet ncus : tmp) {
            if (ncus.getColor_use_db_name() == null) {
                namedColorUseSets.put(ncus.getColor_use_display_string(), ncus);
            } else {
                namedColorUseSets.put(ncus.getColor_use_db_name(), ncus);
            }
        }
    }

    public void populateDefaultColorUseSets(List<NamedColorUseSet> tmp) {
        defaultNamedColorUseSets = new HashMap<String, NamedColorUseSet>();

        for (NamedColorUseSet ncus : tmp) {
            defaultNamedColorUseSets.put(ncus.getColor_use_display_string(),
                    ncus);
        }
    }

    public String getDuration(String dataType) {
        NamedColorUseSet tmp = namedColorUseSets.get(dataType);
        return String.valueOf(tmp.getDefault_duration() / 3600);
    }

    public String getDescription(String dataType) {
        String rval = null;

        Iterator<Map.Entry<String, NamedColorUseSet>> i = namedColorUseSets
                .entrySet().iterator();
        while (i.hasNext() && (rval == null)) {
            Map.Entry<String, NamedColorUseSet> entry = i.next();
            if (dataType.equals(entry.getValue().getColor_use_db_name())) {
                if (entry.getValue().getColor_use_display_string() == null) {
                    rval = entry.getValue().getColor_use_db_name();
                    break;
                } else {
                    rval = entry.getValue().getColor_use_display_string();
                    break;
                }
            } else if (dataType.equals(entry.getValue().getColor_use_display_string())) {
            	// if the display string is passed in just return it
            	rval = dataType;
            	break;
            }
        }

        return rval;
    }

    public ArrayList<String> getDurations(String source, String dataType) {
        String sql = String.format(DATATYPE_DURATIONS, source,
                getApplicationName(), dataType);
        ArrayList<String> durations = new ArrayList<String>();
        try {
            List<Object[]> rval = DirectDbQuery.executeQuery(sql,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] record : rval) {
                durations.add(record[0].toString());
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
        return durations;

    }

    /**
     * Read the color values from IHFS.
     * 
     * @return
     */
    public NamedColorSetGroup readColorValuesFromDatabase() {
        ColorValue colorValue = new ColorValue();
        List<ColorValue> colorValueRecordsList = new ArrayList<ColorValue>();
        NamedColorUseSet namedColorUseSet = null;
        String currentIndexString = null;
        String prevIndexString = null;
        NamedColorSetGroup namedColorSetGroup = new NamedColorSetGroup();

        String whereClause = "WHERE application_name='" + applicationName
                + "' AND userid != 'factory_default' "
                + "ORDER BY color_use_name, userid, duration, threshold_value";

        try {
            String query = "select userid, application_name, color_use_name, "
                    + " duration, threshold_value, threshold_unit, color_name "
                    + " from colorValue " + whereClause;
            List<Object[]> rs = DirectDbQuery.executeQuery(
                    String.format(query, getApplicationName()),
                    HydroConstants.IHFS, QueryLanguage.SQL);

            ArrayList<Double> thresholdValues = new ArrayList<Double>();
            ArrayList<String> colorNames = new ArrayList<String>();
            String missingColorName = null;
            String defaultColorName = null;
            String dbColorUseName = null;
            String colorUseName = null;
            int duration = 0;
            int i = 0;

            for (Object[] oa : rs) {
                colorValue = getColorValueFromColorValueRecord(oa);
                if (i == 0) { // first one
                    prevIndexString = getColorValueKeyString(colorValue);

                    dbColorUseName = colorValue.getColorUseName();
                    colorUseName = colorNameMap.get(dbColorUseName);
                    duration = (int) colorValue.getDuration();

                    thresholdValues.add(colorValue.getThresholdValue());
                    colorNames.add(colorValue.getColorName());

                    if (colorValue.getThresholdValue() == HydroConstants.MISSING_VALUE) {
                        missingColorName = colorValue.getColorName();
                    }

                    if (colorValue.getThresholdValue() == -8888) {
                        defaultColorName = colorValue.getColorName();
                    }
                } else if (i == (colorValueRecordsList.size() - 1)) {
                    // Last entry in the records list

                    thresholdValues.add(colorValue.getThresholdValue());
                    colorNames.add(colorValue.getColorName());

                    if (colorValue.getThresholdValue() == HydroConstants.MISSING_VALUE) {
                        missingColorName = colorValue.getColorName();
                    }

                    if (colorValue.getThresholdValue() == -8888) {
                        defaultColorName = colorValue.getColorName();
                    }

                    // Convert Double[] to double[]
                    double[] valueArray = new double[thresholdValues.size()];
                    for (int j = 0; j < thresholdValues.size(); j++) {
                        valueArray[j] = thresholdValues.get(j);
                    }

                    dbColorUseName = colorValue.getColorUseName();
                    colorUseName = colorNameMap.get(dbColorUseName);
                    duration = (int) colorValue.getDuration();

                    namedColorUseSet = new NamedColorUseSet(dbColorUseName,
                            colorUseName, valueArray,
                            colorNames.toArray(new String[colorNames.size()]),
                            missingColorName, defaultColorName, duration);

                    namedColorSetGroup.addNamedColorUseSet(namedColorUseSet);
                } else {
                    currentIndexString = getColorValueKeyString(colorValue);
                    if (currentIndexString.equalsIgnoreCase(prevIndexString)) {
                        // same index
                        thresholdValues.add(colorValue.getThresholdValue());
                        colorNames.add(colorValue.getColorName());

                        if (colorValue.getThresholdValue() == HydroConstants.MISSING_VALUE) {
                            missingColorName = colorValue.getColorName();
                        }

                        if (colorValue.getThresholdValue() == -8888) {
                            defaultColorName = colorValue.getColorName();
                        }
                    } else {
                        // new index, save off the old data

                        // Convert Double[] to double[]
                        double[] valueArray = new double[thresholdValues.size()];
                        for (int j = 0; j < thresholdValues.size(); j++) {
                            valueArray[j] = thresholdValues.get(j);
                        }

                        namedColorUseSet = new NamedColorUseSet(
                                dbColorUseName,
                                colorUseName,
                                valueArray,
                                colorNames.toArray(new String[colorNames.size()]),
                                missingColorName, defaultColorName, duration);

                        namedColorSetGroup
                                .addNamedColorUseSet(namedColorUseSet);

                        // reset variables for next data set
                        thresholdValues.clear();
                        colorNames.clear();

                        // Get the next data
                        dbColorUseName = colorValue.getColorUseName();
                        colorUseName = colorNameMap.get(dbColorUseName);
                        duration = (int) colorValue.getDuration();

                        thresholdValues.add(colorValue.getThresholdValue());
                        colorNames.add(colorValue.getColorName());

                        if (colorValue.getThresholdValue() == HydroConstants.MISSING_VALUE) {
                            missingColorName = colorValue.getColorName();
                        }

                        if (colorValue.getThresholdValue() == -8888) {
                            defaultColorName = colorValue.getColorName();
                        }

                        prevIndexString = currentIndexString;
                    }
                }
                i++;
            }
            populateColorUseSets(namedColorSetGroup.getColorGroupArray());
        } catch (VizException e) {
            e.printStackTrace();
        }

        return namedColorSetGroup;
    }

    /**
     * Create a ColorValue object from a db returned row.
     * 
     * @param oa
     *            The Object[] returned from the database
     * 
     * @return The ColorValue object
     */
    private ColorValue getColorValueFromColorValueRecord(Object[] oa) {
        ColorValue colorValue = new ColorValue();

        colorValue.setUserID((String) oa[0]);
        colorValue.setApplicationName((String) oa[1]);
        colorValue.setColorUseName((String) oa[2]);
        colorValue.setDuration(((Integer) oa[3]) / 3600);
        colorValue.setThresholdValue((Double) oa[4]);
        colorValue.setThresholdUnit((String) oa[5]);
        colorValue.setColorName((String) oa[6]);

        return colorValue;
    }

    private String getColorValueKeyString(ColorValue colorValue) {
        return colorValue.getUserID() + "|" + colorValue.getColorUseName()
                + "|" + colorValue.getDuration();
    }

    /**
     * @return the labelColor
     */
    protected static RGB getLabelColor() {
        return labelColor;
    }
}
