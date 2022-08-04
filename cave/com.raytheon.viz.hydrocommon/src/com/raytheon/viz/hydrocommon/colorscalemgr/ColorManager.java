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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThreshold;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThresholdArray;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * Hydro Color Manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Jul 06, 2018 6885       mpduff      Cleanup.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public abstract class ColorManager {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColorManager.class);

    protected static final String HARDCODED = "HARDCODED";

    private static final RGB LABEL_COLOR = RGBColors.getRGBColor("White");

    protected static final String USERS_QUERY = "SELECT DISTINCT userid FROM colorvalue WHERE userid != 'default' AND userid != 'factory_default' and application_name='%s'";

    protected static final String USER_DATATYPES = "select distinct color_use_name from colorvalue where userid ='%s' and application_name='%s'";

    protected static final String DATATYPE_DURATIONS = "select distinct duration from colorvalue where userid = '%s' and application_name='%s' and color_use_name='%s'";

    protected Map<String, NamedColorUseSet> namedColorUseSets;

    protected Map<String, NamedColorUseSet> defaultNamedColorUseSets;

    protected Map<String, String> colorNameMap = new HashMap<>();

    protected String applicationName = null;

    public abstract String getApplicationName();

    public List<String> getDataTypes(String source) {
        String sql = String.format(USER_DATATYPES, source,
                getApplicationName());
        List<String> dataTypes = new ArrayList<>();
        try {
            List<Object[]> rval = DirectDbQuery.executeQuery(sql,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] record : rval) {
                dataTypes.add(record[0].toString());
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving data types: " + sql, e);
        }
        return dataTypes;
    }

    public List<String> getDefaultDataTypes() {
        List<String> rval = new ArrayList<>();

        for (Entry<String, NamedColorUseSet> entry : defaultNamedColorUseSets
                .entrySet()) {
            rval.add(entry.getKey());
        }

        return rval;
    }

    public List<String> getUsers() {
        List<String> users = new ArrayList<>();
        try {
            List<Object[]> rval = DirectDbQuery.executeQuery(
                    String.format(USERS_QUERY, getApplicationName()),
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] record : rval) {
                users.add(record[0].toString());
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving users.", e);
        }
        return users;
    }

    public List<ColorScaleData> getDefaultColorScaleData(String dataType) {
        List<ColorScaleData> rval = new ArrayList<>();
        NamedColorUseSet set = defaultNamedColorUseSets.get(dataType);
        ColorThresholdArray arr = set.getThreshold_array();

        /* Add missing scale data */
        ColorScaleData data = new ColorScaleData();
        data.missingScaleData(RGBColors.getRGBColor(arr.getMissingColorName()));
        rval.add(data);

        /* Add default scale data */
        data = new ColorScaleData();
        data.lessThanMinScaleData(
                RGBColors.getRGBColor(arr.getDefaultColorName()));
        rval.add(data);

        ColorThreshold[] thresholds = arr.getThresholds();
        for (ColorThreshold ct : thresholds) {
            double tmpVal = ct.getValue();
            data = new ColorScaleData();
            // We already added the default and missing values... replace them?
            if (tmpVal == -8888) {
                data.missingScaleData(RGBColors.getRGBColor(ct.getColorName()));
            } else if (tmpVal == -9999) {
                data.lessThanMinScaleData(
                        RGBColors.getRGBColor(ct.getColorName()));
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

        for (Entry<String, NamedColorUseSet> entry : namedColorUseSets
                .entrySet()) {
            if (description
                    .equals(entry.getValue().getColor_use_display_string())) {
                rval = entry.getValue().getColor_use_db_name();
                break;
            } else if (description
                    .equals(entry.getValue().getColor_use_db_name())) {
                // if passing in the data type name then just return it
                rval = description;
                break;
            }
        }

        if (rval == null) {
            for (Entry<String, NamedColorUseSet> entry : defaultNamedColorUseSets
                    .entrySet()) {
                if (description.equals(
                        entry.getValue().getColor_use_display_string())) {
                    rval = entry.getValue().getColor_use_db_name();
                    break;
                } else if (description
                        .equals(entry.getValue().getColor_use_db_name())) {
                    // if passing in the data type name then just return it
                    rval = description;
                    break;
                }
            }
        }

        return rval;
    }

    protected void populateColorUseSets(List<NamedColorUseSet> tmp) {
        namedColorUseSets = new HashMap<>();

        for (NamedColorUseSet ncus : tmp) {
            if (ncus.getColor_use_db_name() == null) {
                namedColorUseSets.put(ncus.getColor_use_display_string(), ncus);
            } else {
                namedColorUseSets.put(ncus.getColor_use_db_name(), ncus);
            }
        }
    }

    public void populateDefaultColorUseSets(List<NamedColorUseSet> tmp) {
        defaultNamedColorUseSets = new HashMap<>();

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
        String rval = "";

        for (Entry<String, NamedColorUseSet> entry : namedColorUseSets
                .entrySet()) {
            String displayName = entry.getValue().getColor_use_display_string();
            if (displayName != null && displayName.equals(dataType)) {
                rval = entry.getValue().getColor_use_display_string();
                break;
            }
        }

        if (rval.isEmpty()) {
            for (Entry<String, NamedColorUseSet> entry : defaultNamedColorUseSets
                    .entrySet()) {
                String displayName = entry.getValue()
                        .getColor_use_display_string();
                String dbUseName = entry.getValue().getColor_use_db_name();
                if (displayName != null && (displayName.equals(dataType)
                        || dbUseName.equals(dataType))) {
                    rval = entry.getValue().getColor_use_display_string();
                    break;
                }

            }
        }

        return rval;

    }

    public List<String> getDurations(String source, String dataType) {
        String sql = String.format(DATATYPE_DURATIONS, source,
                getApplicationName(), dataType);
        List<String> durations = new ArrayList<>();
        try {
            List<Object[]> rval = DirectDbQuery.executeQuery(sql,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] record : rval) {
                durations.add(record[0].toString());
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving durations.", e);
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
        List<ColorValue> colorValueRecordsList = new ArrayList<>();
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

            List<Double> thresholdValues = new ArrayList<>();
            List<String> colorNames = new ArrayList<>();
            String missingColorName = null;
            String defaultColorName = null;
            String dbColorUseName = null;
            String colorUseName = null;
            int duration = 0;
            int i = 0;

            for (Object[] oa : rs) {
                colorValue = getColorValueFromColorValueRecord(oa);
                if (i == 0) {
                    prevIndexString = getColorValueKeyString(colorValue);

                    dbColorUseName = colorValue.getColorUseName();
                    colorUseName = colorNameMap.get(dbColorUseName);
                    if (colorUseName == null) {
                        colorNameMap.put(dbColorUseName, dbColorUseName);
                        colorUseName = dbColorUseName;
                    }
                    duration = (int) colorValue.getDuration();

                    thresholdValues.add(colorValue.getThresholdValue());
                    colorNames.add(colorValue.getColorName());

                    if (colorValue
                            .getThresholdValue() == HydroConstants.MISSING_VALUE) {
                        missingColorName = colorValue.getColorName();
                    }

                    if (colorValue.getThresholdValue() == -8888) {
                        defaultColorName = colorValue.getColorName();
                    }
                } else if (i == (colorValueRecordsList.size() - 1)) {
                    // Last entry in the records list

                    thresholdValues.add(colorValue.getThresholdValue());
                    colorNames.add(colorValue.getColorName());

                    if (colorValue
                            .getThresholdValue() == HydroConstants.MISSING_VALUE) {
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

                        if (colorValue
                                .getThresholdValue() == HydroConstants.MISSING_VALUE) {
                            missingColorName = colorValue.getColorName();
                        }

                        if (colorValue.getThresholdValue() == -8888) {
                            defaultColorName = colorValue.getColorName();
                        }
                    } else {
                        // new index, save off the old data

                        // Convert Double[] to double[]
                        double[] valueArray = new double[thresholdValues
                                .size()];
                        for (int j = 0; j < thresholdValues.size(); j++) {
                            valueArray[j] = thresholdValues.get(j);
                        }

                        namedColorUseSet = new NamedColorUseSet(dbColorUseName,
                                colorUseName, valueArray,
                                colorNames
                                        .toArray(new String[colorNames.size()]),
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

                        if (colorValue
                                .getThresholdValue() == HydroConstants.MISSING_VALUE) {
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
            statusHandler.error("Error reading color values.", e);
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
        colorValue.setDuration(((Number) oa[3]).intValue() / 3600);
        colorValue.setThresholdValue((Double) oa[4]);
        colorValue.setThresholdUnit((String) oa[5]);
        colorValue.setColorName((String) oa[6]);

        return colorValue;
    }

    private String getColorValueKeyString(ColorValue colorValue) {
        return colorValue.getUserID() + "|" + colorValue.getColorUseName() + "|"
                + colorValue.getDuration();
    }

    /**
     * @return the labelColor
     */
    protected static RGB getLabelColor() {
        return LABEL_COLOR;
    }
}
