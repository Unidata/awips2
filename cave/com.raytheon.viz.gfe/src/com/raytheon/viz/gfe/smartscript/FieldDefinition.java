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
package com.raytheon.viz.gfe.smartscript;

import java.util.ArrayList;
import java.util.List;

/**
 * Contains definitions of entry fields for variable list GUIs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 29, 2010           dgilling  Initial creation
 * Nov 28, 2017  6540     randerso  Changed resolution to double
 * Jan 15, 2018  6684     randerso  Added new FieldTypes DELTASCALE and
 *                                  COMPACTTEXT to support Serp tool.
 *                                  Restructured to improve
 *                                  maintainability/readability of code.
 *
 * </pre>
 *
 * @author dgilling
 */

public class FieldDefinition {

    public static enum FieldType {
        LABEL("label", "label"),
        NUMERIC("numeric", "entry"),
        ALPHANUMERIC("alphaNumeric", "entry"),
        COMPACTTEXT("compactText", "entry"),
        RADIO("radio", "button"),
        CHECK("check", "button"),
        SCALE("scale", "entry"),
        DELTASCALE("deltascale", "entry"),
        SCROLLBAR("scrollbar", ""),
        PARM("parm", "button"),
        PARMS("parms", "button"),
        PARMPLUSVARIABLE("parmPlusVariable", "button"),
        PARMSPLUSVARIABLE("parmsPlusVariable", "button"),
        PARMMUTABLE("parmMutable", "button"),
        PARMSMUTABLE("parmsMutable", "button"),
        PARMMUTABLEPLUSVARIABLE("parmMutablePlusVariable", "button"),
        PARMSMUTABLEPLUSVARIABLE("parmsMutablePlusVariable", "button"),
        DATABASE("database", "button"),
        DATABASES("databases", "button"),
        D2DMODEL("D2D_model", "button"),
        D2DMODELS("D2D_models", "button"),
        MODEL("model", "button"),
        MODELS("models", "button"),
        REFSET("refset", "button"),
        REFSETS("refsets", "button"),
        TIMERANGE("timeRange", "button"),
        TIMERANGES("timeRanges", "button"),
        MAP("map", "button"),
        MAPS("maps", "button"),
        OUTPUT_FILE("output file", "entry"),
        OUTPUT_DIRECTORY("output directory", "entry"),
        STARTTIME("startTime", "entry"),
        ENDTIME("endTime", "entry"),
        UNKNOWN("", "");

        private String pythonWidgetName;

        private String packType;

        private FieldType(String pythonWidgetName, String packType) {
            this.pythonWidgetName = pythonWidgetName;
            this.packType = packType;
        }

        public String getPythonWidgetName() {
            return pythonWidgetName;
        }

        private void setPythonWidgetName(String pythonWidgetName) {
            this.pythonWidgetName = pythonWidgetName;
        }

        public String getPackType() {
            return packType;
        }

        public static FieldType convertPythonType(String pythonType) {
            FieldType retVal = FieldType.UNKNOWN;

            for (FieldType type : FieldType.values()) {
                if (type.getPythonWidgetName().equals(pythonType)) {
                    retVal = type;
                    break;
                }
            }

            if (retVal == FieldType.UNKNOWN) {
                retVal.setPythonWidgetName(pythonType);
            }

            return retVal;
        }

    }

    private Object name;

    private String description;

    private FieldType type;

    private Object defaultValue;

    private List<? extends Object> valueList;

    private double resolution;

    private int precision;

    private Boolean newRow;

    public FieldDefinition() {
        valueList = new ArrayList<>();
    }

    /**
     * @param name
     * @param description
     * @param type
     * @param defaultValue
     * @param valueList
     * @param resolution
     * @param precision
     */
    public FieldDefinition(Object name, String description, FieldType type,
            Object defaultValue, List<Object> valueList, double resolution,
            int precision) {
        this(name, description, type, defaultValue, valueList, resolution,
                precision, true);
    }

    /**
     * @param name
     * @param description
     * @param type
     * @param defaultValue
     * @param valueList
     * @param resolution
     * @param precision
     * @param newRow
     */
    public FieldDefinition(Object name, String description, FieldType type,
            Object defaultValue, List<Object> valueList, double resolution,
            int precision, Boolean newRow) {
        this.name = name;
        this.description = description;
        this.type = type;
        this.defaultValue = defaultValue;
        this.valueList = valueList;
        this.resolution = resolution;
        this.precision = precision;
        this.newRow = newRow;
    }

    public Object getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public FieldType getType() {
        return type;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public List<? extends Object> getValueList() {
        return valueList;
    }

    public double getResolution() {
        return resolution;
    }

    public int getPrecision() {
        return precision;
    }

    public Boolean getNewRow() {
        return newRow;
    }

    public String getPackType() {
        if ((type == FieldType.LABEL) && description.isEmpty()) {
            return "";
        }
        return getType().getPackType();
    }

}
