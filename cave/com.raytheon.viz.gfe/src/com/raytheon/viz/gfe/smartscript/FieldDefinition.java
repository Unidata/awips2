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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class FieldDefinition {

    public static enum FieldType {
        LABEL("label"), NUMERIC("numeric"), ALPHANUMERIC("alphaNumeric"), RADIO(
                "radio"), CHECK("check"), SCALE("scale"), SCROLLBAR("scrollbar"), PARM(
                "parm"), PARMS("parms"), PARMPLUSVARIABLE("parmPlusVariable"), PARMSPLUSVARIABLE(
                "parmsPlusVariable"), PARMMUTABLE("parmMutable"), PARMSMUTABLE(
                "parmsMutable"), PARMMUTABLEPLUSVARIABLE(
                "parmMutablePlusVariable"), PARMSMUTABLEPLUSVARIABLE(
                "parmsMutablePlusVariable"), DATABASE("database"), DATABASES(
                "databases"), D2DMODEL("D2D_model"), D2DMODELS("D2D_models"), MODEL(
                "model"), MODELS("models"), REFSET("refset"), REFSETS("refsets"), TIMERANGE(
                "timeRange"), TIMERANGES("timeRanges"), MAP("map"), MAPS("maps"), OUTPUT_FILE(
                "output file"), OUTPUT_DIRECTORY("output directory"), STARTTIME(
                "startTime"), ENDTIME("endTime"), UNKNOWN("");

        private String pythonWidgetName;

        private FieldType(String pythonWidgetName) {
            this.pythonWidgetName = pythonWidgetName;
        }

        public String getPythonWidgetName() {
            return pythonWidgetName;
        }

        private void setPythonWidgetName(String pythonWidgetName) {
            this.pythonWidgetName = pythonWidgetName;
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

    private float resolution;

    private int precision;

    public FieldDefinition() {
        valueList = new ArrayList<Object>();
    }

    /**
     * @param name
     * @param description
     * @param type
     * @param defaultValue
     * @param valueList
     * @param resolution
     */
    public FieldDefinition(Object name, String description, FieldType type,
            Object defaultValue, List<Object> valueList, float resolution,
            int precision) {
        this.name = name;
        this.description = description;
        this.type = type;
        this.defaultValue = defaultValue;
        this.valueList = valueList;
        this.resolution = resolution;
        this.precision = precision;
    }

    public Object getName() {
        return name;
    }

    public void setName(Object name) {
        this.name = name;
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

    public void setType(FieldType type) {
        this.type = type;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(Object defaultValue) {
        this.defaultValue = defaultValue;
    }

    public List<? extends Object> getValueList() {
        return valueList;
    }

    public void setValueList(List<? extends Object> valueList) {
        this.valueList = valueList;
    }

    public float getResolution() {
        return resolution;
    }

    public void setResolution(float resolution) {
        this.resolution = resolution;
    }

    public void setPrecision(int precision) {
        this.precision = precision;
    }

    public int getPrecision() {
        return precision;
    }

}
