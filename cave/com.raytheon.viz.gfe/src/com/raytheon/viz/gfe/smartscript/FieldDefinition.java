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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.viz.python.swt.widgets.CheckWidget;
import com.raytheon.uf.viz.python.swt.widgets.LabelWidget;
import com.raytheon.uf.viz.python.swt.widgets.ListWidget;
import com.raytheon.uf.viz.python.swt.widgets.NumberWidget;
import com.raytheon.uf.viz.python.swt.widgets.RadioWidget;
import com.raytheon.uf.viz.python.swt.widgets.ScaleWidget;
import com.raytheon.uf.viz.python.swt.widgets.ScrollbarWidget;
import com.raytheon.uf.viz.python.swt.widgets.TextWidget;
import com.raytheon.uf.viz.python.swt.widgets.Widget;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * TODO Add Description
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
    
    private int getPrecision() {
		return precision;
	}

    public static List<Widget> buildWidgetList(List<FieldDefinition> fieldDefs,
            DataManager dataMgr) {
        List<Widget> widgets = new ArrayList<Widget>();

        for (FieldDefinition fieldDef : fieldDefs) {
            Widget widget = null;

            // TODO: handle unimplemented FieldType values--see TODOs below
            // Refer to AWIPS 1's SelectionDialog.py body() function for details
            if (fieldDef.getType() == FieldType.LABEL) {
                widget = makeLabel(fieldDef.getDescription(),
                        fieldDef.getDefaultValue());
            } else if (fieldDef.getType() == FieldType.ALPHANUMERIC
                    || fieldDef.getType() == FieldType.NUMERIC) {
                widget = makeEntry(fieldDef.getDescription(),
                        fieldDef.getDefaultValue(),
                        (fieldDef.getType() == FieldType.NUMERIC));
            } else if (fieldDef.getType() == FieldType.CHECK
                    || fieldDef.getType() == FieldType.RADIO) {
                widget = makeButtonList(
                        (fieldDef.getType() == FieldType.RADIO),
                        fieldDef.getDescription(), fieldDef.getValueList(),
                        fieldDef.getDefaultValue());
            } else if (fieldDef.getType() == FieldType.SCALE) {
                widget = makeScale(fieldDef.getDescription(),
                        fieldDef.getValueList(), fieldDef.getDefaultValue(),
                        fieldDef.getResolution(), fieldDef.getPrecision());
            } else if (fieldDef.getType() == FieldType.SCROLLBAR) {
                widget = makeScrollbar(fieldDef.getDescription(),
                        fieldDef.getDefaultValue());
            } else if (fieldDef.getType() == FieldType.STARTTIME
                    || fieldDef.getType() == FieldType.ENDTIME
                    || fieldDef.getType() == FieldType.OUTPUT_DIRECTORY
                    || fieldDef.getType() == FieldType.OUTPUT_FILE) {
                // TODO: Implement "startTime", "endTime", "output file" and
                // "output directory" AWIPS 1 smart script GUI widgets
            } else if (fieldDef.getType() == FieldType.DATABASE
                    || fieldDef.getType() == FieldType.DATABASES) {
                // TODO: Implement "database" and "databases" AWIPS 1 smart
                // script GUI widgets
            } else if (fieldDef.getType() == FieldType.MODEL
                    || fieldDef.getType() == FieldType.MODELS) {
                List<DatabaseID> models = dataMgr.getParmManager()
                        .getAvailableDbs();
                List<String> filteredDbIdList = new ArrayList<String>();

                for (DatabaseID dbId : models) {
                    if (dbId.getDbType().equals("")
                            && !(dbId.getModelName().contains("Fcst")
                                    || dbId.getModelName().contains("Official") || dbId
                                    .getModelName().contains("Slider"))) {
                        filteredDbIdList.add(dbId.getModelId());
                    }
                }

                widget = makeButtonList(
                        (fieldDef.getType() == FieldType.MODEL),
                        fieldDef.getDescription(), filteredDbIdList,
                        fieldDef.getDefaultValue());
            } else if (fieldDef.getType() == FieldType.D2DMODEL
                    || fieldDef.getType() == FieldType.D2DMODELS) {
                List<DatabaseID> models = dataMgr.getParmManager()
                        .getAvailableDbs();
                List<String> filteredDbIdList = new ArrayList<String>();

                for (DatabaseID dbId : models) {
                    if (dbId.getDbType().equals("D2D")
                            && !(dbId.getModelName().contains("Fcst")
                                    || dbId.getModelName().contains("Official") || dbId
                                    .getModelName().contains("Slider"))) {
                        filteredDbIdList.add(dbId.getModelId());
                    }
                }

                widget = makeButtonList(
                        (fieldDef.getType() == FieldType.D2DMODEL),
                        fieldDef.getDescription(), filteredDbIdList,
                        fieldDef.getDefaultValue());
            } else if (fieldDef.getType().getPythonWidgetName()
                    .contains("parm")) {
                // TODO: Implement all parm-related AWIPS 1 smart script GUI
                // widgets
            } else if (fieldDef.getType() == FieldType.REFSET
                    || fieldDef.getType() == FieldType.REFSETS) {
                // TODO: Implement "refset" and "refsets" AWIPS 1 smart script
                // GUI widgets
            } else if (fieldDef.getType() == FieldType.MAP
                    || fieldDef.getType() == FieldType.MAPS) {
                // TODO: Implement "map" and "maps" AWIPS 1 smart script GUI
                // widgets
            } else if (fieldDef.getType() == FieldType.TIMERANGE
                    || fieldDef.getType() == FieldType.TIMERANGES) {
                // TODO: Implement "timerange" and "timeranges" AWIPS 1 smart
                // script GUI widgets
            } else {
                widget = makeLabel("ERROR: " + fieldDef.getDescription()
                        + " unknown widget type: "
                        + fieldDef.getType().getPythonWidgetName(), null);
            }

            widget.setVariable(fieldDef.getName());
            widgets.add(widget);
        }

        return widgets;
    }

    private static Widget makeLabel(String labelText, Object value) {
        return new LabelWidget(labelText, value);
    }

    private static Widget makeEntry(String labelText, Object value,
            boolean numericOnly) {
        Widget entryField = null;

        if (numericOnly) {
            entryField = new NumberWidget(labelText);
        } else {
            entryField = new TextWidget(labelText);
        }
        entryField.setValue(value);

        return entryField;
    }

    private static Widget makeButtonList(boolean radioButton, String label,
            List<? extends Object> elementList, Object initialValue) {
        if (radioButton) {
            return makeRadioList(label, elementList, initialValue);
        } else {
            return makeCheckList(label, elementList, initialValue);
        }
    }

    private static Widget makeRadioList(String label,
            List<? extends Object> elementList, Object initialValue) {
        Widget radioList;
        if (elementList.size() < 20) {
            radioList = new RadioWidget(label);
        } else {
            radioList = new ListWidget(label, false);
        }
        radioList.setValue(initialValue);
        radioList.setOptions(elementList);

        return radioList;
    }

    private static Widget makeCheckList(String label,
            List<? extends Object> elementList, Object initialValue) {
        Widget checkList;
        if (elementList.size() < 20) {
            checkList = new CheckWidget(label);
        } else {
            checkList = new ListWidget(label, true);
        }
        checkList.setValue(initialValue);
        checkList.setOptions(elementList);

        return checkList;
    }

    private static ScaleWidget makeScale(String labelText,
            List<? extends Object> valueList, Object initialValue, float res, int precision) {
        ScaleWidget scale = new ScaleWidget(labelText);
        scale.setOptions(valueList);
        scale.setValue(initialValue);
        scale.setResolution(res);
        scale.setPrecision(precision);

        return scale;
    }

    private static Widget makeScrollbar(String label, Object initialValue) {
        Widget scrollbar = new ScrollbarWidget(label);
        scrollbar.setValue(initialValue);

        return scrollbar;
    }
}
