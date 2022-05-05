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
package com.raytheon.viz.gfe.ui.runtimeui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.python.swt.widgets.CheckWidget;
import com.raytheon.uf.viz.python.swt.widgets.CompactTextWidget;
import com.raytheon.uf.viz.python.swt.widgets.LabelWidget;
import com.raytheon.uf.viz.python.swt.widgets.ListWidget;
import com.raytheon.uf.viz.python.swt.widgets.NumberWidget;
import com.raytheon.uf.viz.python.swt.widgets.RadioWidget;
import com.raytheon.uf.viz.python.swt.widgets.ScaleWidget;
import com.raytheon.uf.viz.python.swt.widgets.ScrollbarWidget;
import com.raytheon.uf.viz.python.swt.widgets.TextWidget;
import com.raytheon.uf.viz.python.swt.widgets.Widget;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smartscript.FieldDefinition.FieldType;

/**
 * Main dialog area for python VariableList defined GUIs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 * May 30, 2012           randerso        Initial creation
 * May 12, 2014  16167    ryu             Fix sizing and accessibility of
 *                                        content.
 * Dec 11, 2014  638      mgamazaychikov  Set isFloat in makeScale.
 * Nov 28, 2017  6540     randerso        Changed makeScale to check for Double
 *                                        instead of Float for Jep 3.6
 * Dec 27, 2017  19773    ryu             Use of check widget (not switch to
 *                                        list widget when item count exceeds
 *                                        20)
 * Jan 15, 2018  6684     randerso        Added support for DELTASCALE and
 *                                        COMPACTTEXT types. Restructured code
 *                                        for improved maintainability/
 *                                        readability. Code cleanup.
 * Oct 23, 2018  6684     randerso        Fixed return values for MODEL and
 *                                        D2DMODEL fields
 *
 * </pre>
 *
 * @author randerso
 */

public class DialogAreaComposite extends ScrolledComposite {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DialogAreaComposite.class);

    private static final double MAX_HEIGHT_RATIO = 0.80;

    private static final double MAX_WIDTH_RATIO = 0.85;

    private static final List<FieldType> SINGLE_SELECTION_TYPES = Arrays
            .asList(FieldType.RADIO, FieldType.MODEL, FieldType.D2DMODEL);

    private List<Widget> widgetList;

    private String packType;

    private Composite varFrame;

    private Composite topFrame;

    /**
     * @param parent
     * @param fieldDefs
     * @param dataMgr
     */
    public DialogAreaComposite(Composite parent,
            List<FieldDefinition> fieldDefs, DataManager dataMgr) {
        super(parent, SWT.H_SCROLL | SWT.V_SCROLL);
        GridLayout layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.verticalSpacing = 0;
        layout.horizontalSpacing = 0;
        this.setLayout(new GridLayout());

        varFrame = new Composite(this, SWT.NONE);
        this.setContent(varFrame);

        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.verticalSpacing = 0;
        layout.horizontalSpacing = 0;
        varFrame.setLayout(layout);
        varFrame.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        widgetList = new ArrayList<>();
        this.packType = "";
        for (FieldDefinition fieldDef : fieldDefs) {
            // See if we have to start a new frame
            if ((this.topFrame == null)
                    || !this.packType.equals(fieldDef.getPackType())) {
                this.topFrame = new Composite(this.varFrame, SWT.NONE);
                layout = new GridLayout(1, false);
                layout.marginHeight = 0;
                layout.marginWidth = 0;
                layout.verticalSpacing = 0;
                layout.horizontalSpacing = 0;
                this.topFrame.setLayout(layout);
                this.packType = fieldDef.getPackType();
            }

            Widget widget = null;

            // TODO: handle unimplemented FieldType values--see TODOs below
            // Refer to AWIPS 1's SelectionDialog.py body() function for details
            switch (fieldDef.getType()) {
            case LABEL:
                widget = makeLabel(fieldDef);
                break;
            case ALPHANUMERIC:
            case NUMERIC:
                widget = makeEntry(fieldDef);
                break;
            case COMPACTTEXT:
                widget = makeCompactTextWidget(fieldDef);
                break;

            case CHECK:
            case RADIO:
                widget = makeButtonList(fieldDef);
                break;

            case SCALE:
            case DELTASCALE:
                widget = makeScale(fieldDef);
                break;

            case SCROLLBAR:
                widget = makeScrollbar(fieldDef);
                break;

            case STARTTIME:
            case ENDTIME:
            case OUTPUT_DIRECTORY:
            case OUTPUT_FILE:
                // TODO: Implement "startTime", "endTime", "output file" and
                // "output directory" AWIPS 1 smart script GUI widgets
                break;

            case DATABASE:
            case DATABASES:
                // TODO: Implement "database" and "databases" AWIPS 1 smart
                // script GUI widgets
                break;

            case MODEL:
            case MODELS:
                if (dataMgr != null) {
                    List<DatabaseID> models = dataMgr.getParmManager()
                            .getAvailableDbs();
                    List<String> filteredDbIdList = new ArrayList<>();

                    for (DatabaseID dbId : models) {
                        if ("".equals(dbId.getDbType()) && !(dbId.getModelName()
                                .contains("Fcst")
                                || dbId.getModelName().contains("Official")
                                || dbId.getModelName().contains("Slider"))) {
                            filteredDbIdList.add(dbId.getModelId());
                        }
                    }

                    widget = makeButtonList(fieldDef, filteredDbIdList);
                } else {
                    statusHandler.handle(Priority.ERROR,
                            "No dataMgr supplied to ProcessVariableList, cannot retrieve models");
                }
                break;

            case D2DMODEL:
            case D2DMODELS:
                if (dataMgr != null) {
                    List<DatabaseID> models = dataMgr.getParmManager()
                            .getAvailableDbs();
                    List<String> filteredDbIdList = new ArrayList<>();

                    for (DatabaseID dbId : models) {
                        if ("D2D".equals(dbId.getDbType()) && !(dbId
                                .getModelName().contains("Fcst")
                                || dbId.getModelName().contains("Official")
                                || dbId.getModelName().contains("Slider"))) {
                            filteredDbIdList.add(dbId.getModelId());
                        }
                    }

                    widget = makeButtonList(fieldDef, filteredDbIdList);
                } else {
                    statusHandler.handle(Priority.ERROR,
                            "No dataMgr supplied to ProcessVariableList, cannot retrieve models");
                }
                break;

            case PARM:
            case PARMMUTABLE:
            case PARMMUTABLEPLUSVARIABLE:
            case PARMPLUSVARIABLE:
            case PARMS:
            case PARMSMUTABLE:
            case PARMSMUTABLEPLUSVARIABLE:
            case PARMSPLUSVARIABLE:
                // TODO: Implement all parm-related AWIPS 1 smart script GUI
                // widgets
                break;

            case REFSET:
            case REFSETS:
                // TODO: Implement "refset" and "refsets" AWIPS 1 smart script
                // GUI widgets
                break;

            case MAP:
            case MAPS:
                // TODO: Implement "map" and "maps" AWIPS 1 smart script GUI
                // widgets
                break;

            case TIMERANGE:
            case TIMERANGES:
                // TODO: Implement "timerange" and "timeranges" AWIPS 1 smart
                // script GUI widgets
                break;

            default:
                FieldDefinition errorField = new FieldDefinition();
                errorField.setDescription("ERROR: " + fieldDef.getDescription()
                        + " unknown widget type: "
                        + fieldDef.getType().getPythonWidgetName());
                widget = makeLabel(errorField);
            }

            if (widget != null) {
                widget.setVariable(fieldDef.getName());

                widgetList.add(widget);
            }

            Boolean newRow = fieldDef.getNewRow();
            if (newRow == null) {
                if ("button".equals(fieldDef.getPackType())) {
                    ((GridLayout) this.topFrame.getLayout()).numColumns++;
                }
            } else {
                if (newRow) {
                    this.packType = "newRow";
                } else {
                    ((GridLayout) this.topFrame.getLayout()).numColumns++;
                }
            }
        }

        // Make sure widgets are scrolled into view when they gain focus
        Listener listener = new Listener() {

            @Override
            public void handleEvent(Event event) {
                if (event.widget instanceof Control) {
                    DialogAreaComposite.this
                            .showControl((Control) event.widget);
                }
            }
        };

        for (Control control : varFrame.getChildren()) {
            if (control instanceof Composite) {
                for (Control child : ((Composite) control).getChildren()) {
                    child.addListener(SWT.Activate, listener);
                }
            }
        }

        Point compositeSize = varFrame.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        int xSize = compositeSize.x;
        int ySize = compositeSize.y;

        this.setMinSize(new Point(xSize, ySize));
        this.setExpandHorizontal(true);
        this.setExpandVertical(true);

        Rectangle monitorBounds = getMonitor().getBounds();
        int maxXSize = (int) (monitorBounds.width * MAX_WIDTH_RATIO);
        int maxYSize = (int) (monitorBounds.height * MAX_HEIGHT_RATIO);

        maxXSize = Math.min(maxXSize, xSize);
        maxYSize = Math.min(maxYSize, ySize);

        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = maxYSize;
        gridData.widthHint = maxXSize;

        for (Widget widget : widgetList) {
            if (widget instanceof ScrollbarWidget) {
                gridData.heightHint = (Integer) widget.getValue();
            }
        }
        setLayoutData(gridData);
    }

    private Widget makeLabel(FieldDefinition fieldDef) {
        if (fieldDef.getDescription().isEmpty()) {
            return null;
        }

        Widget widget = new LabelWidget(fieldDef.getDescription(),
                fieldDef.getDefaultValue());
        widget.buildComposite(this.topFrame);
        return widget;
    }

    private Widget makeEntry(FieldDefinition fieldDef) {

        Widget widget = null;
        if (fieldDef.getType() == FieldType.NUMERIC) {
            widget = new NumberWidget(fieldDef.getDescription());
        } else {
            widget = new TextWidget(fieldDef.getDescription());
        }
        widget.setValue(fieldDef.getDefaultValue());

        widget.buildComposite(this.topFrame);
        return widget;
    }

    private Widget makeCompactTextWidget(FieldDefinition fieldDef) {
        CompactTextWidget widget = new CompactTextWidget(
                fieldDef.getDescription());
        widget.setValue(fieldDef.getDefaultValue());

        widget.buildComposite(this.topFrame);
        return widget;
    }

    private Widget makeButtonList(FieldDefinition fieldDef) {
        return makeButtonList(fieldDef, fieldDef.getValueList());
    }

    private Widget makeButtonList(FieldDefinition fieldDef,
            List<? extends Object> elementList) {

        Widget widget;
        if (SINGLE_SELECTION_TYPES.contains(fieldDef.getType())) {
            widget = makeRadioList(fieldDef.getDescription(), elementList,
                    fieldDef.getDefaultValue());
        } else {
            widget = makeCheckList(fieldDef.getDescription(), elementList,
                    fieldDef.getDefaultValue());
        }

        widget.buildComposite(this.topFrame);
        return widget;
    }

    private Widget makeRadioList(String label,
            List<? extends Object> elementList, Object initialValue) {
        Widget radioList;
        if (elementList.size() > 20) {
            radioList = new ListWidget(label, false);
        } else {
            radioList = new RadioWidget(label);
        }
        radioList.setValue(initialValue);
        radioList.setOptions(elementList);

        return radioList;
    }

    private Widget makeCheckList(String label,
            List<? extends Object> elementList, Object initialValue) {
        Widget checkList;
        if (elementList.size() > 20) {
            checkList = new ListWidget(label, true);
        } else {
            checkList = new CheckWidget(label);
        }
        checkList.setValue(initialValue);
        checkList.setOptions(elementList);

        return checkList;
    }

    private ScaleWidget makeScale(FieldDefinition fieldDef) {

        ScaleWidget scale = new ScaleWidget(fieldDef.getDescription(),
                fieldDef.getType() == FieldType.DELTASCALE);
        scale.setOptions(fieldDef.getValueList());
        scale.setResolution(fieldDef.getResolution());
        scale.setPrecision(fieldDef.getPrecision());

        Object initialValue = fieldDef.getDefaultValue();
        if (initialValue instanceof Double) {
            scale.setFloat(true);
        } else {
            scale.setFloat(false);
        }
        scale.setValue(initialValue);

        scale.buildComposite(this.topFrame);
        return scale;
    }

    private Widget makeScrollbar(FieldDefinition fieldDef) {
        if (fieldDef.getDefaultValue() instanceof Number) {
            Widget scrollbar = new ScrollbarWidget(fieldDef.getDescription());
            scrollbar
                    .setValue(((Number) fieldDef.getDefaultValue()).intValue());

            return scrollbar;
        }
        return null;
    }

    /**
     * @return the widget list
     */
    public List<Widget> getWidgetList() {
        return this.widgetList;
    }
}
