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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DialogAreaComposite extends ScrolledComposite {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DialogAreaComposite.class);

    private static final double MAX_HEIGHT_RATIO = 0.85;

    private static final double MAX_WIDTH_RATIO = 0.85;

    private List<Widget> widgetList;

    private String packType;

    private Composite varFrame;

    private Composite topFrame;

    /**
     * @param parent
     * @param fieldDefs
     * @param dataMgr
     * @param style
     */
    public DialogAreaComposite(Composite parent,
            List<FieldDefinition> fieldDefs, DataManager dataMgr) {
        super(parent, SWT.H_SCROLL | SWT.V_SCROLL);

        varFrame = new Composite(this, SWT.NONE);

        this.setContent(varFrame);
        this.setLayout(new GridLayout());

        GridLayout layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.verticalSpacing = 0;
        layout.horizontalSpacing = 0;
        varFrame.setLayout(layout);
        varFrame.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        widgetList = new ArrayList<Widget>();
        this.packType = "";
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
                if (dataMgr != null) {
                    List<DatabaseID> models = dataMgr.getParmManager()
                            .getAvailableDbs();
                    List<String> filteredDbIdList = new ArrayList<String>();

                    for (DatabaseID dbId : models) {
                        if (dbId.getDbType().equals("")
                                && !(dbId.getModelName().contains("Fcst")
                                        || dbId.getModelName().contains(
                                                "Official") || dbId
                                        .getModelName().contains("Slider"))) {
                            filteredDbIdList.add(dbId.getModelId());
                        }
                    }

                    widget = makeButtonList(
                            (fieldDef.getType() == FieldType.MODEL),
                            fieldDef.getDescription(), filteredDbIdList,
                            fieldDef.getDefaultValue());
                } else {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "No dataMgr supplied to ProcessVariableList, cannot retrieve models");
                }
            } else if (fieldDef.getType() == FieldType.D2DMODEL
                    || fieldDef.getType() == FieldType.D2DMODELS) {
                if (dataMgr != null) {
                    List<DatabaseID> models = dataMgr.getParmManager()
                            .getAvailableDbs();
                    List<String> filteredDbIdList = new ArrayList<String>();

                    for (DatabaseID dbId : models) {
                        if (dbId.getDbType().equals("D2D")
                                && !(dbId.getModelName().contains("Fcst")
                                        || dbId.getModelName().contains(
                                                "Official") || dbId
                                        .getModelName().contains("Slider"))) {
                            filteredDbIdList.add(dbId.getModelId());
                        }
                    }

                    widget = makeButtonList(
                            (fieldDef.getType() == FieldType.D2DMODEL),
                            fieldDef.getDescription(), filteredDbIdList,
                            fieldDef.getDefaultValue());
                } else {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "No dataMgr supplied to ProcessVariableList, cannot retrieve models");
                }
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

            if (widget != null) {
                widget.setVariable(fieldDef.getName());
                widgetList.add(widget);
            }
        }

        Rectangle monitorBounds = this.getDisplay().getPrimaryMonitor()
                .getBounds();
        int maxXSize = (int) (monitorBounds.width * MAX_WIDTH_RATIO);
        int maxYSize = (int) (monitorBounds.height * MAX_HEIGHT_RATIO);

        Point compositeSize = varFrame.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        int xSize = compositeSize.x;
        int ySize = compositeSize.y;
        if (xSize > maxXSize) {
            xSize = maxXSize;
        }
        if (ySize > maxYSize) {
            ySize = maxYSize;
        }

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = ySize;
        gd.widthHint = xSize;
        this.setLayoutData(gd);

        this.setMinSize(new Point(xSize, ySize));
        this.setExpandHorizontal(true);
        this.setExpandVertical(true);

        // Make sure widgets are scrolled into view when they gain focus
        // see:
        // http://www.java2s.com/Code/Java/SWT-JFace-Eclipse/ScrollSWTwidgetsintoviewwhentheygetfocus.htm
        final DialogAreaComposite sc = this;
        Listener scrollOnFocus = new Listener() {

            @Override
            public void handleEvent(Event event) {
                Control child = (Control) event.widget;
                Rectangle bounds = child.getBounds();
                Rectangle area = sc.getClientArea();
                Point origin = sc.getOrigin();
                if (origin.x > bounds.x) {
                    origin.x = Math.max(0, bounds.x);
                }
                if (origin.y > bounds.y) {
                    origin.y = Math.max(0, bounds.y);
                }
                if (origin.x + area.width < bounds.x + bounds.width) {
                    origin.x = Math
                            .max(0, bounds.x + bounds.width - area.width);
                }
                if (origin.y + area.height < bounds.y + bounds.height) {
                    origin.y = Math.max(0, bounds.y + bounds.height
                            - area.height);
                }
                sc.setOrigin(origin);
            }

        };

        Control[] controls = varFrame.getChildren();
        for (Control c : controls) {
            c.addListener(SWT.Activate, scrollOnFocus);
        }

    }

    private Widget makeLabel(String labelText, Object value) {
        if (labelText.isEmpty()) {
            this.packType = "";
            return null;
        }
        // See if we have to start a new frame
        if (!this.packType.equals("label")) {
            this.topFrame = new Composite(this.varFrame, SWT.NONE);
            GridLayout layout = new GridLayout();
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            layout.horizontalSpacing = 0;
            this.topFrame.setLayout(layout);
            this.packType = "label";
        }

        Widget widget = new LabelWidget(labelText, value);
        widget.buildComposite(this.topFrame);
        return widget;
    }

    private Widget makeEntry(String labelText, Object value, boolean numericOnly) {
        // See if we have to start a new frame
        if (!this.packType.equals("entry")) {
            this.topFrame = new Composite(this.varFrame, SWT.NONE);
            GridLayout layout = new GridLayout();
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            layout.horizontalSpacing = 0;
            this.topFrame.setLayout(layout);
            this.packType = "entry";
        }

        Widget widget = null;
        if (numericOnly) {
            widget = new NumberWidget(labelText);
        } else {
            widget = new TextWidget(labelText);
        }
        widget.setValue(value);

        widget.buildComposite(this.topFrame);
        return widget;
    }

    private Widget makeButtonList(boolean radioButton, String label,
            List<? extends Object> elementList, Object initialValue) {
        // See if we have to start a new frame
        if (!this.packType.equals("button")) {
            this.topFrame = new Composite(this.varFrame, SWT.NONE);
            GridLayout layout = new GridLayout(0, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            layout.horizontalSpacing = 0;
            this.topFrame.setLayout(layout);
            this.packType = "button";
        }

        Widget widget;
        if (radioButton) {
            widget = makeRadioList(label, elementList, initialValue);
        } else {
            widget = makeCheckList(label, elementList, initialValue);
        }

        ((GridLayout) this.topFrame.getLayout()).numColumns++;
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

    private ScaleWidget makeScale(String labelText,
            List<? extends Object> valueList, Object initialValue, float res,
            int precision) {
        // See if we have to start a new frame
        if (!this.packType.equals("entry")) {
            this.topFrame = new Composite(this.varFrame, SWT.NONE);
            GridLayout layout = new GridLayout();
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            layout.horizontalSpacing = 0;
            this.topFrame.setLayout(layout);
            this.packType = "entry";
        }

        ScaleWidget scale = new ScaleWidget(labelText);
        scale.setOptions(valueList);
        scale.setValue(initialValue);
        scale.setResolution(res);
        scale.setPrecision(precision);

        scale.buildComposite(this.topFrame);
        return scale;
    }

    private Widget makeScrollbar(String label, Object initialValue) {
        Widget scrollbar = new ScrollbarWidget(label);
        scrollbar.setValue(initialValue);

        return scrollbar;
    }

    public List<Widget> getWidgetList() {
        return this.widgetList;
    }
}
