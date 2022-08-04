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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.python.swt.ButtonConstant;
import com.raytheon.uf.viz.python.swt.widgets.LabelWidget;
import com.raytheon.uf.viz.python.swt.widgets.Widget;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dynamic non-modal dialog for displaying options for running tools/procedures
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 09, 2010  3353     njensen   Initial creation
 * Jul 13, 2011  9291     rferrel   Convert to subclass of CaveJFACEDialog.
 * Nov 15, 2012  1298     rferrel   Code cleanup for non-blocking dialogs.
 * Sep 23, 2015  4871     randerso  Changed to concrete class with do nothing
 *                                  run method Added flag to change buttons when
 *                                  called from procedure
 * Jan 15, 2018  6684     randerso  Fixed dialog sizing issues. Code cleanup.
 *
 * </pre>
 *
 */

public class SelectionDlg extends CaveJFACEDialog {
    /**
     * The top composite.
     */
    protected DialogAreaComposite comp;

    protected String name;

    protected DataManager dataMgr;

    private List<FieldDefinition> fieldDefs;

    private boolean fromProcedure;

    @Override
    protected void buttonPressed(int buttonId) {
        setReturnCode(buttonId);

        switch (ButtonConstant.getButton(buttonId)) {
        case RUN:
            run();
            break;
        case OK:
        case RUN_DISMISS:
            run();
            close();
            break;
        default:
            super.buttonPressed(buttonId);
            break;
        }
    }

    /**
     * Method called when the Run, Run/Dismiss, or OK buttons are pressed.
     */
    protected void run() {
        // Base dialog does nothing for case when called from procedure.
    }

    /**
     * Constructor
     *
     * @param parent
     *            parent shell
     * @param name
     *            name of smartTool/procedure
     * @param dataMgr
     *            DataManager instance to use
     * @param fieldDefs
     *            field definitions for dialog
     * @param fromProcedure
     *            true if being called from procedure
     */
    public SelectionDlg(Shell parent, String name, DataManager dataMgr,
            List<FieldDefinition> fieldDefs, boolean fromProcedure) {
        super(parent);
        this.name = name;
        this.dataMgr = dataMgr;
        this.fieldDefs = fieldDefs;
        this.fromProcedure = fromProcedure;
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(this.name + " Values");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        // Create the main layout for the top level composite.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        top.setLayout(mainLayout);

        this.comp = new DialogAreaComposite(top, fieldDefs, this.dataMgr);

        return top;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (fromProcedure) {
            createButton(parent, ButtonConstant.OK.id, ButtonConstant.OK.label,
                    false);
        } else {
            createButton(parent, ButtonConstant.RUN.id,
                    ButtonConstant.RUN.label, false);
            createButton(parent, ButtonConstant.RUN_DISMISS.id,
                    ButtonConstant.RUN_DISMISS.label, false);
        }
        createButton(parent, ButtonConstant.CANCEL.id,
                ButtonConstant.CANCEL.label, false);

        // Restrict the Shell from resizing below the size of the button bar
        parent.pack();
        getShell().setMinimumSize(parent.getSize().x + 10,
                parent.getSize().y + 20);
    }

    /**
     * Get values from dialog
     *
     * @return map of field labels to values
     */
    public Map<String, Object> getValues() {
        Map<String, Object> map = new HashMap<>();
        for (Widget w : this.comp.getWidgetList()) {
            if (!(w instanceof LabelWidget)) {
                map.put(w.getLabel(), w.getValue());
            }
        }
        return map;
    }
}