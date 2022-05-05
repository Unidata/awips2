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
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.python.swt.ButtonConstant;
import com.raytheon.uf.viz.python.swt.CallbackFunctor;
import com.raytheon.uf.viz.python.swt.widgets.LabelWidget;
import com.raytheon.uf.viz.python.swt.widgets.Widget;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Displays a shell based on a list of widgets.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 03, 2008  1164     jelkins   Initial creation
 * Oct 15, 2008           njensen   Static methods to keep UI thread working
 * Mar 28, 2013  1790     rferrel   Make dialog modal except when the static
 *                                  openDialog is used.
 * Jan 15, 2015  5054     randerso  Remove unnecessary new Shell
 * Jan 29, 2016  5289     tgurney   Add missing close button in trim
 * Jan 15, 2018  6684     randerso  Fixed dialog sizing issues. Code cleanup.
 *
 * </pre>
 *
 * @author jelkins
 */

public class ValuesDialog extends CaveJFACEDialog {

    private String title;

    private List<FieldDefinition> fieldDefs;

    private DataManager dataMgr;

    private Map<Object, Object> values;

    private CallbackFunctor callback;

    private Object callbackResult;

    private boolean closeAfterRun;

    private DialogAreaComposite composite;

    /**
     * Class constructor
     *
     * @param parentShell
     *            the parent shell of this dialog
     *
     * @param title
     *            title to be displayed in the title bar of this dialog
     * @param fieldDefs
     *            a list of field definitions this dialog will display.
     * @param dataMgr
     */
    public ValuesDialog(Shell parentShell, String title,
            List<FieldDefinition> fieldDefs, DataManager dataMgr) {
        super(parentShell);
        this.title = title;
        this.fieldDefs = fieldDefs;
        this.dataMgr = dataMgr;

        this.values = new HashMap<>();
        this.closeAfterRun = false;

        this.setShellStyle(SWT.APPLICATION_MODAL | SWT.TITLE | SWT.RESIZE);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(title);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        if ((callback != null) || closeAfterRun) {
            createButton(parent, ButtonConstant.RUN.id,
                    ButtonConstant.RUN.label, false);
            createButton(parent, ButtonConstant.RUN_DISMISS.id,
                    ButtonConstant.RUN_DISMISS.label, false);
        } else {
            createButton(parent, ButtonConstant.OK.id, ButtonConstant.OK.label,
                    true);
        }

        // Always have a cancel
        createButton(parent, ButtonConstant.CANCEL.id,
                ButtonConstant.CANCEL.label, false);

        // Change return code from OK to CANCEL to prevent running dialog
        // when GFE perspective is closed while the dialog is open.
        setReturnCode(ButtonConstant.CANCEL.id);

        // Restrict the Shell from resizing below the size of the button bar
        parent.pack();
        getShell().setMinimumSize(parent.getSize().x + 10,
                parent.getSize().y + 20);

    }

    @Override
    protected void buttonPressed(int buttonId) {

        setReturnCode(buttonId);

        switch (ButtonConstant.getButton(buttonId)) {
        case RUN:
            doRun();
            if (closeAfterRun) {
                close();
            }
            break;
        case OK:
        case RUN_DISMISS:
            doRun();
            close();
            break;

        case CANCEL:
            close();
            break;
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        // Create the main layout for the top level composite.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        top.setLayout(mainLayout);

        this.composite = new DialogAreaComposite(top, fieldDefs, this.dataMgr);

        return top;
    }

    /**
     * Get the values of this dialog and use them to evaluate the callback
     * function.
     */
    private void doRun() {

        // get the values
        for (Widget w : composite.getWidgetList()) {
            if (!(w instanceof LabelWidget)) {
                values.put(w.getVariable(), w.getValue());
            }
        }

        // evaluate the callback
        if (callback != null) {
            callbackResult = callback.evaluate(values);
        }

    }

    /**
     * @return the values of the widgets in the dialog
     */
    public Map<Object, Object> getValues() {
        return values;
    }

    /**
     * @return the callback
     */
    public CallbackFunctor getCallback() {
        return callback;
    }

    /**
     * Set the callback function for the Run buttons.
     * <p>
     * Set the function to call when the user clicks the Run or Run/Dismiss
     * buttons on this dialog. The callback should be set before calling
     * <code>open</code>.
     * </p>
     *
     * @param callback
     *            the callback to set
     */
    public void setCallback(CallbackFunctor callback) {
        this.callback = callback;
    }

    /**
     * @return the last result of the callback evaluation.
     */
    public Object getCallbackResult() {
        return callbackResult;
    }

    /**
     * @return the closeAfterRun
     */
    public boolean isCloseAfterRun() {
        return closeAfterRun;
    }

    /**
     * Enable closing the dialog after the run button is pressed.
     * <p>
     * This function is used in the ProcessVariableList.py interface as a way to
     * handle python callbacks. By default this is <code>false</code>.
     * </p>
     * <p>
     * This function is more of a utlity for the python bridge than for Java.
     * Ideally this functionality will not be needed from within pure Java code.
     * </p>
     *
     * @param closeAfterRun
     *            the closeAfterRun to set
     */
    public void setCloseAfterRun(boolean closeAfterRun) {
        this.closeAfterRun = closeAfterRun;
    }

    /**
     * This function is used by the python bridge when dynamically loading the
     * widgets.
     * <p>
     * This result of this function is easily handled in Java. In python
     * (through JEP), it is more difficult to determine the classLoader of a
     * class.
     * </p>
     *
     * @return the classLoader of this class
     */
    public static ClassLoader getClassLoader() {
        return ValuesDialog.class.getClassLoader();
    }

    /**
     * This method used by python to create a blocking, non-modal dialog.
     *
     * @param title
     * @param fieldDefs
     * @param dataMgr
     * @return the dialog
     */
    public static ValuesDialog openDialog(final String title,
            final List<FieldDefinition> fieldDefs, final DataManager dataMgr) {

        ValuesDialog[] dialog = new ValuesDialog[1];
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                Shell shell = null;
                if (PlatformUI.isWorkbenchRunning()) {
                    shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getShell();
                }

                dialog[0] = new ValuesDialog(shell, title, fieldDefs, dataMgr);
                dialog[0].setShellStyle(
                        SWT.CLOSE | SWT.MODELESS | SWT.TITLE | SWT.RESIZE);
                dialog[0].open();
            }
        });
        return dialog[0];
    }
}
