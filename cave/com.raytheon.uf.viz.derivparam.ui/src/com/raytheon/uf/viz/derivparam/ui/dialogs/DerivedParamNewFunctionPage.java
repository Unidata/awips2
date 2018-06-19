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
package com.raytheon.uf.viz.derivparam.ui.dialogs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.derivparam.DerivParamFunctionType;
import com.raytheon.uf.common.derivparam.DerivParamFunctionType.FunctionArgument;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTWizardPage;

/**
 * Wizard page for adding a derived parameter function
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DerivedParamNewFunctionPage extends CaveSWTWizardPage {

    /* SWT widgets */
    private Text nameText;

    private Combo typeCbo;

    private Button removeBtn;

    private Table parameterTable;

    private Map<String, DerivParamFunctionType> functionMap;

    /** Selected function type */
    private DerivParamFunctionType selectedType;

    /**
     * Constructor for new function page
     */
    public DerivedParamNewFunctionPage() {
        super("New Function");
        this.setDescription("Create a new function for the derived parameter.  "
                + "If you do not want to create \na new function, leave the fields blank and continue");
        functionMap = new HashMap<String, DerivParamFunctionType>();

        DerivParamFunctionType[] functionTypes = DerivedParameterGenerator
                .getFunctionTypes();
        for (DerivParamFunctionType type : functionTypes) {
            functionMap.put(type.getName(), type);
        }
    }

    public String getFunctionName() {
        return nameText.getText().trim();
    }

    public DerivParamFunctionType getFunctionType() {
        return selectedType;
    }

    @Override
    public boolean isPageComplete() {
        boolean nameSet = !nameText.getText().trim().isEmpty();
        boolean hasParameters = parameterTable.getItemCount() > 0;
        boolean canFlip = true;
        if (nameSet == false && hasParameters) {
            setErrorMessage("Enter a name for the function.  If no function is desired, remove"
                    + " all function parameters");
            canFlip = false;
        } else if (nameSet && hasParameters == false) {
            setErrorMessage("Add 1 or more arguments for the function");
            canFlip = false;
        }

        return canFlip;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTWizardPage#createPageControls(org.
     * eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPageControls(Composite parentComp) {
        createTextControls(parentComp);
        createArgumentListControls(parentComp);
        createButtonControls(parentComp);

        typeCbo.select(0);
        newTypeSelected();
    }

    /**
     * @param parent
     */
    private void createTextControls(Composite parent) {
        // *name:
        // type: -> dropdown combo
        Composite textComp = new Composite(parent, SWT.NONE);
        textComp.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        textComp.setLayoutData(gd);

        Label nameLbl = new Label(textComp, SWT.NONE);
        nameLbl.setText("Function Name: ");
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        nameLbl.setLayoutData(gd);

        nameText = new Text(textComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        nameText.setLayoutData(gd);

        Label typeLbl = new Label(textComp, SWT.NONE);
        typeLbl.setText("Function Type: ");
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        typeLbl.setLayoutData(gd);

        typeCbo = new Combo(textComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        typeCbo.setLayoutData(gd);

        typeCbo.setItems(functionMap.keySet().toArray(new String[0]));
        typeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                newTypeSelected();
            }
        });
    }

    /**
     * @param parent
     */
    private void createButtonControls(Composite parent) {
        // Add Edit Remove
        Composite buttonComp = new Composite(parent, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        Button addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add Argument...");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addNewArgument();
            }
        });

        removeBtn = new Button(buttonComp, SWT.PUSH);
        removeBtn.setText("Remove Argument(s)");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        removeBtn.setLayoutData(gd);
        removeBtn.setEnabled(false);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                removeSelectedArguments();
            }
        });

    }

    /**
     * @param parent
     */
    private void createArgumentListControls(Composite parent) {
        // Parameter Name | Parameter Type
        Composite argsTableComp = new Composite(parent, SWT.NONE);
        argsTableComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        argsTableComp.setLayoutData(gd);

        parameterTable = new Table(argsTableComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 200;
        parameterTable.setLayoutData(gd);
        parameterTable.setHeaderVisible(true);
        parameterTable.setLinesVisible(true);
        parameterTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTableSelection();
            }
        });

        TableColumn tc1 = new TableColumn(parameterTable, SWT.NONE);
        tc1.setText(" Parameter Name ");
        tc1.setResizable(true);
        tc1.pack();

        TableColumn tc2 = new TableColumn(parameterTable, SWT.NONE);
        tc2.setText(" Parameter Type ");
        tc2.setResizable(true);
        tc2.pack();
    }

    private void addTableItem(FunctionArgument arg) {
        TableItem ti = new TableItem(parameterTable, SWT.NONE);
        ti.setText(new String[] { arg.name, arg.type });
        ti.setData(arg);

        /*
         * Loop and pack the table columns so the columns will be resized to see
         * the text. This must be done every time a table item or a list of
         * table items are entered in.
         */
        for (TableColumn tc : parameterTable.getColumns()) {
            tc.pack();
        }
    }

    /**
     * Using the arguments, run against template to create text that is the
     * function
     * 
     * @return the text of the new function
     */
    public String createFunction() {
        if (getFunctionName().isEmpty() == false) {
            List<FunctionArgument> args = new ArrayList<FunctionArgument>();
            for (TableItem ti : parameterTable.getItems()) {
                args.add((FunctionArgument) ti.getData());
            }
            return selectedType.getAdapter().createNewFunction(
                    getFunctionName(),
                    args.toArray(new FunctionArgument[args.size()]));
        }
        return null;
    }

    /**
     * Get the selected function type for the function
     * 
     * @return
     */
    public DerivParamFunctionType getSelectedFunctionType() {
        return selectedType;
    }

    private void newTypeSelected() {
        String type = typeCbo.getText();
        selectedType = functionMap.get(type);
        parameterTable.clearAll();
    }

    private void addNewArgument() {
        AddParameterDlg dlg = new AddParameterDlg(getShell());
        Boolean rval = (Boolean) dlg.open();
        if (rval == true) {
            // Add the argument
            FunctionArgument fa = new FunctionArgument();
            fa.name = dlg.name;
            fa.type = dlg.type;
            addTableItem(fa);
        }
    }

    private void removeSelectedArguments() {
        if (parameterTable.getSelectionCount() != 0) {
            parameterTable.remove(parameterTable.getSelectionIndices());
        }
    }

    private void handleTableSelection() {
        if (parameterTable.getSelectionCount() == 0) {
            removeBtn.setEnabled(false);
            return;
        }
        removeBtn.setEnabled(true);
    }

    private class AddParameterDlg extends CaveSWTDialog {

        private Text nameText;

        private Combo typeCbo;

        private String name, type;

        /**
         * @param parentShell
         */
        protected AddParameterDlg(Shell parentShell) {
            super(parentShell);
            setText("Add Parameter");
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents
         * (org.eclipse.swt.widgets.Shell)
         */
        @Override
        protected void initializeComponents(final Shell shell) {
            setReturnValue(false);

            shell.addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    name = nameText.getText().trim();
                    type = typeCbo.getText().trim();
                }
            });

            Composite c = new Composite(shell, SWT.NONE);
            c.setLayout(new GridLayout(2, false));
            c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

            new Label(c, SWT.NONE).setText("Parameter Name:");

            GridData gd;
            nameText = new Text(c, SWT.NONE);
            nameText.setText("parameter" + (parameterTable.getItemCount() + 1));
            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
            gd.widthHint = 150;
            nameText.setLayoutData(gd);

            new Label(c, SWT.NONE).setText("Parameter Type:");

            typeCbo = new Combo(c, SWT.DROP_DOWN | SWT.READ_ONLY);
            typeCbo.setItems(selectedType.getAdapter().getArgumentTypes());
            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
            gd.widthHint = 160;
            typeCbo.setLayoutData(gd);

            c = new Composite(shell, SWT.NONE);
            c.setLayout(new GridLayout(2, true));
            c.setLayoutData(new GridData(SWT.RIGHT, SWT.DEFAULT, true, false));

            Button b = new Button(c, SWT.PUSH);
            b.setText("Cancel");
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    setReturnValue(false);
                    shell.close();
                }
            });
            gd = new GridData(SWT.FILL, SWT.FILL, false, true);
            gd.widthHint = 100;
            b.setLayoutData(gd);

            b = new Button(c, SWT.PUSH);
            b.setText("Add");
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (nameText.getText().trim().isEmpty() == false) {
                        setReturnValue(true);
                        shell.close();
                    } else {
                        if (nameText.getText().trim().isEmpty()) {
                            nameText.setBackground(getDisplay().getSystemColor(
                                    SWT.COLOR_RED));
                        } else {
                            nameText.setBackground(getDisplay().getSystemColor(
                                    SWT.COLOR_WHITE));
                        }
                    }
                }
            });
            gd = new GridData(SWT.FILL, SWT.FILL, false, true);
            gd.widthHint = 100;
            b.setLayoutData(gd);

            typeCbo.select(0);
        }
    }
}
