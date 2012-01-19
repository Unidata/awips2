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

import javax.measure.unit.Unit;
import javax.xml.bind.JAXBException;

import org.eclipse.jface.dialogs.MessageDialog;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.derivparam.library.DerivParamConstantField;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.library.IDerivParamField;
import com.raytheon.uf.viz.derivparam.ui.Activator;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTWizardPage;

/**
 * Page in wizard for creating a new derived parameter definition
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

public class DerivedParamNewDefinitionPage extends CaveSWTWizardPage {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(DerivedParamNewDefinitionPage.class);

    /* SWT Widgets */
    private Text nameText;

    private Text abbrevText;

    private Text unitText;

    private Table methodTable;

    private Button removeBtn, editBtn;

    /**
     * @param pageName
     */
    public DerivedParamNewDefinitionPage() {
        super("New Definition");
        setDescription("Create a new function for the derived parameter.  "
                + "If you do not want to create \na new definition, leave the fields blank and continue");
    }

    public String getAbbreviation() {
        return abbrevText.getText().trim();
    }

    /**
     * Using the arguments, run against template to create text that is the
     * definition
     * 
     * @return the text of the new definition
     */
    public String createDefinition() {
        if (getAbbreviation().isEmpty() == false) {
            DerivParamDesc desc = new DerivParamDesc();
            desc.setAbbreviation(abbrevText.getText().trim());
            try {
                if (unitText.getText().trim().isEmpty() == false) {
                    desc.setUnit(new UnitAdapter().unmarshal(unitText.getText()
                            .trim()));
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing unit string", e);
                desc.setUnit(Unit.ONE);
            }
            desc.setName(nameText.getText().trim());
            for (TableItem ti : methodTable.getItems()) {
                desc.addMethod((DerivParamMethod) ti.getData());
            }
            try {
                return SerializationUtil.marshalToXml(desc);
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
                return null;
            }
        }
        return null;
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
        createTableControl(parentComp);
        createButtonControls(parentComp);
    }

    /**
     * @param parent
     */
    private void createTextControls(Composite parent) {
        // *abbreviation: (required)
        // name:
        // unit:
        Composite textComp = new Composite(parent, SWT.NONE);
        textComp.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        textComp.setLayoutData(gd);

        /*
         * Abbreviation
         */
        Label abbrLbl = new Label(textComp, SWT.NONE);
        abbrLbl.setText("Abbreviation: ");
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        abbrLbl.setLayoutData(gd);

        abbrevText = new Text(textComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        abbrevText.setLayoutData(gd);

        /*
         * Name
         */
        Label nameLbl = new Label(textComp, SWT.NONE);
        nameLbl.setText("Name: ");
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        nameLbl.setLayoutData(gd);

        nameText = new Text(textComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        nameText.setLayoutData(gd);

        /*
         * Unit
         */
        Label unitLbl = new Label(textComp, SWT.NONE);
        unitLbl.setText("Unit: ");
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        unitLbl.setLayoutData(gd);

        unitText = new Text(textComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        unitText.setLayoutData(gd);
    }

    /**
     * @param parent
     */
    private void createTableControl(Composite parent) {
        Composite argsTableComp = new Composite(parent, SWT.NONE);
        argsTableComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        argsTableComp.setLayoutData(gd);

        methodTable = new Table(argsTableComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 200;
        methodTable.setLayoutData(gd);
        methodTable.setHeaderVisible(true);
        methodTable.setLinesVisible(true);
        methodTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTableSelection();
            }
        });

        TableColumn tc = new TableColumn(methodTable, SWT.NONE);
        tc.setText(" Method Name ");
        tc.setResizable(true);
        tc.pack();

        tc = new TableColumn(methodTable, SWT.NONE);
        tc.setText(" Method Parameters ");
        tc.setResizable(true);
        tc.pack();
    }

    /**
     * @param parent
     */
    private void createButtonControls(Composite parent) {
        // Add Edit Remove
        Composite buttonComp = new Composite(parent, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        Button addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add Method...");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addNewMethod();
            }
        });

        editBtn = new Button(buttonComp, SWT.PUSH);
        editBtn.setText("Edit Method...");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        editBtn.setLayoutData(gd);
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                editSelectedMethod();
            }
        });

        removeBtn = new Button(buttonComp, SWT.PUSH);
        removeBtn.setText("Remove Method(s)");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        removeBtn.setLayoutData(gd);
        removeBtn.setEnabled(false);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                removeSelectedMethods();
            }
        });
    }

    private void addTableItem(DerivParamMethod arg) {
        TableItem ti = new TableItem(methodTable, SWT.NONE);
        addTableItem(arg, ti);
    }

    private void addTableItem(DerivParamMethod arg, TableItem ti) {
        String name = arg.getName();
        boolean first = true;
        String fields = "";
        for (IDerivParamField field : arg.getFields()) {
            if (!first) {
                fields += "|";
            }
            first = false;
            if (field instanceof DerivParamField) {
                fields += ((DerivParamField) field).getParam();
            } else {
                fields += ((DerivParamConstantField) field).getValue()
                        .toString();
            }
        }

        ti.setText(new String[] { name, fields });
        ti.setData(arg);

        /*
         * Loop and pack the table columns so the columns will be resized to see
         * the text. This must be done every time a table item or a list of
         * table items are entered in.
         */
        for (TableColumn tc : methodTable.getColumns()) {
            tc.pack();
        }
    }

    private void handleTableSelection() {
        if (methodTable.getSelectionCount() == 0) {
            editBtn.setEnabled(false);
            removeBtn.setEnabled(false);
            return;
        }
        editBtn.setEnabled(true);
        removeBtn.setEnabled(true);
    }

    private void addNewMethod() {
        AddMethodDlg dlg = new AddMethodDlg(getShell());
        Boolean rval = (Boolean) dlg.open();
        if (rval) {
            addTableItem(dlg.method);
        }
    }

    private void removeSelectedMethods() {
        if (methodTable.getSelectionCount() != 0) {
            methodTable.remove(methodTable.getSelectionIndices());
        }
    }

    private void editSelectedMethod() {
        TableItem selected = methodTable.getSelection()[0];
        AddMethodDlg dlg = new AddMethodDlg(getShell(),
                (DerivParamMethod) selected.getData());
        Boolean rval = (Boolean) dlg.open();
        if (rval) {
            addTableItem(dlg.method, selected);
        }
    }

    @Override
    public boolean isPageComplete() {
        if (abbrevText.getText().trim().isEmpty()) {
            if (nameText.getText().trim().isEmpty()
                    && unitText.getText().isEmpty()
                    && methodTable.getSelectionCount() == 0) {
                return true;
            }
            setErrorMessage("Enter an abbreviation for the defined definition or leave all fields blank");
            return false;
        }
        return true;
    }

    private class AddMethodDlg extends CaveSWTDialog {

        private Text nameText;

        private Table methodParamTable;

        private DerivParamMethod method = null;

        /**
         * @param parentShell
         */
        protected AddMethodDlg(Shell parentShell) {
            super(parentShell);
            setText("Add Method");
        }

        protected AddMethodDlg(Shell parentShell, DerivParamMethod method) {
            this(parentShell);
            this.method = method;
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
                    method = new DerivParamMethod();
                    for (TableItem ti : methodParamTable.getItems()) {
                        method.addField((IDerivParamField) ti.getData());
                    }
                    method.setName(nameText.getText().trim());
                }
            });

            Composite c = new Composite(shell, SWT.NONE);
            c.setLayout(new GridLayout(2, false));
            c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

            new Label(c, SWT.NONE).setText("Method Name:");

            GridData gd;
            nameText = new Text(c, SWT.BORDER);
            nameText.setText("MethodName" + methodTable.getItemCount());
            gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            nameText.setLayoutData(gd);

            Group fieldGroup = new Group(c, SWT.NONE);
            fieldGroup.setLayout(new GridLayout(2, false));
            fieldGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                    true, 2, 1));
            fieldGroup.setText("Method Parameters");

            new Label(fieldGroup, SWT.RIGHT).setText(" Type: ");
            final Combo typeCbo = new Combo(fieldGroup, SWT.DROP_DOWN
                    | SWT.READ_ONLY);
            typeCbo.setItems(new String[] { "Field", "Constant" });
            gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
            gd.widthHint = 210;
            typeCbo.setLayoutData(gd);

            new Label(fieldGroup, SWT.RIGHT).setText(" Value: ");
            final Text valueText = new Text(fieldGroup, SWT.BORDER);
            gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
            gd.widthHint = 200;
            valueText.setLayoutData(gd);

            Composite buttonComp = new Composite(fieldGroup, SWT.NONE);
            buttonComp.setLayout(new GridLayout(3, true));
            buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                    true, 2, 1));
            final Button add = new Button(buttonComp, SWT.PUSH | SWT.CENTER);
            add.setText("Add Argument");
            gd = new GridData(SWT.FILL, SWT.FILL, false, true);
            gd.widthHint = 135;
            add.setLayoutData(gd);
            add.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    String type = typeCbo.getText().trim();
                    String value = valueText.getText().trim();
                    boolean valid = true;

                    if ("".equals(value)) {
                        valid = false;
                        valueText.setBackground(getDisplay().getSystemColor(
                                SWT.COLOR_RED));
                    } else {
                        valueText.setBackground(getDisplay().getSystemColor(
                                SWT.COLOR_WHITE));
                    }
                    if (valid) {
                        IDerivParamField field = null;
                        if ("Constant".equals(type)) {
                            DerivParamConstantField f = new DerivParamConstantField();
                            try {
                                f.setValue(Float.parseFloat(value));
                            } catch (Throwable t) {
                                MessageDialog
                                        .openError(getShell(), "Error",
                                                "Could not parse constant as a float value");
                                valid = false;
                            }
                            field = f;
                        } else {
                            DerivParamField f = new DerivParamField();
                            f.setParam(value);
                            field = f;
                        }

                        if (valid) {
                            TableItem ti = new TableItem(methodParamTable,
                                    SWT.NONE);
                            ti.setText(new String[] { type, value });
                            ti.setData(field);
                            for (TableColumn tc : methodParamTable.getColumns()) {
                                tc.pack();
                            }
                        }
                    }
                }
            });

            final Button update = new Button(buttonComp, SWT.PUSH | SWT.CENTER);
            update.setText("Update Argument");
            gd = new GridData(SWT.FILL, SWT.FILL, false, true);
            gd.widthHint = 135;
            update.setLayoutData(gd);
            update.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    String type = typeCbo.getText().trim();
                    String value = valueText.getText().trim();
                    boolean valid = true;
                    if ("".equals(value)) {
                        valid = false;
                        valueText.setBackground(getDisplay().getSystemColor(
                                SWT.COLOR_RED));
                    } else {
                        valueText.setBackground(getDisplay().getSystemColor(
                                SWT.COLOR_WHITE));
                    }
                    if (valid) {
                        TableItem ti = methodParamTable.getSelection()[0];
                        ti.setText(new String[] { type, value });
                        for (TableColumn tc : methodParamTable.getColumns()) {
                            tc.pack();
                        }
                    }
                }
            });

            final Button remove = new Button(buttonComp, SWT.PUSH | SWT.CENTER);
            remove.setText("Remove Argument");
            gd = new GridData(SWT.FILL, SWT.FILL, false, true);
            gd.widthHint = 135;
            remove.setLayoutData(gd);
            remove.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    methodParamTable.remove(methodParamTable
                            .getSelectionIndices());
                }
            });

            methodParamTable = new Table(fieldGroup, SWT.BORDER | SWT.V_SCROLL
                    | SWT.H_SCROLL | SWT.SINGLE);
            gd = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
            gd.heightHint = 200;
            methodParamTable.setLayoutData(gd);
            methodParamTable.setHeaderVisible(true);
            methodParamTable.setLinesVisible(true);
            methodParamTable.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (methodParamTable.getSelectionCount() == 0) {
                        remove.setEnabled(false);
                        update.setEnabled(false);
                    } else {
                        TableItem ti = methodParamTable.getSelection()[0];
                        typeCbo.setText(ti.getText(0));
                        valueText.setText(ti.getText(1));
                        remove.setEnabled(true);
                        update.setEnabled(true);
                    }
                }
            });

            TableColumn tc = new TableColumn(methodParamTable, SWT.NONE);
            tc.setText(" Parameter Type ");
            tc.setResizable(true);
            tc.pack();

            tc = new TableColumn(methodParamTable, SWT.NONE);
            tc.setText(" Parameter Value ");
            tc.setResizable(true);
            tc.pack();

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
                        if (methodParamTable.getItemCount() == 0) {
                            MessageDialog
                                    .openError(getShell(), "Error",
                                            "Must have at least one parameter for the method call");
                        } else {
                            setReturnValue(true);
                            shell.close();
                        }
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

            if (method != null) {
                nameText.setText(method.getName());
                for (IDerivParamField field : method.getFields()) {
                    if (field instanceof DerivParamField) {
                        TableItem ti = new TableItem(methodParamTable, SWT.NONE);
                        ti.setText(new String[] { "Field",
                                ((DerivParamField) field).getParam() });
                        ti.setData(field);
                    } else {
                        TableItem ti = new TableItem(methodParamTable, SWT.NONE);
                        ti.setText(new String[] {
                                "Constant",
                                ((DerivParamConstantField) field).getValue()
                                        .toString() });
                        ti.setData(field);
                    }
                }
            }
        }
    }
}
