package com.raytheon.uf.viz.damagepath;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Dialog to add/remove the key/value pairs that are part of the "properties"
 * member object in GeoJSON Feature objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2015  #4354     dgilling     Initial creation based on dialog from 
 *                                      lvenable.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class EditGeoJsonPropertiesDlg extends CaveSWTDialog {

    private Table table;

    private Button deleteBtn;

    private Map<String, String> properties;

    public EditGeoJsonPropertiesDlg(Shell parentShell,
            Map<String, String> properties) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);

        this.properties = new LinkedHashMap<>(properties);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;

        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        return gd;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setText("GeoJSON Properties Editor");

        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                setReturnValue(properties);
            }
        });

        createTableComp();
        createAddDeleteButtons();
        addSeparator(shell, SWT.HORIZONTAL);
        createBottomButtons();

        populateTable();
    }

    private void createTableComp() {
        int columnWidth = 150;
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 250;
        gd.widthHint = columnWidth * 2;

        table = new Table(shell, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.SINGLE);
        table.setLayoutData(gd);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        TableColumn column = new TableColumn(table, SWT.NONE);
        column.setText("Key");
        column = new TableColumn(table, SWT.NONE);
        column.setText("Value");

        for (int i = 0; i < table.getColumnCount(); i++) {
            table.getColumn(i).setWidth(columnWidth);
        }
    }

    private void createAddDeleteButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        int buttonWidth = 70;

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText(" Add ");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                AddKeyValueDlg akvd = new AddKeyValueDlg(shell, properties
                        .keySet());
                akvd.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue == null) {
                            return;
                        }

                        if (returnValue instanceof Pair) {
                            Pair<String, String> newProperty = (Pair<String, String>) returnValue;
                            properties.put(newProperty.getFirst(),
                                    newProperty.getSecond());
                            populateTable();
                        }
                    }
                });
                akvd.open();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText(" Delete ");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDeleteAction();
            }
        });
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        int buttonWidth = 70;

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText(" Close ");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(properties);
                close();
            }
        });
    }

    public void addSeparator(Composite comp, int orientation) {
        GridData gd;

        if (orientation == SWT.HORIZONTAL) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

            if (comp.getLayout() instanceof GridLayout) {
                int columns = ((GridLayout) comp.getLayout()).numColumns;
                gd.horizontalSpan = columns;
            }
        } else {
            gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        }

        Label sepLbl = new Label(comp, SWT.SEPARATOR | orientation);
        sepLbl.setLayoutData(gd);
    }

    private void populateTable() {
        table.removeAll();
        for (Entry<String, String> entry : properties.entrySet()) {
            TableItem ti = new TableItem(table, SWT.NONE);
            ti.setText(0, entry.getKey());
            ti.setText(1, entry.getValue());
        }

        if (table.getItemCount() > 0) {
            table.select(0);
        }

        enableDeleteButton();
    }

    private void handleDeleteAction() {
        int index = table.getSelectionIndex();

        if (index < 0) {
            return;
        }

        properties.remove(table.getItem(index).getText(0));
        populateTable();

        if (index < table.getItemCount()) {
            table.select(index);
        } else if (table.getItemCount() > 0) {
            table.select(table.getItemCount() - 1);
        }
    }

    private void enableDeleteButton() {
        if ((table.getItemCount() > 0) && (table.getSelectionIndex() >= 0)) {
            deleteBtn.setEnabled(true);
        } else {
            deleteBtn.setEnabled(false);
        }
    }
}
