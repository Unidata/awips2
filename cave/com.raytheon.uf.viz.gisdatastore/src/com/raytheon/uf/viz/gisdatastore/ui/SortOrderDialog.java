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
package com.raytheon.uf.viz.gisdatastore.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.type.Pair;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog to allow the user to define the sort order for the
 * {@link AttributeViewer}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SortOrderDialog extends CaveJFACEDialog {
    private class SortField {
        public static final int ASCENDING_VALUE = 1;

        public static final int DESCENDING_VALUE = -1;

        private static final String ASCENDING_TEXT = "Ascending";

        private static final String DESCENDING_TEXT = "Descending";

        private Group group;

        private Combo columnCombo;

        private Combo orderCombo;

        public SortField(Composite parent) {
            group = new Group(parent, SWT.DEFAULT);
            GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false);
            group.setLayoutData(layoutData);
            GridLayout layout = new GridLayout(2, false);
            group.setLayout(layout);
            group.setText("Then by:");

            columnCombo = new Combo(group, SWT.DROP_DOWN);
            layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
            columnCombo.setLayoutData(layoutData);
            columnCombo.add("", 0);
            columnCombo.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    updateFields();
                }
            });

            orderCombo = new Combo(group, SWT.DROP_DOWN);
            layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
            orderCombo.setLayoutData(layoutData);
            orderCombo.add(ASCENDING_TEXT);
            orderCombo.add(DESCENDING_TEXT);
            orderCombo.setText(orderCombo.getItem(0));
        }

        public void dispose() {
            group.dispose();
        }

        public String getField() {
            return columnCombo.getText();
        }

        public void setField(String field) {
            this.columnCombo.setText(field);
        }

        public int getOrder() {
            return (orderCombo.getText().equals(ASCENDING_TEXT) ? ASCENDING_VALUE
                    : DESCENDING_VALUE);
        }

        public void setOrder(int order) {
            if (order > 0) {
                orderCombo.setText(ASCENDING_TEXT);
            } else if (order < 0) {
                orderCombo.setText(DESCENDING_TEXT);
            }
        }

        public void setFields(List<String> fields) {
            String currentField = getField();
            columnCombo.removeAll();
            columnCombo.add("");
            for (String f : fields) {
                columnCombo.add(f);
            }
            if (fields.contains(currentField)) {
                columnCombo.setText(currentField);
            } else {
                columnCombo.setText("");
            }
        }

        public void setLabel(String label) {
            group.setText(label);
        }
    }

    private String[] columns;

    private List<Pair<String, Integer>> initialOrder;

    private Composite mainComp;

    private List<SortField> sortFields;

    protected SortOrderDialog(Shell parentShell, String[] columns,
            List<Pair<String, Integer>> order) {
        super(parentShell);
        this.columns = columns;
        this.initialOrder = order;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Sort Order");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        mainComp = (Composite) super.createDialogArea(parent);

        this.sortFields = new ArrayList<SortField>();
        for (Pair<String, Integer> p : initialOrder) {
            addField(p.getFirst(), p.getSecond());
        }
        updateFields();

        return mainComp;
    }

    private SortField addField(String field, int order) {
        SortField newField = new SortField(mainComp);
        newField.setField(field);
        newField.setOrder(order);
        sortFields.add(newField);
        return newField;
    }

    private void removeField(SortField field) {
        sortFields.remove(field);
        field.dispose();
    }

    private void updateFields() {
        List<String> availableFields = new ArrayList<String>(
                Arrays.asList(columns));
        for (int i = 0; i < sortFields.size(); i++) {
            SortField sf = sortFields.get(i);
            sf.setFields(availableFields);
            String s = sf.getField();
            if (s.isEmpty()) {
                if (i < sortFields.size() - 1) {
                    removeField(sf);
                    i--;
                }
            } else {
                availableFields.remove(s);
            }
        }

        if (sortFields.isEmpty()
                || !sortFields.get(sortFields.size() - 1).getField().isEmpty()) {
            SortField sf = addField("", SortField.ASCENDING_VALUE);
            sf.setFields(availableFields);
        }

        sortFields.get(0).setLabel("Sort by:");

        mainComp.getShell().pack();
    }

    public List<Pair<String, Integer>> getSortOrder() {
        List<Pair<String, Integer>> sortOrder = new ArrayList<Pair<String, Integer>>();
        for (SortField sf : sortFields) {
            if (sf.getField().isEmpty()) {
                continue;
            }
            sortOrder.add(new Pair<String, Integer>(sf.getField(), sf
                    .getOrder()));
        }

        return sortOrder;
    }
}
