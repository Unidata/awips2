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
package com.raytheon.viz.pointdata.def.ui;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.viz.pointdata.def.ColorCondition;
import com.raytheon.viz.pointdata.def.ConditionalColor;
import com.raytheon.viz.pointdata.def.ParamConstraint;
import com.raytheon.viz.pointdata.def.PlotParameterDefinitions;
import com.raytheon.viz.pointdata.def.PlotParameterDefinitionsManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * EditConditionalColorDialog used when user selects different colors for
 * different value points
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/10/2019   71272      Mark Peters   Initial Creation
 * Jan 07, 2020 73083      ksunil        changes to absorb the new ColorCondition class
 *
 * </pre>
 *
 * @author mpeters
 */

public class EditConditionalColorDialog extends CaveSWTDialog {

    private final ConditionalColor conditionalColor;

    private final PlotParameterDefinitions paramDefs;

    private Table conditionsTable;

    private Button addButton, removeButton, upButton, downButton;

    private ColorFieldEditor conditionColorEditor;

    private Group conditionGroup;

    private final String[] params;

    private static final String[] availableConstraints = Arrays
            .stream(ConstraintType.values()).map(ConstraintType::getOperand)
            .toArray(String[]::new);

    public EditConditionalColorDialog(Shell parent, String plugin,
            ConditionalColor conditionalColor) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.conditionalColor = new ConditionalColor(conditionalColor);
        this.paramDefs = PlotParameterDefinitionsManager.getInstance()
                .getDefinitions(plugin);
        this.params = paramDefs.getParamDisplayNamesForColorMap()
                .toArray(new String[0]);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(new GridLayout());

        ColorFieldEditor cfe = new ColorFieldEditor("", "Default color:",
                new Composite(comp, SWT.NONE));
        ColorSelector colorSelector = cfe.getColorSelector();
        colorSelector.setColorValue(conditionalColor.getDefaultColor());
        colorSelector.addListener(new IPropertyChangeListener() {

            @Override
            public void propertyChange(PropertyChangeEvent event) {
                conditionalColor.setDefaultColor(
                        cfe.getColorSelector().getColorValue());
            }
        });

        Composite listComp = new Composite(comp, SWT.NONE);
        listComp.setLayout(new GridLayout(2, false));
        listComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        conditionsTable = new Table(listComp,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        conditionsTable
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        conditionsTable.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                handleConditionSelected();
            }
        });

        Composite buttonsComp = new Composite(listComp, SWT.NONE);
        buttonsComp.setLayout(new GridLayout(1, true));

        addButton = new Button(buttonsComp, SWT.NONE);
        addButton.setText("Add");
        addButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        addButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                ColorCondition newCondition = new ColorCondition(
                        new RGB(255, 255, 255));
                conditionalColor.getConditionsAndColors().add(newCondition);
                populateConditionsTable(newCondition);
            }
        });

        removeButton = new Button(buttonsComp, SWT.NONE);
        removeButton.setText("Remove");
        removeButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        removeButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                TableItem item = conditionsTable.getSelection()[0];
                conditionalColor.getConditionsAndColors()
                        .remove(item.getData());
                populateConditionsTable(null);
            }
        });

        upButton = new Button(buttonsComp, SWT.NONE);
        upButton.setText("Up");
        upButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        upButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                handleUpDown(true);
            }
        });

        downButton = new Button(buttonsComp, SWT.NONE);
        downButton.setText("Down");
        downButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        downButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                handleUpDown(false);
            }
        });

        conditionColorEditor = new ColorFieldEditor("", "Condition color:",
                new Composite(comp, SWT.NONE));
        conditionColorEditor.getColorSelector()
                .addListener(new IPropertyChangeListener() {

                    @Override
                    public void propertyChange(PropertyChangeEvent event) {
                        getSelectedCondition().setColor(conditionColorEditor
                                .getColorSelector().getColorValue());
                    }
                });

        conditionGroup = new Group(comp, SWT.NONE);
        conditionGroup.setText("Condition");
        conditionGroup.setLayout(new GridLayout(4, false));

        Composite bottomButtonsComp = new Composite(comp, SWT.NONE);
        bottomButtonsComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        bottomButtonsComp.setLayout(new GridLayout(2, true));

        Button cancel = new Button(bottomButtonsComp, SWT.PUSH);
        cancel.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        cancel.setText("Cancel");
        cancel.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

        Button ok = new Button(bottomButtonsComp, SWT.PUSH);
        ok.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        ok.setText("OK");
        ok.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                setReturnValue(conditionalColor);
                close();
            }
        });

        populateConditionsTable(null);
    }

    private void handleUpDown(boolean up) {
        TableItem item = conditionsTable.getSelection()[0];
        ColorCondition condition = (ColorCondition) item.getData();
        List<ColorCondition> conditionsAndColors = conditionalColor
                .getConditionsAndColors();
        int index = conditionsAndColors.indexOf(condition);
        int newIndex = up ? index - 1 : index + 1;
        Collections.swap(conditionsAndColors, index, newIndex);

        populateConditionsTable(condition);
    }

    private void handleConditionSelected() {

        int selectionIndex = conditionsTable.getSelectionIndex();
        boolean hasSelection = selectionIndex >= 0;
        removeButton.setEnabled(hasSelection);
        upButton.setEnabled(hasSelection && selectionIndex > 0);
        downButton.setEnabled(hasSelection
                && selectionIndex < conditionsTable.getItemCount() - 1);

        populateCondition();
    }

    private ColorCondition getSelectedCondition() {
        TableItem[] selection = conditionsTable.getSelection();
        if (selection.length == 1) {
            return (ColorCondition) selection[0].getData();
        }
        return null;
    }

    private void populateConditionsTable(ColorCondition selected) {
        for (TableItem item : conditionsTable.getItems()) {
            item.dispose();
        }

        List<ColorCondition> conditionsAndColors = conditionalColor
                .getConditionsAndColors();

        TableItem selectedItem = null;
        for (ColorCondition condition : conditionsAndColors) {
            TableItem item = new TableItem(conditionsTable, SWT.NONE);
            item.setData(condition);
            updateConditionText(item);

            if (condition == selected) {
                selectedItem = item;
            }
        }

        if (selectedItem != null) {
            conditionsTable.setSelection(selectedItem);
        } else {
            conditionsTable.deselectAll();
        }

        handleConditionSelected();
    }

    private void populateCondition() {
        for (Control child : conditionGroup.getChildren()) {
            // recursive?
            child.dispose();
        }

        ColorCondition condition = getSelectedCondition();

        Button addConstraintButton = new Button(conditionGroup, SWT.PUSH);
        addConstraintButton.setText("Add");
        addConstraintButton.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        addConstraintButton.setEnabled(condition != null);
        addConstraintButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                getSelectedCondition().addParamConstraint(new ParamConstraint(
                        params[0], new RequestConstraint("")));
                populateCondition();
                // Refresh table item name
            }
        });

        Label paramLabel = new Label(conditionGroup, SWT.NONE);
        paramLabel.setText("Parameter Name");

        Label constraintLabel = new Label(conditionGroup, SWT.NONE);
        constraintLabel.setText("Constraint Type");

        Label valueLabel = new Label(conditionGroup, SWT.NONE);
        valueLabel.setText("Value");

        conditionColorEditor.getColorSelector().setEnabled(condition != null);

        if (condition != null) {
            conditionColorEditor.getColorSelector()
                    .setColorValue(condition.getColor());

            List<ParamConstraint> constraints = condition.getParamConstraints();
            for (ParamConstraint constraint : constraints) {
                Button removeConstraintButton = new Button(conditionGroup,
                        SWT.NONE);
                removeConstraintButton.setText("Remove");
                removeConstraintButton
                        .addSelectionListener(new SelectionAdapter() {

                            @Override
                            public void widgetSelected(SelectionEvent e) {
                                condition.removeParamConstraint(constraint);
                                populateCondition();
                                updateSelectedConditionText();
                            }
                        });

                Combo paramCombo = new Combo(conditionGroup, SWT.READ_ONLY);
                paramCombo.setItems(params);
                paramCombo.setText(constraint.getParam());
                paramCombo.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        constraint.setParam(paramCombo.getText());
                        updateSelectedConditionText();
                    }
                });

                Combo constraintCombo = new Combo(conditionGroup,
                        SWT.READ_ONLY);
                constraintCombo.setItems(availableConstraints);
                constraintCombo.setText(constraint.getConstraint()
                        .getConstraintType().getOperand());
                constraintCombo.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        constraint.getConstraint()
                                .setConstraintType(ConstraintType.fromOperand(
                                        constraintCombo.getText()));
                        updateSelectedConditionText();
                    }
                });

                Text valueText = new Text(conditionGroup, SWT.BORDER);
                valueText.setText(
                        constraint.getConstraint().getConstraintValue());
                valueText.addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {
                        constraint.getConstraint()
                                .setConstraintValue(valueText.getText());
                        updateSelectedConditionText();
                    }
                });
            }
        }
        conditionGroup.layout(true, true);
        conditionGroup.pack();
        getShell().pack();
    }

    private void updateSelectedConditionText() {
        updateConditionText(conditionsTable.getSelection()[0]);
    }

    private void updateConditionText(TableItem item) {
        ColorCondition condition = (ColorCondition) item.getData();
        String constraintsStr = condition.getParamConstraints().stream()
                .map(ParamConstraint::toString)
                .collect(Collectors.joining(" and "));

        item.setText(constraintsStr);
    }
}
