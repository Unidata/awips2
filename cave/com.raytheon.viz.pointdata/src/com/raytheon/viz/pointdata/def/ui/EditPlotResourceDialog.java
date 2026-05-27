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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.pointdata.IPlotModelFactory;
import com.raytheon.viz.pointdata.PlotModelFactory;
import com.raytheon.viz.pointdata.def.Condition;
import com.raytheon.viz.pointdata.def.ConditionalFilter;
import com.raytheon.viz.pointdata.def.ConditionalFilterElement;
import com.raytheon.viz.pointdata.def.ConditionalFilterFactory;
import com.raytheon.viz.pointdata.def.ParamConstraint;
import com.raytheon.viz.pointdata.rsc.PlotResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * EditPlotResourceDialog is called A) when user right clicks and selects "Edit
 * Plot attributes". B) also when plot model is edited through
 * Tools->Plots->Plot Model Manager. Initializes EditPlotModelComposite dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/10/2019   71272      Mark Peters   Initial Creation
 * 12/10/2019   72280      K Sunil       code to switch between NCP and D2D perspective
 * Jan 07, 2020 73083      ksunil        Check for null filter while Applying filter. Give a "no filter"
 *                                        option.
 * Feb 25, 2020 75195      ksunil        show the .svg name on the Dialog's title bar.
 *
 * </pre>
 *
 * @author mpeters
 */

public class EditPlotResourceDialog extends CaveSWTDialog {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditPlotResourceAction.class);

    private final PlotResource resource;

    private final String plotFileName;

    private EditPlotModelComposite plotModelComp;

    public EditPlotResourceDialog(Shell parent, PlotResource resource) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Plot Model" + " - "
                + resource.getResourceData().getPlotModelFile());
        this.plotFileName = null;
        this.resource = resource;
        resource.registerListener(new IDisposeListener() {

            @Override
            public void disposed(AbstractVizResource<?, ?> rsc) {
                close();
            }
        });
    }

    public EditPlotResourceDialog(Shell parent, String plotFileName) {
        super(parent);
        setText("Plot Model" + " - " + plotFileName);
        this.resource = null;
        this.plotFileName = plotFileName;
    }

    @Override
    protected void initializeComponents(Shell shell) {

        // Conditional filter group
        if (resource != null) {
            Group cfGroup = new Group(shell, SWT.NONE);
            cfGroup.setText("Conditional Filter");
            cfGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
            cfGroup.setLayout(new GridLayout(2, false));
            Combo filterListCombo = new Combo(cfGroup, SWT.READ_ONLY);

            try {
                String plugin = PlotModelFactory
                        .findSVGPluginName(IPlotModelFactory.PLOT_MODEL_DIR
                                + IPathManager.SEPARATOR + resource
                                        .getResourceData().getPlotModelFile());
                String[] condFiltersArray = ConditionalFilterFactory
                        .getFilterManagerInstance(ConditionalFilterFactory.D2D)
                        .getAllConditionalFiltersByPlugin(plugin);
                // Add an empty element at the top. (To reset the filter)
                filterListCombo
                        .setItems(ArrayUtils.add(condFiltersArray, 0, ""));

            } catch (VizException e1) {
                statusHandler.warn(
                        "Unable to find filters for "
                                + resource.getResourceData().getPlotModelFile(),
                        e1);
            }
            Button cfButton = new Button(cfGroup, SWT.PUSH);
            cfButton.setText("Apply...");
            cfButton.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    try {

                        if (!"".equals(filterListCombo.getText())) {
                            ConditionalFilter filter = ConditionalFilterFactory
                                    .getFilterManagerInstance(
                                            ConditionalFilterFactory.D2D)
                                    .getConditionalFilter(
                                            PlotModelFactory.findSVGPluginName(
                                                    IPlotModelFactory.PLOT_MODEL_DIR
                                                            + IPathManager.SEPARATOR
                                                            + resource
                                                                    .getResourceData()
                                                                    .getPlotModelFile()),
                                            filterListCombo.getText());
                            Condition conditionApplied = convertToD2DCondition(
                                    filter);
                            resource.setConditionalFilter(conditionApplied);
                        } else {
                            resource.setConditionalFilter(null);
                        }

                    } catch (VizException e1) {
                        statusHandler.warn(
                                "Unable to apply filters for " + resource
                                        .getResourceData().getPlotModelFile(),
                                e1);

                    }

                }
            });
        }

        // Plot model group
        Group pmGroup = new Group(shell, SWT.NONE);
        pmGroup.setText("Edit Plot Model");
        pmGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        GridLayout gl = new GridLayout();
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        pmGroup.setLayout(gl);

        try {
            if (resource != null) {
                plotModelComp = new EditPlotModelComposite(pmGroup, SWT.NONE,
                        resource.getResourceData().getPlotModelFile(),
                        resource.getCapability(ColorableCapability.class)
                                .getColor());
            } else {
                // TODO How do I grab the color from the resource?
                plotModelComp = new EditPlotModelComposite(pmGroup, SWT.NONE,
                        plotFileName, new RGB(100, 100, 100));

            }
        } catch (VizException e) {
            statusHandler.error("Error opening plot editing dialog", e);
            close();
            return;
        }

        // Bottom buttons
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));
        buttonComp.setLayout(new GridLayout(4, true));

        Button cancel = new Button(buttonComp, SWT.PUSH);
        cancel.setText("Cancel");
        cancel.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        cancel.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

        Button reset = new Button(buttonComp, SWT.PUSH);
        reset.setText("Reset");
        reset.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        reset.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                // TODO reset conditional filter
                plotModelComp.reset();
            }
        });

        Button apply = new Button(buttonComp, SWT.PUSH);
        apply.setText("Apply");
        apply.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        apply.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                apply();
            }
        });

        Button ok = new Button(buttonComp, SWT.PUSH);
        ok.setText("OK");
        ok.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        ok.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                apply();
                close();
            }
        });
    }

    private void apply() {
        plotModelComp.apply();
    }

    private Condition convertToD2DCondition(ConditionalFilter filter) {
        List<ParamConstraint> paramConstraints = new ArrayList<>();
        for (ConditionalFilterElement element : filter
                .getConditionalFilterElements()) {
            ParamConstraint constraint = new ParamConstraint(
                    element.getParamName(),
                    new RequestConstraint(element.getValue(), ConstraintType
                            .valueOf(element.getConstraintType())));
            paramConstraints.add(constraint);
        }
        return new Condition(paramConstraints);
    }
}
