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
package com.raytheon.uf.viz.auto.transition;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.auto.transition.AverageValueCalculator.CalculatorListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Dialog for configuring a {@link AutoTransitionResource}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 09, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AutoTransitionDialog extends CaveSWTDialog implements
        CalculatorListener {

    private final AutoTransitionResource resource;

    private int originalControlIndex;

    private double originalThreshold;

    private Combo resourceCombo;
    
    private Scale thresholdSlider;

    private Text thresholdText;
    
    private Text currentValueText;
    
    public AutoTransitionDialog(Shell parentShell,AutoTransitionResource resource) {
        super(parentShell);
        this.resource = resource;
        setText("Configure Automatic Transition");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        resource.getCalculator().addListener(this);
        new Label(shell, SWT.NONE).setText("Control Resource: ");
        resourceCombo = new Combo(shell, SWT.READ_ONLY);
        for (ResourcePair pair : resource.getResourceList()) {
            resourceCombo.add(pair.getResource().getName());
        }
        resourceCombo.select(resource.getResourceData().getControlIndex());
        GridData layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        layoutData.horizontalSpan = 2;
        resourceCombo.setLayoutData(layoutData);

        resourceCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                resourceComboSelected();
            }
        });

        new Label(shell, SWT.NONE).setText("Threshold Value: ");
        thresholdSlider = new Scale(shell, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        layoutData.minimumWidth = 100;
        thresholdSlider.setLayoutData(layoutData);
        
        originalThreshold = resource.getResourceData()
                .getThresholdValue();
        originalControlIndex = resource.getResourceData().getControlIndex();
        
        ResourcePair controlPair = resource.getResourceList().get(resource.getResourceData().getControlIndex());
        final ColorMapParameters params = controlPair.getResource().getCapability(ColorMapCapability.class).getColorMapParameters();

        int sliderSpan = thresholdSlider.getMaximum()
                - thresholdSlider.getMinimum();
        double paramSpan = params.getColorMapMax() - params.getColorMapMin();

        thresholdSlider
                .setSelection((int) (thresholdSlider.getMinimum() + sliderSpan
                        * (originalThreshold - params.getColorMapMin())
                        / paramSpan));

        
        thresholdText = new Text(shell, SWT.BORDER | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, false, false);
        layoutData.widthHint = 50;
        thresholdText.setLayoutData(layoutData);
        thresholdText.setText(Double.toString(originalThreshold));
        
        thresholdSlider.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                sliderSelected();
            }
        });
        new Label(shell, SWT.NONE).setText("Current Value: ");
        currentValueText = new Text(shell, SWT.BORDER | SWT.READ_ONLY);
        currentValueText
                .setText(Double.toString(resource.getCalculator().getValue()));
        layoutData = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        layoutData.widthHint = 50;
        currentValueText.setLayoutData(layoutData);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        layoutData = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        layoutData.horizontalSpan = 3;
        buttonComp.setLayoutData(layoutData);

        buttonComp.setLayout(new RowLayout(SWT.HORIZONTAL));
        Button okButton = new Button(buttonComp, SWT.PUSH);
        okButton.setText("OK");
        okButton.setLayoutData(new RowData(100, SWT.DEFAULT));
        okButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                okPressed();
            }

        });

        Button cancelButton = new Button(buttonComp, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(new RowData(100, SWT.DEFAULT));
        cancelButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                cancelPressed();
            }

        });

    }

    protected void resourceComboSelected() {
        resource.getResourceData().setControlIndex(
                resourceCombo.getSelectionIndex());
        sliderSelected();
    }

    protected void sliderSelected() {
        ResourcePair controlPair = resource.getResourceList().get(
                resource.getResourceData().getControlIndex());
        final ColorMapParameters params = controlPair.getResource()
                .getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        int sliderSpan = thresholdSlider.getMaximum()
                - thresholdSlider.getMinimum();
        double paramSpan = params.getColorMapMax() - params.getColorMapMin();

        double value = params.getColorMapMin()
                + paramSpan
                * (thresholdSlider.getSelection() - thresholdSlider
                        .getMinimum()) / sliderSpan;
        resource.getResourceData().setThresholdValue(value);
        resource.issueRefresh();
        thresholdText.setText(Double.toString(value));
    }

    protected void okPressed() {
        close();
    }

    protected void cancelPressed() {
        resource.getResourceData().setThresholdValue(originalThreshold);
        resource.getResourceData().setControlIndex(originalControlIndex);
        resource.issueRefresh();
        close();
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(3, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        super.disposed();
        resource.getCalculator().removeListener(this);
    }

    @Override
    public void calculationComplete() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (currentValueText.isDisposed()) {
                    return;
                }
                currentValueText
                .setText(Double.toString(resource.getCalculator().getValue()));

            }

        });
    }

}
