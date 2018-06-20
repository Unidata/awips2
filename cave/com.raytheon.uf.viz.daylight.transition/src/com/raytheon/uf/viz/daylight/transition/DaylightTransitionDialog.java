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
package com.raytheon.uf.viz.daylight.transition;

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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.daylight.transition.resource.DaylightTransitionBlendedResource;
import com.raytheon.uf.viz.daylight.transition.resource.DaylightTransitionBlendedResourceData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Dialog for configuring a {@link DaylightTransitionBlendedResource}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DaylightTransitionDialog extends CaveSWTDialog implements
        IDisposeListener {

    private final DaylightTransitionBlendedResource resource;

    private int originalTransitionIndex;

    private int originalDelta;

    private Combo resourceCombo;

    private Scale deltaSlider;

    private Text deltaText;

    public DaylightTransitionDialog(Shell parentShell,
            DaylightTransitionBlendedResource resource) {
        super(parentShell);
        this.resource = resource;
        setText("Configure Daylight Transition");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        resource.registerListener(this);
        new Label(shell, SWT.NONE).setText("Transition Resource: ");
        resourceCombo = new Combo(shell, SWT.READ_ONLY);
        for (ResourcePair pair : resource.getResourceList()) {
            resourceCombo.add(pair.getResource().getName());
        }
        resourceCombo.select(resource.getResourceData().getTransitionIndex());
        GridData layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        layoutData.horizontalSpan = 2;
        resourceCombo.setLayoutData(layoutData);

        resourceCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                resourceComboSelected();
            }
        });

        new Label(shell, SWT.NONE).setText("Sun Offset Minutes: ");
        deltaSlider = new Scale(shell, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        layoutData.minimumWidth = 100;
        deltaSlider.setLayoutData(layoutData);
        deltaSlider
                .setToolTipText("The amount of time after sunrise and before sunset to mask the data.");

        originalDelta = resource.getResourceData().getSunDelta();
        originalTransitionIndex = resource.getResourceData()
                .getTransitionIndex();

        deltaSlider.setSelection(originalDelta);

        deltaText = new Text(shell, SWT.BORDER | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, false, false);
        layoutData.widthHint = 50;
        deltaText.setLayoutData(layoutData);
        deltaText.setText(Double.toString(originalDelta));

        deltaSlider.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                sliderSelected();
            }
        });
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
        resource.getResourceData().setTransitionIndex(
                resourceCombo.getSelectionIndex());
        resource.getResourceData().applyTransitionIndex();

        sliderSelected();
    }

    protected void sliderSelected() {
        resource.getResourceData().setSunDelta(deltaSlider.getSelection());
        resource.issueRefresh();
        deltaText.setText(Integer.toString(deltaSlider.getSelection()));
    }

    protected void okPressed() {
        close();
    }

    protected void cancelPressed() {
        DaylightTransitionBlendedResourceData resourceData = resource
                .getResourceData();
        resourceData.setSunDelta(originalDelta);
        if (resourceData.getTransitionIndex() != originalTransitionIndex) {
            resourceData.setTransitionIndex(originalTransitionIndex);
            resourceData.applyTransitionIndex();
        }
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
        resource.unregisterListener(this);
        super.disposed();
    }

    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {
        if (rsc == resource) {
            Display.getCurrent().asyncExec(new Runnable() {

                @Override
                public void run() {
                    close();
                }
            });
        }
    }

}
