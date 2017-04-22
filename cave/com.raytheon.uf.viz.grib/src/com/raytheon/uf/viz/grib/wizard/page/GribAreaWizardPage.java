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
package com.raytheon.uf.viz.grib.wizard.page;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.grib.GribCoverageStore;
import com.raytheon.uf.common.grib.GridModel;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.viz.grib.DefineGridCoverageComposite;
import com.raytheon.uf.viz.grib.wizard.GribWizardData;

/**
 * 
 * Wizard page which allows the user to select or define a grid definition for a
 * grib model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribAreaWizardPage extends AbstractGribWizardPage {

    private final GribCoverageStore existingCoverages;

    private Button noAreaRadio;

    private Button predefinedAreaRadio;

    private Button newAreaRadio;

    private Combo predefinedAreaCombo;

    private DefineGridCoverageComposite newGridComposite;

    public GribAreaWizardPage(GribCoverageStore coverageStore,
            GribWizardData data) {
        super("Grib Area", data);
        setDescription("Define the area for a new Grib Model");
        this.existingCoverages = coverageStore;
    }

    @Override
    public void createControl(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(1, false));
        noAreaRadio = new Button(container, SWT.RADIO);
        Point radioSize = noAreaRadio.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        noAreaRadio.setText("Use this model for all areas");
        predefinedAreaRadio = new Button(container, SWT.RADIO);
        predefinedAreaRadio.setText("Use a predefined area");
        predefinedAreaCombo = new Combo(container, SWT.NONE);
        GridData gridData = new GridData();
        gridData.horizontalIndent = radioSize.x;
        predefinedAreaCombo.setLayoutData(gridData);
        newAreaRadio = new Button(container, SWT.RADIO);
        newAreaRadio.setText("Define a new Area");

        newGridComposite = new DefineGridCoverageComposite(container);
        newGridComposite.setEnabled(false);
        gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalIndent = radioSize.x;
        gridData.horizontalAlignment = SWT.FILL;
        newGridComposite.setLayoutData(gridData);

        predefinedAreaCombo.setItems(existingCoverages.getAllCoverageNames()
                .toArray(new String[0]));

        ModifyListener mListener = new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                updatePageComplete();
            }

        };

        SelectionListener sListener = new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updatePageComplete();
            }

        };

        noAreaRadio.addSelectionListener(sListener);
        predefinedAreaRadio.addSelectionListener(sListener);
        newAreaRadio.addSelectionListener(sListener);
        predefinedAreaCombo.addSelectionListener(sListener);
        predefinedAreaCombo.addModifyListener(mListener);

        newGridComposite.addListeners(sListener, mListener);

        populateFromData();

        setControl(container);
    }

    @Override
    public void populateFromData() {
        GridModel model = data.getModel();

        List<String> grids = null;
        if (model != null) {
            grids = model.getGrids();
        }
        if (data.getCoverage() != null) {
            noAreaRadio.setSelection(false);
            predefinedAreaRadio.setSelection(false);
            newAreaRadio.setSelection(true);
            predefinedAreaCombo.setEnabled(false);
            newGridComposite.setEnabled(true);
            newGridComposite.setCoverage(data.getCoverage());
        } else if (grids == null || grids.isEmpty()) {
            noAreaRadio.setSelection(true);
            predefinedAreaRadio.setSelection(false);
            newAreaRadio.setSelection(false);
            predefinedAreaCombo.setEnabled(false);
            newGridComposite.setEnabled(false);
            newGridComposite.setCoverage(null);
        } else {
            noAreaRadio.setSelection(false);
            predefinedAreaRadio.setSelection(true);
            newAreaRadio.setSelection(false);

            predefinedAreaCombo.setEnabled(true);
            newGridComposite.setEnabled(false);
            predefinedAreaCombo.setText(grids.get(0));
            newGridComposite.setCoverage(null);
        }

        updatePageComplete();
    }

    private void updatePageComplete() {
        setErrorMessage(null);

        predefinedAreaCombo.setEnabled(predefinedAreaRadio.getSelection());
        newGridComposite.setEnabled(newAreaRadio.getSelection());

        if (noAreaRadio.getSelection()) {
            data.getOrCreateModel().setGrids(null);
            data.setCoverage(null);
            setPageComplete(true);
        } else if (predefinedAreaRadio.getSelection()) {
            String area = predefinedAreaCombo.getText();
            if (area.isEmpty()) {
                setPageComplete(false);
                setErrorMessage("Please select an area.");
            } else if (predefinedAreaCombo.indexOf(area) < 0) {
                setPageComplete(false);
                setErrorMessage("Predefined Area is not defined.");
            } else {
                data.getOrCreateModel().setGrids(Arrays.asList(area));
                data.setCoverage(null);
                setPageComplete(true);
            }
        } else if (newAreaRadio.getSelection()) {
            String error = newGridComposite.validate();
            if (error != null) {
                setPageComplete(false);
                setErrorMessage(error);
            } else {
                GridCoverage coverage = newGridComposite.getCoverage();
                GridCoverage existingCoverage = existingCoverages
                        .getCoverageByName(coverage.getName());
                Set<String> existingNames = existingCoverages
                        .getGribCoverageNames(coverage);
                if (!existingNames.isEmpty()) {
                    setPageComplete(false);
                    setErrorMessage("Area is already defined as "
                            + existingNames.iterator().next());
                } else if (existingCoverage != null) {
                    setPageComplete(false);
                    setErrorMessage("Another coverage already exists named "
                            + coverage.getName());
                } else {
                    data.setCoverage(coverage);
                    data.getOrCreateModel().setGrids(
                            Arrays.asList(coverage.getName()));
                    setPageComplete(true);
                }
            }
        } else {
            setPageComplete(false);
            setErrorMessage("Must Pick a selection.");
        }

    }
}