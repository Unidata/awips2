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
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.grib.wizard.GribWizardData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.xml.VbSource;
import com.raytheon.viz.volumebrowser.xml.VbSourceList;

/**
 * 
 * Wizard page for configuring where a model appears in the volume browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 26, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribVbWizardPage extends AbstractGribWizardPage {

    private Button addVbCheck;

    private Combo categoryCombo;

    private Button verticalCheck;

    public GribVbWizardPage(GribWizardData data) {
        super("Volume Browser Configuration", data);
        setTitle(getName());
        setDescription("Configure the Volume Browser for a new Grib Model");
    }

    @Override
    public void createControl(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(2, false));
        addVbCheck = new Button(container, SWT.CHECK);
        addVbCheck.setText("Include model in the Volume Browser");
        GridData gridData = new GridData();
        gridData.horizontalSpan = 2;
        addVbCheck.setLayoutData(gridData);
        Label label = new Label(container, SWT.NONE);
        label.setText("Category");
        gridData = new GridData();
        gridData.horizontalIndent = 20;
        label.setLayoutData(gridData);
        categoryCombo = new Combo(container, SWT.NONE);
        Set<String> categories = new TreeSet<>();
        for (VbSource source : VbSourceList.getInstance().getAllSources()) {
            categories.add(source.getCategory());
        }
        categories.remove("Point");
        categoryCombo.setItems(categories.toArray(new String[0]));

        verticalCheck = new Button(container, SWT.CHECK);
        verticalCheck.setText("Model contains vertical data.");
        verticalCheck
                .setToolTipText("Determines whether the model will be available for Cross Section, Time Height, Var Height, and Sounding.");
        gridData = new GridData();
        gridData.horizontalIndent = 20;
        gridData.horizontalSpan = 2;
        verticalCheck.setLayoutData(gridData);

        SelectionListener selectionListener = new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updatePageComplete();
            }
        };

        ModifyListener modifyListener = new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                updatePageComplete();
            }

        };

        addVbCheck.addSelectionListener(selectionListener);
        categoryCombo.addSelectionListener(selectionListener);
        categoryCombo.addModifyListener(modifyListener);
        verticalCheck.addSelectionListener(selectionListener);

        setControl(container);

        populateFromData();
    }

    @Override
    public void populateFromData() {
        VbSource source = data.getVbSource();
        if (source == null) {
            addVbCheck.setSelection(false);
            categoryCombo.setText("");
            verticalCheck.setSelection(false);
        } else {
            addVbCheck.setSelection(true);
            categoryCombo.setText(source.getCategory());
            verticalCheck.setSelection(source.getViews().contains(
                    ViewMenu.CROSSSECTION));
        }

        updatePageComplete();
    }

    private void updatePageComplete() {
        if (addVbCheck.getSelection()) {
            categoryCombo.setEnabled(true);
            verticalCheck.setEnabled(true);
            String text = categoryCombo.getText();
            if (text == null || text.isEmpty()) {
                setPageComplete(false);
                setErrorMessage("Must select a category.");
            } else {
                VbSource source = new VbSource();
                source.setKey(data.getModel().getName());
                source.setCategory(categoryCombo.getText());
                if (verticalCheck.getSelection()) {
                    source.setViews(Arrays.asList(ViewMenu.values()));
                } else {
                    source.setViews(Arrays.asList(ViewMenu.PLANVIEW,
                            ViewMenu.TIMESERIES));
                }
                data.setVbSource(source);
                setPageComplete(true);
                setErrorMessage(null);
            }
        } else {
            categoryCombo.setEnabled(false);
            verticalCheck.setEnabled(false);
            data.setVbSource(null);
            setPageComplete(true);
            setErrorMessage(null);
        }
    }

}
