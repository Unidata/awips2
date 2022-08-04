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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.jface.wizard.WizardPage;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.grib.wizard.GribWizardData;

/**
 * 
 * Wizard page allowing the user to choose a location to save the data created
 * in the wizard.
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
public class GribExportPage extends AbstractGribWizardPage {

    private Button localizationCheck;

    private Button exportCheck;

    private Text exportLocationText;

    private Button exportLocationButton;

    public GribExportPage(GribWizardData data) {
        super("Save Settings", data);
        setDescription("Choose how to save the new grib model. ");
    }

    @Override
    public void createControl(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(1, false));
        localizationCheck = new Button(container, SWT.CHECK);
        localizationCheck.setText("Save to SITE localization files");

        exportCheck = new Button(container, SWT.CHECK);
        Point checkSize = exportCheck.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        exportCheck.setText("Export settings to an archive file.");
        exportCheck
                .setToolTipText("Archive files can be imported through this wizard or unpacked into localization.");

        Composite exportLocationComposite = new Composite(container, SWT.NONE);
        exportLocationComposite.setLayout(new GridLayout(2, false));
        GridData gridData = new GridData();
        gridData.horizontalIndent = checkSize.x;
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        exportLocationComposite.setLayoutData(gridData);

        exportLocationText = new Text(exportLocationComposite, SWT.BORDER
                | SWT.SINGLE);
        exportLocationText.setToolTipText(exportCheck.getToolTipText());
        gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        exportLocationText.setLayoutData(gridData);

        exportLocationButton = new Button(exportLocationComposite, SWT.PUSH);
        exportLocationButton.setText("Browse ...");
        exportLocationButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectDestinationFile();
            }
        });

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

        localizationCheck.addSelectionListener(selectionListener);
        exportCheck.addSelectionListener(selectionListener);
        exportLocationText.addModifyListener(modifyListener);

        setControl(container);
        setPageComplete(false);
    }

    protected void selectDestinationFile() {
        FileDialog fileDialog = new FileDialog(getShell(), SWT.SAVE);
        File file = new File(exportLocationText.getText());
        fileDialog.setFileName(file.getName());
        if (file.getParentFile() != null && file.getParentFile().isDirectory()) {
            fileDialog.setFilterPath(file.getParent());
        }
        fileDialog.setFilterExtensions(new String[] { "*.zip" });
        fileDialog.open();

        String filterPath = fileDialog.getFilterPath();
        String selectedFile = fileDialog.getFileName();
        if (selectedFile.equalsIgnoreCase("")) {
            return;
        }

        if (!filterPath.endsWith("/")) {
            filterPath += "/";
        }
        String destinationFile = filterPath + selectedFile;
        exportLocationText.setText(destinationFile);
    }

    @Override
    public void populateFromData() {
        LocalizationContext saveContext = data.getSaveContext();
        if (saveContext != null
                && saveContext.getLocalizationLevel() == LocalizationLevel.SITE) {
            localizationCheck.setSelection(true);
        } else {
            localizationCheck.setSelection(false);
        }

        Path path = data.getExportPath();
        if (path != null) {
            exportCheck.setSelection(true);
            exportLocationText.setText(path.toString());
        } else {
            exportCheck.setSelection(false);
        }

        updatePageComplete();
    }

    private void updatePageComplete() {
        setErrorMessage(null);

        if (exportCheck.getSelection()) {
            exportLocationText.setEnabled(true);
            exportLocationButton.setEnabled(true);
            String location = exportLocationText.getText();
            if (location == null || location.isEmpty()) {
                setPageComplete(false);
                setErrorMessage("Please select an export file.");
                return;
            }
            Path path = Paths.get(location);
            if (Files.isDirectory(path)) {
                setPageComplete(false);
                setErrorMessage("Selected export file is a directory, please select a file.");
                return;
            }
            if (Files.exists(path)) {
                setMessage(
                        "Selected File already exists and will be overridden",
                        WizardPage.WARNING);
            }
            data.setExportPath(path);
        } else {
            exportLocationText.setEnabled(false);
            exportLocationButton.setEnabled(false);
            data.setExportPath(null);
        }

        if (localizationCheck.getSelection()) {
            IPathManager pathManager = PathManagerFactory.getPathManager();
            data.setSaveContext(pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE));
        } else {
            data.setSaveContext(null);
        }

        if (!localizationCheck.getSelection() && !exportCheck.getSelection()) {
            setPageComplete(false);
            setErrorMessage("Must select an export option.");
        } else {
            setPageComplete(true);
        }
    }

}
