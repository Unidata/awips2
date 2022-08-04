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
package com.raytheon.uf.viz.d2d.ui.dialogs;

import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.printing.PrintJob;
import com.raytheon.uf.viz.core.printing.PrintJob.PrintingException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.ui.DensityPopulator;
import com.raytheon.uf.viz.d2d.ui.MagnificationPopulator;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Class representing a print dialog with various print settings.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Feb 15, 2011           bkowal       Initial creation
 * Aug 15, 2012  1053     jkorman      Added capability to save/restore user
 *                                     print settings.
 * Oct 12, 2012  1229     rferrel      Made dialog non-blocking.
 * Dec 09, 2014  11982    D. Friedman  Fix print-to-file
 * Jan 11, 2016  5242     kbisanz      Replaced calls to deprecated
 *                                     LocalizationFile methods
 * May 13, 2016  5653     randerso     Fixed print scaling Handle empty printer
 *                                     list Code cleanup
 * Mar 23, 2017  6117     bsteffen     Workaround crash when printing images.
 * Feb 20, 2020  8039     randerso     Fix print scaling with latest
 *                                     Eclipse/SWT. Required using the SWT
 *                                     PrintDialog to select printer and specify
 *                                     page/job setup parameters to get properly
 *                                     populated PrinterData object.
 * Apr 13, 2020  8120     randerso     Extracted image printing to PrintJob
 *
 * </pre>
 *
 * @author bkowal
 */
public class PrintDialog extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrintDialog.class);

    private static final String SETTINGS_FILENAME = "printSettings.xml";

    private UserPrintSettings printSettings;

    private Button fitToPageBtn;

    private Combo magnificationCombo;

    private Combo densityCombo;

    private Button invertCheckbox;

    private Button printButton;

    private Button cancelButton;

    private MagnificationInformationStorage magnificationInformationStorage;

    private DensityInformationStorage densityInformationStorage;

    private class MagnificationInformationStorage {
        private double applicationMagnification = 0.0;

        public boolean magnificationAdjusted = false;

        public Map<String, Double> magnificationStore = new HashMap<>();
    }

    private class DensityInformationStorage {
        private double applicationDensity = 0.0;

        public boolean densityAdjusted = false;

        public Map<String, Double> densityStore = new HashMap<>();
    }

    /**
     * Constructor
     *
     * @param parent
     *            parent shell
     */
    public PrintDialog(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.setText("Print");

        printSettings = readFromConfig();
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        /*
         * save current mag/density settings to be restored when the dialog is
         * closed
         */
        this.saveMagnificationAndDensityByResource();
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                restoreMagnificationAndDensity();
            }
        });

        this.createImageOptionsSection();
        this.createPrintDialogButtons();
    }

    private void createImageOptionsSection() {
        Group group = new Group(this.shell, SWT.SHADOW_ETCHED_IN);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group.setLayoutData(gridData);
        GridLayout gridLayout = new GridLayout(1, false);
        group.setLayout(gridLayout);
        group.setText("Image Options");

        Composite magDensityComp = new Composite(group, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        magDensityComp.setLayoutData(gridData);
        magDensityComp.setLayout(new GridLayout(2, false));

        Label label = new Label(magDensityComp, SWT.NONE);
        gridData = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        label.setLayoutData(gridData);
        label.setText("Mag:");

        magnificationCombo = new Combo(magDensityComp,
                SWT.READ_ONLY | SWT.DROP_DOWN);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        magnificationCombo.setLayoutData(gridData);
        magnificationCombo.setItems(MagnificationPopulator.getMagnifications());
        magnificationCombo
                .select(magnificationCombo.indexOf(getCurrentMagnification()));

        magnificationCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printButton.setEnabled(false);
                cancelButton.setEnabled(false);
                updateMagnification();
                printButton.setEnabled(true);
                cancelButton.setEnabled(true);
            }
        });
        Integer n = printSettings.getMag();
        if (n != null) {
            magnificationCombo.select(n);
            updateMagnification();
        }

        label = new Label(magDensityComp, SWT.NONE);
        gridData = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        label.setLayoutData(gridData);
        label.setText("Density:");

        densityCombo = new Combo(magDensityComp, SWT.READ_ONLY | SWT.DROP_DOWN);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        densityCombo.setLayoutData(gridData);
        densityCombo.setItems(DensityPopulator.getDensityLabels());
        densityCombo.select(densityCombo.indexOf(getCurrentDensity()));
        densityCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printButton.setEnabled(false);
                cancelButton.setEnabled(false);
                updateDensity();
                printButton.setEnabled(true);
                cancelButton.setEnabled(true);
            }
        });
        n = printSettings.getDensity();
        if (n != null) {
            densityCombo.select(n);
            updateDensity();
        }

        fitToPageBtn = new Button(group, SWT.CHECK);
        gridData = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        fitToPageBtn.setLayoutData(gridData);
        fitToPageBtn.setText("Fit to Page");
        fitToPageBtn.setSelection(printSettings.isFitToPage());
        this.fitToPageBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printSettings.setFitToPage(fitToPageBtn.getSelection());
            }
        });

        invertCheckbox = new Button(group, SWT.CHECK);
        gridData = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        invertCheckbox.setLayoutData(gridData);
        invertCheckbox.setText("Invert Black/White");
        invertCheckbox.setSelection(printSettings.isInvertBlackWhite());
        invertCheckbox.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printSettings
                        .setInvertBlackWhite(invertCheckbox.getSelection());
            }
        });
    }

    private void createPrintDialogButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gridData);
        buttonComp.setLayout(new GridLayout(2, true));

        printButton = new Button(buttonComp, SWT.PUSH);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        printButton.setLayoutData(gridData);
        printButton.setText("Print...");
        printButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                org.eclipse.swt.printing.PrintDialog printDialog = new org.eclipse.swt.printing.PrintDialog(
                        getShell());
                printDialog.setPrinterData(printSettings.getPrinterData());
                PrinterData printerData = printDialog.open();
                if (printerData != null) {
                    printSettings.setPrinterData(printerData);
                    printButton.setEnabled(false);
                    cancelButton.setEnabled(false);
                    setText("Printing . . .");
                    print(printSettings);
                    setText("Print");
                    saveToConfig();
                    close();
                }
            }
        });

        cancelButton = new Button(buttonComp, SWT.PUSH);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        cancelButton.setLayoutData(gridData);
        cancelButton.setText("Cancel");
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printButton.setEnabled(false);
                cancelButton.setEnabled(false);
                close();
            }
        });
    }

    private void updateMagnification() {
        int selectedMagIndex = magnificationCombo.getSelectionIndex();
        printSettings.setMag(selectedMagIndex);

        String selectedMagnificationString = this.magnificationCombo
                .getItem(selectedMagIndex);
        Double selectedMagnification = Double
                .parseDouble(selectedMagnificationString);

        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        IDisplayPane displayPane = editor.getActiveDisplayPane();
        ((ID2DRenderableDisplay) displayPane.getRenderableDisplay())
                .setMagnification(selectedMagnification);
        this.magnificationInformationStorage.magnificationAdjusted = true;
    }

    private void updateDensity() {
        int selectedDensityIndex = this.densityCombo.getSelectionIndex();
        printSettings.setDensity(selectedDensityIndex);

        Double selectedDensity = DensityPopulator
                .getDensityValues()[selectedDensityIndex];

        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        IDisplayPane displayPane = editor.getActiveDisplayPane();
        ((ID2DRenderableDisplay) displayPane.getRenderableDisplay())
                .setDensity(selectedDensity);
        this.densityInformationStorage.densityAdjusted = true;
    }

    private void saveMagnificationAndDensityByResource() {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        IDisplayPane displayPane = editor.getActiveDisplayPane();
        ResourceList resources = displayPane.getDescriptor().getResourceList();

        this.magnificationInformationStorage = new MagnificationInformationStorage();
        this.saveMagnificationByResource(resources);
        this.densityInformationStorage = new DensityInformationStorage();
        this.saveDensityByResource(resources);
    }

    private void saveMagnificationByResource(ResourceList resources) {
        for (ResourcePair rp : resources) {
            if (rp.getResource().hasCapability(MagnificationCapability.class)) {
                double currentMagnification = rp.getResource()
                        .getCapability(MagnificationCapability.class)
                        .getMagnification();
                this.magnificationInformationStorage.magnificationStore
                        .put(rp.getResource().getName(), currentMagnification);
            }
        }
    }

    private void saveDensityByResource(ResourceList resources) {
        for (ResourcePair rp : resources) {
            if (rp.getResource().hasCapability(DensityCapability.class)) {
                double currentDensity = rp.getResource()
                        .getCapability(DensityCapability.class).getDensity();
                this.densityInformationStorage.densityStore
                        .put(rp.getResource().getName(), currentDensity);
            }
        }
    }

    private void restoreMagnificationAndDensity() {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        IDisplayPane displayPane = editor.getActiveDisplayPane();
        ResourceList resources = displayPane.getDescriptor().getResourceList();

        /*
         * Restore By Resource Assuming That One Or The Other Has Been Adjusted.
         */
        if (this.magnificationInformationStorage.magnificationAdjusted
                || this.densityInformationStorage.densityAdjusted) {

            /* Revert The Application-Wide Density & Magnification. */
            if (this.magnificationInformationStorage.magnificationAdjusted) {
                ((ID2DRenderableDisplay) displayPane.getRenderableDisplay())
                        .setMagnification(
                                this.magnificationInformationStorage.applicationMagnification);
            }

            if (this.densityInformationStorage.densityAdjusted) {
                ((ID2DRenderableDisplay) displayPane.getRenderableDisplay())
                        .setDensity(
                                this.densityInformationStorage.applicationDensity);
            }

            for (ResourcePair rp : resources) {
                if (this.magnificationInformationStorage.magnificationAdjusted) {
                    if (this.magnificationInformationStorage.magnificationStore
                            .containsKey(rp.getResource().getName())) {

                        Double magnification = this.magnificationInformationStorage.magnificationStore
                                .get(rp.getResource().getName());
                        rp.getResource()
                                .getCapability(MagnificationCapability.class)
                                .setMagnification(magnification);
                    }
                }

                if (this.densityInformationStorage.densityAdjusted) {
                    if (this.densityInformationStorage.densityStore
                            .containsKey(rp.getResource().getName())) {

                        Double density = this.densityInformationStorage.densityStore
                                .get(rp.getResource().getName());
                        rp.getResource().getCapability(DensityCapability.class)
                                .setDensity(density);
                    }
                }
            }
        }
    }

    private void print(UserPrintSettings printSettings) {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        BufferedImage bi = editor.screenshot();
        Display display = editor.getActiveDisplayPane().getDisplay();

        try (PrintJob printJob = new PrintJob(printSettings.getPrinterData())) {
            printJob.printImage(display, bi, printSettings.isInvertBlackWhite(),
                    printSettings.isFitToPage());
        } catch (PrintingException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
    }

    private String getCurrentMagnification() {
        MagnificationCapability magnificationCapability = new MagnificationCapability();
        String currentMagnificationString = magnificationCapability
                .getMagnificationString();
        this.magnificationInformationStorage.applicationMagnification = magnificationCapability
                .getMagnification();

        return currentMagnificationString;
    }

    private String getCurrentDensity() {
        DensityCapability densityCapability = new DensityCapability();
        Double currentDensity = densityCapability.getDensity();
        this.densityInformationStorage.applicationDensity = currentDensity;

        return currentDensity.toString();
    }

    /**
     * Save the user print settings.
     */
    private void saveToConfig() {
        printSettings.setInvertBlackWhite(invertCheckbox.getSelection());
        printSettings.setFitToPage(fitToPageBtn.getSelection());
        printSettings.setDensity(densityCombo.getSelectionIndex());
        printSettings.setMag(magnificationCombo.getSelectionIndex());

        try {
            printSettings.store(SETTINGS_FILENAME);
        } catch (Exception e) {
            statusHandler.error("Could not save user print settings", e);
        }
    }

    /**
     * Read user print settings if they exist.
     */
    private UserPrintSettings readFromConfig() {

        UserPrintSettings settings = null;
        try {
            settings = UserPrintSettings.load(SETTINGS_FILENAME);

        } catch (Exception e) {
            statusHandler.error(
                    "Could not read user print settings-using defaults", e);
            settings = new UserPrintSettings();
        }
        return settings;
    }

}
