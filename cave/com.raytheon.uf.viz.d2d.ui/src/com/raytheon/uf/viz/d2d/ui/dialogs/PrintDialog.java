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

import java.awt.Color;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.awt.image.ComponentColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.bind.JAXB;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.ui.DensityPopulator;
import com.raytheon.uf.viz.d2d.ui.MagnificationPopulator;
import com.raytheon.uf.viz.d2d.ui.dialogs.UserPrintSettings.PRINT_ORIENTATION;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Print Dialog
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
 * May 13, 2016  5653     randerso     Fixed print scaling
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class PrintDialog extends CaveSWTDialog {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrintDialog.class);

    private final String SETTINGS_FILENAME = "printSettings";

    private ArrayList<PrinterData> printerDataStore = null;

    private PrinterData printToFileData = null;

    /* Print To */
    private Combo selectedPrinterCombo = null;

    private Button printerRadioButton = null;

    private Button fileRadioButton = null;

    private Text destinationFileText = null;

    private Button browseButton = null;

    /* Print In */
    private Button colorRadioButton = null;

    private Button grayscaleRadioButton = null;

    /* Orientation */
    private Button landscapeRadioButton = null;

    private Button portraitRadioButton = null;

    /* Remaining Settings */
    private Spinner scaleSpinner = null;

    private Button fitToPageBtn;

    private Combo magnificationCombo = null;

    private Spinner copiesSpinner = null;

    private Combo densityCombo = null;

    private Button invertCheckbox = null;

    private Button okButton = null;

    private Button cancelButton = null;

    private MagnificationInformationStorage magnificationInformationStorage = null;

    private DensityInformationStorage densityInformationStorage = null;

    private class MagnificationInformationStorage {
        private double applicationMagnification = 0.0;

        public boolean magnificationAdjusted = false;

        public Map<String, Double> magnificationStore = new HashMap<String, Double>();
    }

    private class DensityInformationStorage {
        private double applicationDensity = 0.0;

        public boolean densityAdjusted = false;

        public Map<String, Double> densityStore = new HashMap<String, Double>();
    }

    private class PrinterSettings {
        public PrinterData selectedPrinter = null;

        /* Print In Color Is The Default. */
        public boolean printInGrayscale = false;

        /* Portrait Is The Default */
        public boolean printInLandscape = false;

        public float scale;

        public boolean fitToPage = false;

        public boolean invert = false;
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
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(2, true);
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        this.saveMagnificationAndDensityByResource();

        this.createPrintToGroup();
        this.createPrintInAndOrientationGroups();
        this.createRemainingPrintingSettingsSection();
        this.createPrintDialogButtons();
        // Now read from the saved user config items, if any.
        readFromConfig();
    }

    private void createPrintToGroup() {
        ArrayList<String> availablePrinters = this.getAvailablePrinters();

        Group group = new Group(this.shell, SWT.SHADOW_ETCHED_IN);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.horizontalSpan = 2;
        group.setLayoutData(gridData);

        GridLayout gridLayout = new GridLayout(3, false);
        group.setLayout(gridLayout);
        group.setText("Print To");

        Button button = new Button(group, SWT.RADIO);
        button.setText("Printer");
        button.setSelection(true);
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (printerRadioButton.getSelection()) {
                    selectedPrinterCombo.setEnabled(true);
                    destinationFileText.setEnabled(false);
                    destinationFileText.setText("");
                    destinationFileText.setToolTipText("~ NO FILE SELECTED ~");
                    browseButton.setEnabled(false);
                }
            }
        });
        this.printerRadioButton = button;

        Combo combo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        combo.setLayoutData(gridData);

        if (availablePrinters != null) {
            Iterator<String> printerIterator = availablePrinters.iterator();
            while (printerIterator.hasNext()) {
                combo.add(printerIterator.next());
            }
            combo.select(0);
        }
        this.selectedPrinterCombo = combo;

        // filler
        new Label(group, SWT.NONE);

        button = new Button(group, SWT.RADIO);
        button.setText("File");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (fileRadioButton.getSelection()) {
                    selectedPrinterCombo.setEnabled(false);
                    destinationFileText.setEnabled(true);
                    browseButton.setEnabled(true);
                }
            }
        });
        this.fileRadioButton = button;

        Text text = new Text(group, SWT.BORDER);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GC gc = new GC(text);
        int textWidth = gc.getFontMetrics().getAverageCharWidth() * 30;
        gc.dispose();
        gridData.minimumWidth = textWidth;
        text.setLayoutData(gridData);
        text.setEnabled(false);
        this.destinationFileText = text;

        button = new Button(group, SWT.PUSH);
        button.setText("Browse ...");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectDestinationFile(destinationFileText.getText());
            }
        });
        button.setEnabled(false);
        this.browseButton = button;
    }

    private void createPrintInAndOrientationGroups() {
        GridData gridData = new GridData();
        gridData.widthHint = 130;
        gridData.horizontalAlignment = GridData.FILL;

        Group group = new Group(this.shell, SWT.SHADOW_ETCHED_IN);
        group.setLayoutData(gridData);
        group.setLayout(new GridLayout(1, true));
        group.setText("Print In");

        Button button = new Button(group, SWT.RADIO);
        button.setText("Color");
        button.setSelection(true);
        colorRadioButton = button;

        button = new Button(group, SWT.RADIO);
        button.setText("Grayscale");
        this.grayscaleRadioButton = button;

        group = new Group(this.shell, SWT.SHADOW_ETCHED_IN);
        group.setLayoutData(gridData);
        group.setLayout(new GridLayout(1, true));
        group.setText("Orientation");

        button = new Button(group, SWT.RADIO);
        button.setText("Portrait");
        button.setSelection(true);
        portraitRadioButton = button;

        button = new Button(group, SWT.RADIO);
        button.setText("Landscape");
        this.landscapeRadioButton = button;
    }

    private void createRemainingPrintingSettingsSection() {
        Composite comp = new Composite(this.shell, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.horizontalSpan = 2;
        comp.setLayoutData(gridData);
        GridLayout gridLayout = new GridLayout(2, false);
        comp.setLayout(gridLayout);

        Composite leftComp = new Composite(comp, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leftComp.setLayoutData(gridData);
        leftComp.setLayout(new GridLayout(2, false));

        Label label = new Label(leftComp, SWT.NONE);
        gridData = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        label.setLayoutData(gridData);
        label.setText("Scale %:");

        Spinner spinner = new Spinner(leftComp, SWT.READ_ONLY | SWT.BORDER);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        spinner.setLayoutData(gridData);
        spinner.setMinimum(0);
        spinner.setMaximum(9999);
        spinner.setSelection(100);
        spinner.setIncrement(1);
        this.scaleSpinner = spinner;

        this.fitToPageBtn = new Button(leftComp, SWT.CHECK);
        gridData = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gridData.horizontalSpan = 2;
        this.fitToPageBtn.setLayoutData(gridData);
        this.fitToPageBtn.setText("Fit to Page");
        this.fitToPageBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                scaleSpinner.setEnabled(!fitToPageBtn.getSelection());
            }

        });

        label = new Label(leftComp, SWT.NONE);
        gridData = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        label.setLayoutData(gridData);
        label.setText("Copies:");

        spinner = new Spinner(leftComp, SWT.READ_ONLY | SWT.BORDER);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        spinner.setLayoutData(gridData);

        spinner.setMinimum(1);
        spinner.setMaximum(9999);
        spinner.setSelection(0);
        spinner.setIncrement(1);
        this.copiesSpinner = spinner;

        Composite rightComp = new Composite(comp, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        rightComp.setLayoutData(gridData);
        rightComp.setLayout(new GridLayout(2, false));

        label = new Label(rightComp, SWT.NONE);
        gridData = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        label.setLayoutData(gridData);
        label.setText("Mag:");

        Combo combo = new Combo(rightComp, SWT.READ_ONLY | SWT.DROP_DOWN);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        combo.setLayoutData(gridData);
        /* Get The Magnification Values. */
        for (int i = 0; i < MagnificationPopulator.getMagnifications().length; i++) {
            combo.add(MagnificationPopulator.getMagnifications()[i]);
        }
        String currentMagnification = this.getCurrentMagnification();
        for (int i = 0; i < MagnificationPopulator.getMagnifications().length; i++) {
            if (currentMagnification.equals(MagnificationPopulator
                    .getMagnifications()[i])) {
                combo.select(i);
                break;
            }
        }
        combo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okButton.setEnabled(false);
                cancelButton.setEnabled(false);
                updateMagnification();
                okButton.setEnabled(true);
                cancelButton.setEnabled(true);
            }
        });
        this.magnificationCombo = combo;

        label = new Label(rightComp, SWT.NONE);
        gridData = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        label.setLayoutData(gridData);
        label.setText("Density:");

        combo = new Combo(rightComp, SWT.READ_ONLY | SWT.DROP_DOWN);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        combo.setLayoutData(gridData);
        for (int i = 0; i < DensityPopulator.getDensityLabels().length; i++) {
            combo.add(DensityPopulator.getDensityLabels()[i]);
        }
        String currentDensity = this.getCurrentDensity();
        for (int i = 0; i < DensityPopulator.getDensityLabels().length; i++) {
            if (currentDensity.equals(DensityPopulator.getDensityLabels()[i])) {
                combo.select(i);
                break;
            }
        }
        combo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okButton.setEnabled(false);
                cancelButton.setEnabled(false);
                updateDensity();
                okButton.setEnabled(true);
                cancelButton.setEnabled(true);
            }
        });
        this.densityCombo = combo;

        rightComp = new Composite(comp, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.horizontalSpan = 2;
        rightComp.setLayoutData(gridData);
        rightComp.setLayout(new GridLayout(2, false));
        label = new Label(rightComp, SWT.NONE);
        label.setText("Paper:");

        combo = new Combo(rightComp, SWT.READ_ONLY | SWT.DROP_DOWN);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        combo.setLayoutData(gridData);
        combo.add("DEFAULT");
        combo.setEnabled(false);
        combo.select(0);
        // TODO: implement paper settings

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        Button button = new Button(comp, SWT.CHECK);
        button.setLayoutData(gridData);
        button.setText("Invert Black/White");
        button.setSelection(true);
        this.invertCheckbox = button;

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        button = new Button(comp, SWT.CHECK);
        button.setLayoutData(gridData);
        button.setText("Manual Feed");
        button.setEnabled(false);
        // TODO: implement manual feed
    }

    private void createPrintDialogButtons() {
        int buttonWidth = getDisplay().getDPI().x * 3 / 2;
        GridData gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        gridData.widthHint = buttonWidth;
        Button button = new Button(this.shell, SWT.PUSH);
        button.setLayoutData(gridData);
        button.setText("OK");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okButton.setEnabled(false);
                cancelButton.setEnabled(false);
                setText("Printing . . .");
                print(getPrintPreferences());
                setText("Print");
                saveToConfig();
                close();
            }
        });
        this.okButton = button;

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        gridData.widthHint = buttonWidth;
        button = new Button(this.shell, SWT.PUSH);
        button.setLayoutData(gridData);
        button.setText("Cancel");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okButton.setEnabled(false);
                cancelButton.setEnabled(false);
                restoreMagnificationAndDensity();
                close();
            }
        });
        this.cancelButton = button;
    }

    private void selectDestinationFile(String fileName) {

        FileDialog fileDialog = new FileDialog(this.shell, SWT.SAVE);

        if (fileName != null) {
            int n = fileName.lastIndexOf(File.separator);
            String path = null;
            if (n > 0) {
                path = fileName.substring(0, n);
                fileName = fileName.substring(n + 1);
            }
            fileDialog.setFileName(fileName);
            fileDialog.setFilterPath(path);
        }

        fileDialog.open();

        String filterPath = fileDialog.getFilterPath();
        String selectedFile = fileDialog.getFileName();
        /*
         * Ensure that the user has entered a name for the file.
         */
        if (selectedFile.equalsIgnoreCase("")) {
            return;
        }

        if (!filterPath.endsWith("/")) {
            filterPath += "/";
        }
        String destinationFile = filterPath + selectedFile;
        this.destinationFileText.setText(destinationFile);
        this.destinationFileText.setToolTipText(destinationFile);
    }

    private void updateMagnification() {
        String selectedMagnificationString = this.magnificationCombo
                .getItem(this.magnificationCombo.getSelectionIndex());
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
        Double selectedDensity = DensityPopulator.getDensityValues()[selectedDensityIndex];

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
                this.magnificationInformationStorage.magnificationStore.put(rp
                        .getResource().getName(), currentMagnification);
            }
        }
    }

    private void saveDensityByResource(ResourceList resources) {
        for (ResourcePair rp : resources) {
            if (rp.getResource().hasCapability(DensityCapability.class)) {
                double currentDensity = rp.getResource()
                        .getCapability(DensityCapability.class).getDensity();
                this.densityInformationStorage.densityStore.put(rp
                        .getResource().getName(), currentDensity);
            }
        }
    }

    private void restoreMagnificationAndDensity() {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        IDisplayPane displayPane = editor.getActiveDisplayPane();
        ResourceList resources = displayPane.getDescriptor().getResourceList();

        /* Restore By Resource Assuming That One Or The Other Has Been Adjusted. */
        if (this.magnificationInformationStorage.magnificationAdjusted
                || this.densityInformationStorage.densityAdjusted) {

            /* Revert The Application-Wide Density & Magnification. */
            if (this.magnificationInformationStorage.magnificationAdjusted) {
                ((ID2DRenderableDisplay) displayPane.getRenderableDisplay())
                        .setMagnification(this.magnificationInformationStorage.applicationMagnification);
            }

            if (this.densityInformationStorage.densityAdjusted) {
                ((ID2DRenderableDisplay) displayPane.getRenderableDisplay())
                        .setDensity(this.densityInformationStorage.applicationDensity);
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

    private PrinterSettings getPrintPreferences() {
        PrinterSettings printerSettings = new PrinterSettings();

        if (this.printerRadioButton.getSelection()) {
            printerSettings.selectedPrinter = this.printerDataStore
                    .get(this.selectedPrinterCombo.getSelectionIndex());
        } else if (this.fileRadioButton.getSelection()) {
            printerSettings.selectedPrinter = this.printToFileData;
            printerSettings.selectedPrinter.printToFile = true;
            printerSettings.selectedPrinter.fileName = this.destinationFileText
                    .getText();
        }
        printerSettings.selectedPrinter.copyCount = this.copiesSpinner
                .getSelection();

        if (this.grayscaleRadioButton.getSelection()) {
            printerSettings.printInGrayscale = true;
        }
        if (this.landscapeRadioButton.getSelection()) {
            // TODO: restore this line if we ever get Landscape printing working
            // on the LX workstations
            // printerSettings.selectedPrinter.orientation =
            // PrinterData.LANDSCAPE;
            printerSettings.printInLandscape = true;
        }

        printerSettings.scale = ((float) this.scaleSpinner.getSelection() / 100);
        printerSettings.fitToPage = this.fitToPageBtn.getSelection();

        if (this.invertCheckbox.getSelection()) {
            printerSettings.invert = true;
        }

        return printerSettings;
    }

    private void print(PrinterSettings printerSettings) {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        BufferedImage bi = editor.screenshot();

        // TODO: remove this block if we ever get Landscape printing working
        // on the LX workstations
        if (printerSettings.printInLandscape) {
            AffineTransform transform = AffineTransform
                    .getQuadrantRotateInstance(1);
            transform.concatenate(AffineTransform.getTranslateInstance(0.0,
                    -bi.getHeight()));
            AffineTransformOp transformOp = new AffineTransformOp(transform,
                    AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
            bi = transformOp.filter(
                    bi,
                    new BufferedImage(bi.getHeight(), bi.getWidth(), bi
                            .getType()));
        }

        if (printerSettings.invert) {
            // Only invert gray pixels, not colored pixels, awt doesn't not have
            // a good filter for this.
            for (int x = 0; x < bi.getWidth(); x += 1) {
                for (int y = 0; y < bi.getHeight(); y += 1) {
                    Color color = new Color(bi.getRGB(x, y));
                    if (color.getRed() == color.getBlue()
                            && color.getBlue() == color.getGreen()) {
                        color = new Color(255 - color.getRed(),
                                255 - color.getGreen(), 255 - color.getBlue());
                        bi.setRGB(x, y, color.getRGB());
                    }

                }
            }
        }
        ImageData imageData = convertToSWT(bi);
        Image image = null;
        Image colorImage = null;

        Printer printer = new Printer(printerSettings.selectedPrinter);
        colorImage = new Image(printer, imageData);
        if (!printerSettings.printInGrayscale) {
            image = colorImage;
        } else {
            Image grayScaleImage = new Image(printer, colorImage,
                    SWT.IMAGE_GRAY);
            image = grayScaleImage;
        }

        Rectangle printArea = printer.getClientArea();
        Display display = editor.getActiveDisplayPane().getDisplay();
        Point screenDPI = display.getDPI();
        Point printerDPI = printer.getDPI();

        float scale = printerSettings.scale;

        if (printerSettings.fitToPage) {
            // get image size in inches
            float imageWidth = bi.getWidth() / (float) screenDPI.x;
            float imageHeight = bi.getHeight() / (float) screenDPI.y;

            // get print area size in inches
            float printerWidth = printArea.width / (float) printerDPI.x;
            float printerHeight = printArea.height / (float) printerDPI.y;

            // compute scale to fit the page
            float hScale, vScale;
            hScale = printerWidth / imageWidth;
            vScale = printerHeight / imageHeight;
            scale = Math.min(hScale, vScale);
        }

        // compute output image size in printer pixels
        int scaledImageWidth = Math.round(scale * bi.getWidth() * printerDPI.x
                / screenDPI.x);
        int scaledImageHeight = Math.round(scale * bi.getHeight()
                * printerDPI.y / screenDPI.y);

        Point initialOffset = new Point(0, 0);
        int xPages = (int) Math.ceil((double) scaledImageWidth
                / printArea.width);
        int yPages = (int) Math.ceil((double) scaledImageHeight
                / printArea.height);

        // Compute offset to center image in page(s)
        initialOffset.x = ((xPages * printArea.width) - scaledImageWidth) / 2;
        initialOffset.y = ((yPages * printArea.height) - scaledImageHeight) / 2;

        Point offset = new Point(initialOffset.x, initialOffset.y);
        Point remaining = new Point(scaledImageWidth, scaledImageHeight);
        if (printer.startJob("CAVE")) {
            while (remaining.x > 0 && remaining.y > 0) {
                if (printer.startPage()) {
                    GC gc = new GC(printer);

                    Transform transform = new Transform(gc.getDevice());
                    transform.translate(offset.x, offset.y);
                    transform.scale(scale * printerDPI.x / screenDPI.x, scale
                            * printerDPI.y / screenDPI.y);

                    gc.setTransform(transform);

                    gc.drawImage(image, 0, 0);

                    transform.dispose();
                    gc.dispose();
                    printer.endPage();
                }

                remaining.x -= printArea.width;
                offset.x -= printArea.width;
                if (remaining.x <= 0) {
                    remaining.x = scaledImageWidth;
                    offset.x = initialOffset.x;
                    remaining.y -= printArea.height;
                    offset.y -= printArea.height;
                }
            }
            printer.endJob();
        }

        image.dispose();
        printer.dispose();

        this.restoreMagnificationAndDensity();
        // this.close();
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

    private ArrayList<String> getAvailablePrinters() {
        PrinterData[] printers = Printer.getPrinterList();
        if (printers == null || printers.length <= 0) {
            return null;
        }

        ArrayList<String> availablePrinters = new ArrayList<String>();
        this.printerDataStore = new ArrayList<PrinterData>();
        for (int i = 0; i < printers.length; i++) {
            /* Do Not Include "Print to File" In The Printer List. */
            if (printers[i].name.equalsIgnoreCase("print to file")) {
                this.printToFileData = printers[i];
                continue;
            }
            this.printerDataStore.add(printers[i]);
            availablePrinters.add(printers[i].name);
        }

        return availablePrinters;
    }

    static ImageData convertToSWT(BufferedImage bufferedImage) {
        if (bufferedImage.getColorModel() instanceof ComponentColorModel) {
            ComponentColorModel colorModel = (ComponentColorModel) bufferedImage
                    .getColorModel();

            PaletteData palette = new PaletteData(0x0000ff, 0x00ff00, 0xff0000);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[3];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    int pixel = palette.getPixel(new RGB(pixelArray[0],
                            pixelArray[1], pixelArray[2]));
                    data.setPixel(x, y, pixel);
                }
            }
            return data;
        } else if (bufferedImage.getColorModel() instanceof IndexColorModel) {
            IndexColorModel colorModel = (IndexColorModel) bufferedImage
                    .getColorModel();
            int size = colorModel.getMapSize();
            byte[] reds = new byte[size];
            byte[] greens = new byte[size];
            byte[] blues = new byte[size];
            colorModel.getReds(reds);
            colorModel.getGreens(greens);
            colorModel.getBlues(blues);
            RGB[] rgbs = new RGB[size];
            for (int i = 0; i < rgbs.length; i++) {
                rgbs[i] = new RGB(reds[i] & 0xFF, greens[i] & 0xFF,
                        blues[i] & 0xFF);
            }
            PaletteData palette = new PaletteData(rgbs);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            data.transparentPixel = colorModel.getTransparentPixel();
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[1];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    data.setPixel(x, y, pixelArray[0]);
                }
            }
            return data;
        }
        return null;
    }

    /**
     * Save the user print settings.
     */
    private void saveToConfig() {

        UserPrintSettings settings = new UserPrintSettings();

        settings.setInvertBlackWhite(invertCheckbox.getSelection());
        settings.setPrintGrayScale(grayscaleRadioButton.getSelection());
        settings.setOrientation(PRINT_ORIENTATION
                .getPrintOrientation(landscapeRadioButton.getSelection()));

        settings.setCopies(copiesSpinner.getSelection());
        settings.setScale(scaleSpinner.getSelection());
        settings.setFitToPage(fitToPageBtn.getSelection());

        settings.setDensity(densityCombo.getSelectionIndex());
        settings.setMag(magnificationCombo.getSelectionIndex());

        if (printerRadioButton.getSelection()) {
            int idx = selectedPrinterCombo.getSelectionIndex();
            settings.setPrinterUsed(selectedPrinterCombo.getItem(idx));
            settings.setUsePrinterFile(false);
        } else {
            settings.setPrinterFile(destinationFileText.getText());
            settings.setUsePrinterFile(true);
        }

        LocalizationContext ctx = initUserLocalization();

        // Get a list of localization files!
        LocalizationFile lf = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, SETTINGS_FILENAME);

        try (SaveableOutputStream strm = lf.openOutputStream()) {
            JAXB.marshal(settings, strm);
            // Ensure that the file is saved on the server!
            strm.save();
        } catch (Exception e) {
            statusHandler.error("Could not save user print settings", e);
        }
    }

    /**
     * Read user print settings if they exist.
     */
    private void readFromConfig() {

        LocalizationContext ctx = initUserLocalization();

        // Get a list of localization files!
        LocalizationFile lf = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, SETTINGS_FILENAME);
        // If its not there, no previous settings have been saved. Just exit.
        if (lf.exists()) {
            UserPrintSettings settings = null;
            try (InputStream strm = lf.openInputStream()) {

                settings = JAXB.unmarshal(strm, UserPrintSettings.class);

            } catch (Exception e) {
                statusHandler.error(
                        "Could not read user print settings-using defaults", e);
            }
            if (settings != null) {
                invertCheckbox.setSelection(settings.getInvertBlackWhite());
                grayscaleRadioButton.setSelection(settings.isPrintGrayScale());
                colorRadioButton.setSelection(!grayscaleRadioButton
                        .getSelection());

                landscapeRadioButton.setSelection(settings.getOrientation()
                        .isPrintLandscape());
                portraitRadioButton.setSelection(!landscapeRadioButton
                        .getSelection());

                Integer n = settings.getCopies();
                if (n != null) {
                    if (n >= copiesSpinner.getMinimum()
                            && n <= copiesSpinner.getMaximum()) {
                        copiesSpinner.setSelection(n);
                    }
                }

                n = settings.getScale();
                if (n != null) {
                    if (n >= scaleSpinner.getMinimum()
                            && n <= scaleSpinner.getMaximum()) {
                        scaleSpinner.setSelection(settings.getScale());
                    }
                }

                fitToPageBtn.setSelection(settings.isFitToPage());
                scaleSpinner.setEnabled(!fitToPageBtn.getSelection());

                n = settings.getDensity();
                if (n != null) {
                    if ((n >= 0) && (n < densityCombo.getItemCount())) {
                        densityCombo.select(n);
                    }
                }
                n = settings.getMag();
                if (n != null) {
                    if ((n >= 0) && (n < magnificationCombo.getItemCount())) {
                        magnificationCombo.select(n);
                    }
                }

                String s = settings.getPrinterFile();
                if (s != null) {
                    destinationFileText.setText(s);
                    destinationFileText.setToolTipText(s);
                    destinationFileText.setEnabled(true);
                    browseButton.setEnabled(true);
                }

                s = settings.getPrinterUsed();
                if (s != null) {
                    int idx = -1;
                    for (int i = 0; i < selectedPrinterCombo.getItemCount(); i++) {
                        if (s.equals(selectedPrinterCombo.getItem(i))) {
                            idx = i;
                            break;
                        }
                    }
                    if (idx > -1) {
                        selectedPrinterCombo.select(idx);
                    }
                }
                printerRadioButton.setSelection(!settings.isUsePrinterFile());
                fileRadioButton.setSelection(settings.isUsePrinterFile());
            }
        }
    }

    /**
     * initialize the localization for user with the save/load functions
     * 
     * @return the initialized localization
     */
    private static LocalizationContext initUserLocalization() {
        return initLocalization(LocalizationLevel.USER);
    }

    /**
     * Initialize a LocalizationContext for the given LocalizationLevel.
     * 
     * @param level
     * @return the initialized localization
     */
    private static LocalizationContext initLocalization(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm.getContext(
                LocalizationType.COMMON_STATIC, level);
        return localization;
    }

}