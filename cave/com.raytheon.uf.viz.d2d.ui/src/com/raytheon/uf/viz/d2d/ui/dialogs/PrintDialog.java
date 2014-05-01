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
import java.io.IOException;
import java.io.OutputStream;
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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2011            bkowal     Initial creation
 * ======================================
 * AWIPS2 DR Work
 * 08/15/2012         1053 jkorman     Added capability to save/restore user
 * print settings.
 * 10/12/2012         1229 rferrel    Made dialog non-blocking.
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

    private Combo magnificationCombo = null;

    private Spinner copiesSpinner = null;

    private Combo densityCombo = null;

    private Button invertCheckbox = null;

    private Button okButton = null;

    private Button cancelButton = null;

    private MagnificationInformationStorage magnificationInformationStorage = null;

    private DensityInformationStorage densityInformationStorage = null;

    private float SCALE_CONST = .89f;

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

        public boolean invert = false;
    }

    public PrintDialog(Shell shell) {
        super(shell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.setText("Print");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(2, true);
        mainLayout.marginHeight = 20;
        mainLayout.verticalSpacing = 2;
        mainLayout.marginWidth = 15;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        this.initializeComponents();
    }

    private void initializeComponents() {
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

        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.heightHint = 70;
        gridData.horizontalSpan = 2;

        Group group = new Group(this.shell, SWT.SHADOW_ETCHED_IN);
        group.setLayoutData(gridData);
        group.setLayout(new GridLayout(3, false));
        group.setText("Print To");

        Button button = new Button(group, SWT.RADIO);
        button.setText("Printer");
        button.setSelection(true);
        button.addSelectionListener(new SelectionAdapter() {
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

        gridData = new GridData();
        gridData.widthHint = 195;
        gridData.horizontalSpan = 2;

        Combo combo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
        combo.setLayoutData(gridData);
        if (availablePrinters != null) {
            Iterator<String> printerIterator = availablePrinters.iterator();
            while (printerIterator.hasNext()) {
                combo.add(printerIterator.next());
            }
            combo.select(0);
        }
        this.selectedPrinterCombo = combo;

        button = new Button(group, SWT.RADIO);
        button.setText("File");
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (fileRadioButton.getSelection()) {
                    selectedPrinterCombo.setEnabled(false);
                    destinationFileText.setEnabled(true);
                    browseButton.setEnabled(true);
                }
            }
        });
        this.fileRadioButton = button;

        gridData = new GridData();
        gridData.widthHint = 195;
        Text text = new Text(group, SWT.BORDER);
        text.setLayoutData(gridData);
        text.setEnabled(false);
        this.destinationFileText = text;

        button = new Button(group, SWT.PUSH);
        button.setText("Browse ...");
        button.addSelectionListener(new SelectionAdapter() {
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
        Composite composite = new Composite(this.shell, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));

        Label label = new Label(composite, SWT.NONE);
        label.setText("% Scale: ");

        GridData gridData = new GridData();
        gridData.widthHint = 55;
        Spinner spinner = new Spinner(composite, SWT.READ_ONLY);
        spinner.setLayoutData(gridData);
        spinner.setMinimum(0);
        spinner.setMaximum(9999);
        spinner.setSelection(100);
        spinner.setIncrement(1);
        this.scaleSpinner = spinner;

        composite = new Composite(this.shell, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));

        label = new Label(composite, SWT.NONE);
        label.setText("\u0020\u0020\u0020\u0020\u0020Mag: ");

        gridData = new GridData();
        gridData.widthHint = 110;
        Combo combo = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
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
            public void widgetSelected(SelectionEvent event) {
                okButton.setEnabled(false);
                cancelButton.setEnabled(false);
                updateMagnification();
                okButton.setEnabled(true);
                cancelButton.setEnabled(true);
            }
        });
        this.magnificationCombo = combo;

        composite = new Composite(this.shell, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));

        label = new Label(composite, SWT.NONE);
        label.setText("\u0020\u0020Copies: ");

        gridData = new GridData();
        gridData.widthHint = 55;
        spinner = new Spinner(composite, SWT.READ_ONLY);
        spinner.setLayoutData(gridData);
        spinner.setMinimum(1);
        spinner.setMaximum(9999);
        spinner.setSelection(0);
        spinner.setIncrement(1);
        this.copiesSpinner = spinner;

        composite = new Composite(this.shell, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));

        label = new Label(composite, SWT.NONE);
        label.setText("Density: ");

        gridData = new GridData();
        gridData.widthHint = 110;
        combo = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
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
            public void widgetSelected(SelectionEvent event) {
                okButton.setEnabled(false);
                cancelButton.setEnabled(false);
                updateDensity();
                okButton.setEnabled(true);
                cancelButton.setEnabled(true);
            }
        });
        this.densityCombo = combo;

        gridData = new GridData();
        gridData.horizontalSpan = 2;
        composite = new Composite(this.shell, SWT.NONE);
        composite.setLayoutData(gridData);
        composite.setLayout(new GridLayout(2, false));
        label = new Label(composite, SWT.NONE);
        label.setText("\u0020\u0020\u0020\u0020\u0020Paper: ");

        gridData = new GridData();
        gridData.widthHint = 295;
        combo = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
        combo.setLayoutData(gridData);
        combo.add("DEFAULT");
        combo.setEnabled(false);
        combo.select(0);

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        Button button = new Button(this.shell, SWT.CHECK);
        button.setLayoutData(gridData);
        button.setText("Invert Black/White");
        button.setSelection(true);
        this.invertCheckbox = button;

        gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        button = new Button(this.shell, SWT.CHECK);
        button.setLayoutData(gridData);
        button.setText("Manual Feed");
        button.setEnabled(false);
    }

    private void createPrintDialogButtons() {
        GridData gridData = new GridData();
        gridData.horizontalAlignment = SWT.CENTER;
        gridData.widthHint = 125;
        Button button = new Button(this.shell, SWT.PUSH);
        button.setLayoutData(gridData);
        button.setText("OK");
        button.addSelectionListener(new SelectionAdapter() {
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
        gridData.widthHint = 125;
        button = new Button(this.shell, SWT.PUSH);
        button.setLayoutData(gridData);
        button.setText("Cancel");
        button.addSelectionListener(new SelectionAdapter() {
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
            printerSettings.selectedPrinter.fileName = "file://"
                    + this.destinationFileText.getText();
        }
        printerSettings.selectedPrinter.copyCount = this.copiesSpinner
                .getSelection();

        if (this.grayscaleRadioButton.getSelection()) {
            printerSettings.printInGrayscale = true;
        }
        if (this.landscapeRadioButton.getSelection()) {
            printerSettings.printInLandscape = true;
        }

        printerSettings.scale = ((float) this.scaleSpinner.getSelection() / 100);

        if (this.invertCheckbox.getSelection()) {
            printerSettings.invert = true;
        }

        return printerSettings;
    }

    private void print(PrinterSettings printerSettings) {
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();

        BufferedImage bi = editor.screenshot();
        Display display = editor.getActiveDisplayPane().getDisplay();
        Printer printer = new Printer(printerSettings.selectedPrinter);
        Point screenDPI = display.getDPI();
        Point printerDPI = printer.getDPI();

        // Determine the bounds of the entire area of the printer
        Rectangle printArea = printer.getClientArea();
        float imageWidth = bi.getWidth() / (float) screenDPI.x;
        float imageHeight = bi.getHeight() / (float) screenDPI.y;
        float imageAspect = imageWidth / imageHeight;

        float printerWidth = printArea.width / (float) printerDPI.x;
        float printerHeight = printArea.height / (float) printerDPI.y;
        float printerAspect = printerWidth / printerHeight;

        // rotate image if necessary for best fit
        // NOTE: rotating the image since the SWT Transform appears to have an
        // error for exact 90 degree rotations

        boolean rotated = false;
        if (printerSettings.printInLandscape) {
            if ((imageAspect - 1) * (printerAspect - 1) < 0) {
                AffineTransform transform = AffineTransform
                        .getQuadrantRotateInstance(1);
                transform.concatenate(AffineTransform.getTranslateInstance(0.0,
                        -bi.getHeight()));
                AffineTransformOp transformOp = new AffineTransformOp(
                        transform, AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
                bi = transformOp.filter(bi, new BufferedImage(bi.getHeight(),
                        bi.getWidth(), bi.getType()));
                rotated = true;
            }
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

        colorImage = new Image(printer, imageData);
        if (!printerSettings.printInGrayscale) {
            image = colorImage;
        } else {
            Image grayScaleImage = new Image(printer, colorImage,
                    SWT.IMAGE_GRAY);
            image = grayScaleImage;
        }

        // scale to adjust for difference in pixel aspect
        float aspectScale = ((float) printerDPI.y / printerDPI.x)
                / ((float) screenDPI.y / screenDPI.x);

        // now scale to fill the page
        Rectangle imageBounds = image.getBounds();
        float hScale = ((float) printArea.width / imageBounds.width)
                * printerSettings.scale;
        float vScale = (printArea.height / (imageBounds.height * aspectScale))
                * printerSettings.scale;
        float scaleX = Math.min(hScale, vScale);
        // float scaleY = (scaleX * aspectScale) * printerSettings.scale;

        // if rotated shift image to right edge of page
        Rectangle trim = printer.computeTrim(0, 0, 0, 0);
        Point offset = new Point(-trim.x, -trim.y);
        if (rotated) {
            offset.x += printArea.width - (imageBounds.width) * scaleX;
        }

        if (printer.startJob("CAVE")) {
            if (printer.startPage()) {
                GC gc = new GC(printer);
                Transform transform = new Transform(gc.getDevice());
                transform.translate(offset.x, offset.y);

                transform.scale(SCALE_CONST * printerSettings.scale
                        * printerDPI.x / (float) screenDPI.x, SCALE_CONST
                        * printerSettings.scale * printerDPI.y
                        / (float) screenDPI.y);
                gc.setTransform(transform);

                gc.drawImage(image, 0, 0);

                transform.dispose();
                gc.dispose();
                printer.endPage();
                printer.endJob();
            }
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
        LocalizationFile f = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, SETTINGS_FILENAME);
        OutputStream strm = null;
        try {
            strm = f.openOutputStream();
            JAXB.marshal(settings, strm);
            // Ensure that the file is saved on the server!
            f.save();
        } catch (Exception e) {
            statusHandler.error("Could not save user print settings", e);
        } finally {
            if (f != null) {
                try {
                    strm.close();
                } catch (IOException ioe) {
                    statusHandler.error("Could not close user print settings",
                            ioe);
                }
            }
        }
    }

    /**
     * Read user print settings if they exist.
     */
    private void readFromConfig() {

        LocalizationContext ctx = initUserLocalization();

        // Get a list of localization files!
        LocalizationFile f = PathManagerFactory.getPathManager()
                .getLocalizationFile(ctx, SETTINGS_FILENAME);
        // If its not there, no previous settings have been saved. Just exit.
        if (f.exists()) {
            UserPrintSettings settings = null;
            try {

                settings = (UserPrintSettings) JAXB.unmarshal(
                        f.openInputStream(), UserPrintSettings.class);

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
    public static LocalizationContext initUserLocalization() {
        return initLocalization(LocalizationLevel.USER);
    }

    /**
     * Initialize a LocalizationContext for the given LocalizationLevel.
     * 
     * @return the initialized localization
     */
    public static LocalizationContext initLocalization(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm.getContext(
                LocalizationType.COMMON_STATIC, level);
        return localization;
    }

}