/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.controls;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.rtkp.KsPlotCapability;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil.GeoMagStationType;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * Provides an interface to modify the Ks plot attributes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer    Description
 * ------------  ----------  ----------- --------------------------
 * June 06, 2014 1122        S. Gurung     Initial creation
 * 
 * @author sgurung
 * @version 1.0
 */

public class EditKsPlotAttributesDialog extends Dialog {

    private String dlgTitle = "Edit Ks Plot Colors";

    private static EditKsPlotAttributesDialog INSTANCE = null;

    private Shell shell;

    private RGB[] rgbs;

    private ColorButtonSelector cms0 = null;

    private ColorButtonSelector cms1 = null;

    private ColorButtonSelector cms2 = null;

    private ColorButtonSelector cms3 = null;

    private ColorButtonSelector cms4 = null;

    private ColorButtonSelector cms5 = null;

    private ColorButtonSelector cms6 = null;

    private ColorButtonSelector cms7 = null;

    private ColorButtonSelector cms8 = null;

    private Combo textSizeCombo = null;

    private Combo textFontCombo = null;

    private ColorButtonSelector textColorCms = null;

    private Combo pointStyleCombo = null;

    private Combo pointSizeCombo = null;

    private KsPlotCapability ksCap = null;

    private final String[] textSizeOptions = { "10", "12", "14", "16", "18",
            "20", "22", "24", "32" };

    private final String[] textFontOptions = { "Courier", "Helvetica", "Times" };

    private final String[] pointSizeOptions = { "0.5", "0.75", "1.0", "1.25",
            "1.5", "1.75", "2.0", "2.25", "2.5", "2.75", "3.0" };

    private final String[] pointStyleOptions = { "POINT", "CROSS", "X", "STAR",
            "CIRCLE", "DISC", "DASH", "BOX", "SQUARE" };

    public EditKsPlotAttributesDialog(Shell parentShell, String dialogTitle,
            KsPlotCapability ksCap) {
        super(parentShell);
        this.dlgTitle = dialogTitle;

        shell = new Shell(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.setText(dialogTitle);
        shell.setSize(600, 800); // pack later

        this.ksCap = ksCap;
        rgbs = ksCap.getPlotColors();
    }

    public void initWidgets() {

        textSizeCombo.setItems(textSizeOptions);

        for (int i = 0; i < textSizeOptions.length; i++) {
            String sz = ksCap.getTextSize();
            if (textSizeOptions[i] == sz) {
                textSizeCombo.select(i);
            }
        }

        textFontCombo.setItems(textFontOptions);
        for (int i = 0; i < textFontOptions.length; i++) {
            String fnt = ksCap.getTextFont();
            if (textFontOptions[i] == fnt) {
                textFontCombo.select(i);
            }
        }

        pointSizeCombo.setItems(pointSizeOptions);

        for (int i = 0; i < pointSizeOptions.length; i++) {
            String sz = ksCap.getPointSize();
            if (pointSizeOptions[i] == sz) {
                pointSizeCombo.select(i);
            }
        }

        pointStyleCombo.setItems(pointStyleOptions);

        for (int i = 0; i < pointStyleOptions.length; i++) {
            String ps = ksCap.getPointStyle();
            if (pointStyleOptions[i] == ps) {
                pointStyleCombo.select(i);
            }
        }
    }

    /**
     * Creates the dialog if the dialog does not exist and returns the instance.
     * If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static EditKsPlotAttributesDialog getInstance(Shell parShell,
            String dialogTitle, KsPlotCapability ksCap) {

        if (INSTANCE == null) {
            INSTANCE = new EditKsPlotAttributesDialog(parShell, dialogTitle,
                    ksCap);
        }
        return INSTANCE;

    }

    public Composite createDialog(Composite composite) {
        Composite top = composite;

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        top.setLayout(mainLayout);

        // Initialize all of the controls, and layouts

        // Ks plot colors
        Group colorsGroup = new Group(top, SWT.SHADOW_NONE);
        colorsGroup.setText("Station Count Colors");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        colorsGroup.setLayoutData(gd);

        createColorsControls(colorsGroup);

        // Text options
        Group textPointOptionsGroup = new Group(top, SWT.SHADOW_NONE);
        textPointOptionsGroup.setText("Text Options");
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        textPointOptionsGroup.setLayoutData(gd);

        createTextPointAttrControls(textPointOptionsGroup);

        initWidgets();

        createCloseButton();

        return top;
    }

    public void createColorsControls(final Group colorsGroup) {

        colorsGroup.setLayout(new GridLayout(4, true));

        int numStations = RTKpUtil.getGeoMagStationCodes(GeoMagStationType.KP)
                .size();

        Label color8_lbl = new Label(colorsGroup, SWT.NONE);
        color8_lbl.setText("StationCount=" + numStations + ":");

        cms8 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms8.setColorValue((RGB) rgbs[8]);
        cms8.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[8] = cms8.getColorValue();
            }
        });

        Label color1_lbl = new Label(colorsGroup, SWT.NONE);
        color1_lbl.setText("StationCount=" + (numStations - 1) + ":");

        cms1 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms1.setColorValue((RGB) rgbs[1]);
        cms1.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[1] = cms1.getColorValue();
            }
        });

        Label color2_lbl = new Label(colorsGroup, SWT.NONE);
        color2_lbl.setText("StationCount=" + (numStations - 2) + ":");

        cms2 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms2.setColorValue((RGB) rgbs[2]);
        cms2.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[2] = cms2.getColorValue();
            }
        });

        Label color3_lbl = new Label(colorsGroup, SWT.NONE);
        color3_lbl.setText("StationCount=" + (numStations - 3) + ":");

        cms3 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms3.setColorValue((RGB) rgbs[3]);
        cms3.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[3] = cms3.getColorValue();
            }
        });

        Label color4_lbl = new Label(colorsGroup, SWT.NONE);
        color4_lbl.setText("StationCount=" + (numStations - 4) + ":");

        cms4 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms4.setColorValue((RGB) rgbs[4]);
        cms4.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[4] = cms4.getColorValue();
            }
        });

        Label color5_lbl = new Label(colorsGroup, SWT.NONE);
        color5_lbl.setText("StationCount=" + (numStations - 5) + ":");

        cms5 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms5.setColorValue((RGB) rgbs[5]);
        cms5.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[5] = cms5.getColorValue();
            }
        });

        Label color6_lbl = new Label(colorsGroup, SWT.NONE);
        color6_lbl.setText("StationCount=" + (numStations - 6) + ":");

        cms6 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms6.setColorValue((RGB) rgbs[6]);
        cms6.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[6] = cms6.getColorValue();
            }
        });

        Label color7_lbl = new Label(colorsGroup, SWT.NONE);
        String color7_lbl_str = (numStations - 7) + "";
        if ((numStations - 7) > 1) {
            for (int i = numStations - 7; i > 0; i--) {
                color7_lbl_str += ", " + i;
            }
        }
        color7_lbl.setText("StationCount=" + color7_lbl_str + ":");

        cms7 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms7.setColorValue((RGB) rgbs[7]);
        cms7.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[7] = cms7.getColorValue();
            }
        });

        Label color0_lbl = new Label(colorsGroup, SWT.NONE);
        color0_lbl.setText("StationCount=0:");

        cms0 = new ColorButtonSelector(colorsGroup, 85, 25);
        cms0.setColorValue((RGB) rgbs[0]);
        cms0.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                rgbs[0] = cms0.getColorValue();
            }
        });

    }

    /*
     * Create text attributes -- size, font and style
     */
    private void createTextPointAttrControls(Group txtPtGroup) {

        txtPtGroup.setLayout(new GridLayout(4, true));

        GridLayout gl = new GridLayout(4, false);
        gl.marginTop = 0;
        gl.marginBottom = 3;
        gl.marginRight = 0;
        txtPtGroup.setLayout(gl);
        txtPtGroup.setText("Text and Point Options");

        // Text size attribute
        new Label(txtPtGroup, SWT.NONE).setText("Text Size: ");
        textSizeCombo = new Combo(txtPtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);

        textSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ksCap.setTextSize(textSizeCombo.getText());
            }
        });

        // Point size attribute
        new Label(txtPtGroup, SWT.NONE).setText("              Point Size: ");
        pointSizeCombo = new Combo(txtPtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);

        pointSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ksCap.setPointSize(pointSizeCombo.getText());
            }
        });

        // Text font attribute
        new Label(txtPtGroup, SWT.NONE).setText("Text Font: ");
        textFontCombo = new Combo(txtPtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        textFontCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ksCap.setTextFont(textFontCombo.getText());
            }
        });

        // Point size attribute
        new Label(txtPtGroup, SWT.NONE).setText("              Point Style: ");
        pointStyleCombo = new Combo(txtPtGroup, SWT.DROP_DOWN | SWT.READ_ONLY);

        pointStyleCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ksCap.setPointStyle(pointStyleCombo.getText());
            }
        });

        // Text color
        Label color_lbl = new Label(txtPtGroup, SWT.NONE);
        color_lbl.setText("Text Color:");

        textColorCms = new ColorButtonSelector(txtPtGroup, 85, 25);
        textColorCms.setColorValue(ksCap.getTextColor());
        textColorCms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                ksCap.setTextColor(textColorCms.getColorValue());
            }
        });

    }

    /*
     * Create point attributes -- style and size
     */
    private void createPointAttrControls(Group pointGroup) {

        pointGroup.setLayout(new GridLayout(2, true));

        GridLayout gl = new GridLayout(2, false);
        gl.marginTop = 0;
        gl.marginBottom = 3;
        gl.marginRight = 0;
        pointGroup.setLayout(gl);
        pointGroup.setText("Point");

        // Point size attribute
        new Label(pointGroup, SWT.NONE).setText("Style: ");
        pointStyleCombo = new Combo(pointGroup, SWT.DROP_DOWN | SWT.READ_ONLY);

        pointStyleCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ksCap.setPointStyle(pointStyleCombo.getText());
            }
        });

        // Point size attribute
        new Label(pointGroup, SWT.NONE).setText("Size: ");
        pointSizeCombo = new Combo(pointGroup, SWT.DROP_DOWN | SWT.READ_ONLY);

        pointSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ksCap.setPointSize(pointSizeCombo.getText());
            }
        });
    }

    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, true);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalAlignment = SWT.CENTER;
        centeredComp.setLayoutData(gd);

        Button ok_btn = new Button(centeredComp, SWT.NONE);
        ok_btn.setText("   OK    ");
        ok_btn.setLayoutData(gd);
        ok_btn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("   Close   ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    public KsPlotCapability open() {
        Shell parent = getParent();
        Display display = parent.getDisplay();

        if (shell == null || shell.isDisposed()) {
            shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
            shell.setText(dlgTitle);
            shell.setSize(600, 800);
        }

        createDialog(shell);

        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        ksCap.setPlotColors(rgbs);

        return ksCap;
    }

}
