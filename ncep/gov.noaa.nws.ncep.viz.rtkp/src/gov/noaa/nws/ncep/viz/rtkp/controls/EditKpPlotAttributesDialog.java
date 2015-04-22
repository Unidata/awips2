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
import gov.noaa.nws.ncep.viz.rtkp.KpPlotCapability;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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
 * Provides an interface to modify the Kp plot attributes.
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

public class EditKpPlotAttributesDialog extends Dialog {

    private String dlgTitle = "Edit Kp Plot Colors";

    private static EditKpPlotAttributesDialog INSTANCE = null;

    private Shell shell;

    private ColorButtonSelector plotColorCms = null;

    private Combo textSizeCombo = null;

    private Combo textFontCombo = null;

    private ColorButtonSelector textColorCms = null;

    private Combo pointStyleCombo = null;

    private Combo pointSizeCombo = null;

    private KpPlotCapability kpCap = null;

    private final String[] textSizeOptions = { "10", "12", "14", "16", "18",
            "20", "22", "24", "32" };

    private final String[] textFontOptions = { "Courier", "Helvetica", "Times" };

    private final String[] pointSizeOptions = { "0.5", "0.75", "1.0", "1.25",
            "1.5", "1.75", "2.0", "2.25", "2.5", "2.75", "3.0" };

    private final String[] pointStyleOptions = { "POINT", "CROSS", "X", "STAR",
            "CIRCLE", "DISC", "DASH", "BOX", "SQUARE" };

    public EditKpPlotAttributesDialog(Shell parentShell, String dialogTitle,
            KpPlotCapability kpCap) {
        super(parentShell);
        this.dlgTitle = dialogTitle;

        shell = new Shell(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.setText(dialogTitle);
        shell.setSize(600, 800); // pack later

        this.kpCap = kpCap;
    }

    public void initWidgets() {

        textSizeCombo.setItems(textSizeOptions);

        for (int i = 0; i < textSizeOptions.length; i++) {
            String sz = kpCap.getTextSize();
            if (textSizeOptions[i] == sz) {
                textSizeCombo.select(i);
            }
        }

        textFontCombo.setItems(textFontOptions);
        for (int i = 0; i < textFontOptions.length; i++) {
            String fnt = kpCap.getTextFont();
            if (textFontOptions[i] == fnt) {
                textFontCombo.select(i);
            }
        }

        pointSizeCombo.setItems(pointSizeOptions);

        for (int i = 0; i < pointSizeOptions.length; i++) {
            String sz = kpCap.getPointSize();
            if (pointSizeOptions[i] == sz) {
                pointSizeCombo.select(i);
            }
        }

        pointStyleCombo.setItems(pointStyleOptions);

        for (int i = 0; i < pointStyleOptions.length; i++) {
            String ps = kpCap.getPointStyle();
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
    public static EditKpPlotAttributesDialog getInstance(Shell parShell,
            String dialogTitle, KpPlotCapability ksCap) {

        if (INSTANCE == null) {
            INSTANCE = new EditKpPlotAttributesDialog(parShell, dialogTitle,
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

        // Plot, Text and Point attribute options
        Group optionsGroup = new Group(top, SWT.SHADOW_NONE);
        optionsGroup.setText("Options");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        optionsGroup.setLayoutData(gd);

        createAttrControls(optionsGroup);

        initWidgets();

        createCloseButton();

        return top;
    }

    /*
     * Create plot attribute -- color, text attributes -- size, font and style
     * and point attributes -- size and style.
     */
    private void createAttrControls(Group group) {

        group.setLayout(new GridLayout(4, true));

        GridLayout gl = new GridLayout(4, false);
        gl.marginTop = 0;
        gl.marginBottom = 3;
        gl.marginRight = 0;
        group.setLayout(gl);
        group.setText("Options");

        // Text size attribute
        new Label(group, SWT.NONE).setText("Text Size: ");
        textSizeCombo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);

        textSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                kpCap.setTextSize(textSizeCombo.getText());
            }
        });

        // Point size attribute
        new Label(group, SWT.NONE).setText("             Point Size: ");
        pointSizeCombo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);

        pointSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                kpCap.setPointSize(pointSizeCombo.getText());
            }
        });

        // Text font attribute
        new Label(group, SWT.NONE).setText("Text Font: ");
        textFontCombo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
        textFontCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                kpCap.setTextFont(textFontCombo.getText());
            }
        });

        // Point size attribute
        new Label(group, SWT.NONE).setText("             Point Style: ");
        pointStyleCombo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);

        pointStyleCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                kpCap.setPointStyle(pointStyleCombo.getText());
            }
        });

        // Text color
        Label color_lbl = new Label(group, SWT.NONE);
        color_lbl.setText("Text Color:");

        textColorCms = new ColorButtonSelector(group, 85, 25);
        textColorCms.setColorValue(kpCap.getTextColor());
        textColorCms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kpCap.setTextColor(textColorCms.getColorValue());
            }
        });

        // Plot color
        Label plotcolor_lbl = new Label(group, SWT.NONE);
        plotcolor_lbl.setText("             Plot Color:");

        plotColorCms = new ColorButtonSelector(group, 85, 25);
        plotColorCms.setColorValue(kpCap.getPlotColor());
        plotColorCms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kpCap.setPlotColor(plotColorCms.getColorValue());
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

    public KpPlotCapability open() {
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

        return kpCap;
    }

}
