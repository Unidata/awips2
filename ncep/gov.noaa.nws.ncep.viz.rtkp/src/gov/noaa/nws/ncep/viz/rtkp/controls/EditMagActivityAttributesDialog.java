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

import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.rtkp.MagActivityCapability;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormLayout;
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
import org.eclipse.swt.widgets.Slider;

/**
 * Provides an interface to modify the Mag activity map related capability
 * (k-indices circle colors, marker color, marker size, text size etc.)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           Ticket#     Engineer    Description
 * ------------   ----------  ----------- --------------------------
 * June 2, 2014   1122        S. Gurung     Initial creation
 * 
 * @author sgurung
 * @version 1.0
 */

public class EditMagActivityAttributesDialog extends Dialog {

    private String dlgTitle = "Edit Mag Activity Attributes";

    private static EditMagActivityAttributesDialog INSTANCE = null;

    private Shell shell;

    private RGB[] kIndicesColors;

    private ColorButtonSelector cms0 = null;

    private ColorButtonSelector cms1 = null;

    private ColorButtonSelector cms2 = null;

    private ColorButtonSelector cms3 = null;

    private ColorButtonSelector cms4 = null;

    private ColorButtonSelector cms5 = null;

    private ColorButtonSelector cms6 = null;

    private ColorButtonSelector cms7 = null;

    private RGB nonNetwrkStnColor = null;

    private ColorButtonSelector nnStnCms = null;

    private ColorButtonSelector[] kTextColorsCms = new ColorButtonSelector[8];

    private RGB[] kIndicesTextColors;

    private Float markerSize = null;

    private MarkerTextSize markerTextSize = null;

    public EditMagActivityAttributesDialog(Shell parentShell,
            String dialogTitle, RGB[] colors, RGB[] kIndicesTextColors,
            RGB nonNetwrkStnColor, Float markerSize,
            MarkerTextSize markerTextSize) {
        super(parentShell);
        this.dlgTitle = dialogTitle;

        shell = new Shell(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.setText(dialogTitle);
        shell.setSize(300, 500); // pack later

        this.kIndicesColors = colors;
        this.kIndicesTextColors = kIndicesTextColors;
        this.nonNetwrkStnColor = nonNetwrkStnColor;
        this.markerSize = markerSize;
        this.markerTextSize = markerTextSize;
    }

    /**
     * Creates the dialog if the dialog does not exist and returns the instance.
     * If the dialog exists, return the instance.
     * 
     * @param parShell
     * @return
     */
    public static EditMagActivityAttributesDialog getInstance(Shell parShell,
            String dialogTitle, RGB[] colors, RGB[] kIndicesTextColors,
            RGB nonNetwrkStnColor, Float markerSize,
            MarkerTextSize markerTextSize) {

        if (INSTANCE == null) {
            INSTANCE = new EditMagActivityAttributesDialog(parShell,
                    dialogTitle, colors, kIndicesTextColors, nonNetwrkStnColor,
                    markerSize, markerTextSize);
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
        initializeComponents(top);
        createCloseButton();

        return top;
    }

    public void initializeComponents(final Composite composite) {

        Composite top_form = composite;

        composite.setLayout(new GridLayout(1, true));
        GridData gd = new GridData();

        // K-indices colors

        Group kiColors = new Group(top_form, SWT.SHADOW_NONE);
        kiColors.setText("K-indices Colors");
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        kiColors.setLayoutData(gd);
        kiColors.setLayout(new FormLayout());

        createkIndicesColorsControls(kiColors);

        // K-indices text colors

        Group kiTextColors = new Group(top_form, SWT.SHADOW_NONE);
        kiTextColors.setText("K-indices Text Colors");
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        kiTextColors.setLayoutData(gd);
        kiTextColors.setLayout(new FormLayout());

        createkIndicesTextColorsControls(kiTextColors);

        // Marker attributes

        Group markerGroup = new Group(composite, SWT.SHADOW_NONE);
        markerGroup.setText("Marker");

        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        markerGroup.setLayoutData(gd);
        markerGroup.setLayout(new FormLayout());

        createMarkerControls(markerGroup);

    }

    public void createkIndicesColorsControls(Group kiGroup) {
        kiGroup.setLayout(new GridLayout(4, true));

        Label color0_lbl = new Label(kiGroup, SWT.NONE);
        color0_lbl.setText("Quiet:");

        cms0 = new ColorButtonSelector(kiGroup, 85, 25);
        cms0.setColorValue((RGB) kIndicesColors[0]);
        cms0.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[0] = cms0.getColorValue();
            }
        });

        Label color1_lbl = new Label(kiGroup, SWT.NONE);
        color1_lbl.setText("Unsettled:");

        cms1 = new ColorButtonSelector(kiGroup, 85, 25);
        cms1.setColorValue((RGB) kIndicesColors[1]);
        cms1.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[1] = cms1.getColorValue();
            }
        });

        Label color2_lbl = new Label(kiGroup, SWT.NONE);
        color2_lbl.setText("Active:");

        cms2 = new ColorButtonSelector(kiGroup, 85, 25);
        cms2.setColorValue((RGB) kIndicesColors[2]);
        cms2.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[2] = cms2.getColorValue();
            }
        });

        Label color3_lbl = new Label(kiGroup, SWT.NONE);
        color3_lbl.setText("Minorstorm:");

        cms3 = new ColorButtonSelector(kiGroup, 85, 25);
        cms3.setColorValue((RGB) kIndicesColors[3]);
        cms3.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[3] = cms3.getColorValue();
            }
        });

        Label color4_lbl = new Label(kiGroup, SWT.NONE);
        color4_lbl.setText("Moderatestorm:");

        cms4 = new ColorButtonSelector(kiGroup, 85, 25);
        cms4.setColorValue((RGB) kIndicesColors[4]);
        cms4.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[4] = cms4.getColorValue();
            }
        });

        Label color5_lbl = new Label(kiGroup, SWT.NONE);
        color5_lbl.setText("Strongstorm:");

        cms5 = new ColorButtonSelector(kiGroup, 85, 25);
        cms5.setColorValue((RGB) kIndicesColors[5]);
        cms5.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[5] = cms5.getColorValue();
            }
        });

        Label color6_lbl = new Label(kiGroup, SWT.NONE);
        color6_lbl.setText("Severestorm:");

        cms6 = new ColorButtonSelector(kiGroup, 85, 25);
        cms6.setColorValue((RGB) kIndicesColors[6]);
        cms6.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[6] = cms6.getColorValue();
            }
        });

        Label color7_lbl = new Label(kiGroup, SWT.NONE);
        color7_lbl.setText("Extremestorm:");

        cms7 = new ColorButtonSelector(kiGroup, 85, 25);
        cms7.setColorValue((RGB) kIndicesColors[7]);
        cms7.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesColors[7] = cms7.getColorValue();
            }
        });

    }

    public void createkIndicesTextColorsControls(Group kiTextGroup) {
        kiTextGroup.setLayout(new GridLayout(4, true));

        Label color0_lbl = new Label(kiTextGroup, SWT.NONE);
        color0_lbl.setText("Quiet:");

        kTextColorsCms[0] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[0].setColorValue((RGB) kIndicesTextColors[0]);
        kTextColorsCms[0].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[0] = kTextColorsCms[0].getColorValue();
                kIndicesTextColors[1] = kTextColorsCms[0].getColorValue();
                kIndicesTextColors[2] = kTextColorsCms[0].getColorValue();
            }
        });

        Label color1_lbl = new Label(kiTextGroup, SWT.NONE);
        color1_lbl.setText("Unsettled:");

        kTextColorsCms[1] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[1].setColorValue((RGB) kIndicesTextColors[3]);
        kTextColorsCms[1].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[3] = kTextColorsCms[1].getColorValue();
            }
        });

        Label color2_lbl = new Label(kiTextGroup, SWT.NONE);
        color2_lbl.setText("Active:");

        kTextColorsCms[2] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[2].setColorValue((RGB) kIndicesTextColors[4]);
        kTextColorsCms[2].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[4] = kTextColorsCms[2].getColorValue();
            }
        });

        Label color3_lbl = new Label(kiTextGroup, SWT.NONE);
        color3_lbl.setText("Minorstorm:");

        kTextColorsCms[3] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[3].setColorValue((RGB) kIndicesTextColors[5]);
        kTextColorsCms[3].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[5] = kTextColorsCms[3].getColorValue();
            }
        });

        Label color4_lbl = new Label(kiTextGroup, SWT.NONE);
        color4_lbl.setText("Moderatestorm:");

        kTextColorsCms[4] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[4].setColorValue((RGB) kIndicesTextColors[6]);
        kTextColorsCms[4].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[6] = kTextColorsCms[4].getColorValue();
            }
        });

        Label color5_lbl = new Label(kiTextGroup, SWT.NONE);
        color5_lbl.setText("Strongstorm:");

        kTextColorsCms[5] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[5].setColorValue((RGB) kIndicesTextColors[7]);
        kTextColorsCms[5].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[7] = kTextColorsCms[5].getColorValue();
            }
        });

        Label color6_lbl = new Label(kiTextGroup, SWT.NONE);
        color6_lbl.setText("Severestorm:");

        kTextColorsCms[6] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[6].setColorValue((RGB) kIndicesTextColors[8]);
        kTextColorsCms[6].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[8] = kTextColorsCms[6].getColorValue();
            }
        });

        Label color7_lbl = new Label(kiTextGroup, SWT.NONE);
        color7_lbl.setText("Extremestorm:");

        kTextColorsCms[7] = new ColorButtonSelector(kiTextGroup, 85, 25);
        kTextColorsCms[7].setColorValue((RGB) kIndicesTextColors[9]);
        kTextColorsCms[7].addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                kIndicesTextColors[9] = kTextColorsCms[7].getColorValue();
            }
        });

    }

    public void createMarkerControls(Group markerGroup) {
        markerGroup.setLayout(new GridLayout(2, true));

        // Marker Size

        Label markerSize_lbl = new Label(markerGroup, SWT.NONE);
        markerSize_lbl.setText("Marker Size:");

        Group markerSizeGroup = new Group(markerGroup, SWT.SHADOW_NONE);
        markerSizeGroup.setLayout(new GridLayout(1, true));

        final Label selectMarkerSizeSliderText = new Label(markerSizeGroup,
                SWT.NONE);
        GridData gridData1 = new GridData();
        gridData1.horizontalIndent = 50;
        gridData1.verticalIndent = 0;
        selectMarkerSizeSliderText.setLayoutData(gridData1);
        final Slider selectMarkerSizeSlider = new Slider(markerSizeGroup,
                SWT.HORIZONTAL);
        selectMarkerSizeSlider.setMinimum(5);
        selectMarkerSizeSlider.setMaximum(31);
        selectMarkerSizeSlider.setIncrement(1);
        selectMarkerSizeSlider.setThumb(1);
        selectMarkerSizeSlider.setSelection(10);
        float mSize = markerSize;
        selectMarkerSizeSlider.setSelection((int) (mSize * 10f + 0.5));
        selectMarkerSizeSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                markerSize = (Float) ((float) selectMarkerSizeSlider
                        .getSelection() / 10);
                selectMarkerSizeSliderText.setText(markerSize.toString());
                selectMarkerSizeSliderText.redraw();
                selectMarkerSizeSliderText.update();
            }
        });
        selectMarkerSizeSliderText.setText(markerSize.toString());

        // Marker Text Size

        Label markerTextSize_lbl = new Label(markerGroup, SWT.NONE);
        markerTextSize_lbl.setText("MarkerText Size:");

        final Combo selectMarkerTextSizeCombo = new Combo(markerGroup,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        for (MarkerTextSize mts : MarkerTextSize.values()) {
            selectMarkerTextSizeCombo.add(mts.getDisplayName());
        }
        String markerTextSizeMenuEntry = markerTextSize.getDisplayName();
        selectMarkerTextSizeCombo.setText(markerTextSizeMenuEntry);
        selectMarkerTextSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                markerTextSize = MarkerTextSize.values()[selectMarkerTextSizeCombo
                        .getSelectionIndex()];
            }
        });

        Label nonNetwrkStnMkr_lbl = new Label(markerGroup, SWT.NONE);
        nonNetwrkStnMkr_lbl.setText("Non-network Station Marker Color:");

        nnStnCms = new ColorButtonSelector(markerGroup, 85, 25);
        nnStnCms.setColorValue((RGB) nonNetwrkStnColor);
        nnStnCms.addListener(new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event) {
                nonNetwrkStnColor = nnStnCms.getColorValue();
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

    public MagActivityCapability open() {
        Shell parent = getParent();
        Display display = parent.getDisplay();

        if (shell == null || shell.isDisposed()) {
            shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
            shell.setText(dlgTitle);
            shell.setSize(300, 500);
        }

        createDialog(shell);

        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        MagActivityCapability mac = new MagActivityCapability(kIndicesColors,
                kIndicesTextColors, nonNetwrkStnColor, markerSize,
                markerTextSize);

        return mac;
    }

}
