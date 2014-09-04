package gov.noaa.nws.ncep.viz.rsc.ncscat.rsc;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarEditor;
import gov.noaa.nws.ncep.viz.rsc.ncscat.rsc.NcscatResourceData.ArrowStyle;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

import java.util.EnumSet;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

/**
 * An interface to edit NCSCAT resource attributes.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 14 Jul 2010   #235C       B. Hebbard   Initial Creation.
 * 11 Aug 2010   #273        G. Hull      Call getNcscatMode() instead of using getNatlCntrsResourceName.
 * 08 Oct 2010               B. Hebbard   Fix tool tips.
 * 01 Jul 2014 TTR 1018      S. Russell   Updated call to ColorBarEditor
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */

public class EditNcscatAttrsDialog extends AbstractEditResourceAttrsDialog {

    public EditNcscatAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
        super(parentShell, r, apply);
        ncscatMode = ((NcscatResourceData) r).getNcscatMode();
    }

    private NcscatMode ncscatMode = null;

    private RscAttrValue skipEnableAttr = null;

    private RscAttrValue skipValueAttr = null;

    private RscAttrValue densityValueAttr = null;

    private RscAttrValue timeStampEnableAttr = null;

    private RscAttrValue timeStampColorAttr = null;

    private RscAttrValue timeStampIntervalAttr = null;

    private RscAttrValue timeStampLineWidthAttr = null;

    private RscAttrValue arrowStyleAttr = null;

    private RscAttrValue arrowWidthAttr = null;

    private RscAttrValue arrowSizeAttr = null;

    private RscAttrValue headSizeAttr = null;

    private RscAttrValue colorBarAttr01 = null;

    private RscAttrValue colorBarAttr02 = null;

    private RscAttrValue highWindSpeedEnableAttr = null;

    private RscAttrValue lowWindSpeedEnableAttr = null;

    private RscAttrValue rainFlagEnableAttr = null;

    private RscAttrValue availabilityFlagEnableAttr = null;

    private RscAttrValue use2ndColorForRainEnableAttr = null;

    private RscAttrValue plotCirclesForRainEnableAttr = null;

    Button skipButton = null;

    Button densityButton = null;

    Button timeStampButton = null;

    Button highWindSpeedButton = null;

    Button lowWindSpeedButton = null;

    Button rainQcFlagButton = null;

    Button availRedunFlagButton = null;

    Button use2ndColorForRainQcButton = null;

    Button edit2ndColorButton = null;

    Button plotCirclesForRainQcButton = null;

    ColorBar editedColorBar1 = null;

    ColorBar editedColorBar2 = null;

    ColorBarEditor colorBarEditor1 = null;

    ColorBarEditor colorBarEditor2 = null;

    ColorButtonSelector timeStampColorSelector = null;

    Combo arrowBarbStyleCombo = null;

    Composite timeStampColorComp = null;

    Group colorBarGrp1 = null;

    Group colorBarGrp2 = null;

    Label skipLabel1 = null;

    Label densityLabel1 = null;

    Label timeStampLabel1 = null;

    Label timeStampLabel2 = null;

    Label timeStampLabel3 = null;

    Label timeStampLabel4 = null;

    Label arrowLabel1 = null;

    Label arrowLabel2 = null;

    Label arrowLabel3 = null;

    Label arrowLabel4 = null;

    Label flagInclusionLabel = null;

    Label edit2ndColorLabel = null;

    Spinner skipSpinner = null;

    Spinner densitySpinner = null;

    Spinner timeStampIntervalSpinner = null;

    Spinner timeStampLineWidthSpinner = null;

    Spinner arrowWidthSpinner = null;

    Spinner arrowSizeSpinner = null;

    Spinner arrowHeadSizeSpinner = null;

    // 
    @Override
    public Composite createDialog(Composite topComp) {

        skipEnableAttr = editedRscAttrSet.getRscAttr("skipEnable");
        skipValueAttr = editedRscAttrSet.getRscAttr("skipValue");
        densityValueAttr = editedRscAttrSet.getRscAttr("densityValue");
        timeStampEnableAttr = editedRscAttrSet.getRscAttr("timeStampEnable");
        timeStampColorAttr = editedRscAttrSet.getRscAttr("timeStampColor");
        timeStampIntervalAttr = editedRscAttrSet.getRscAttr("timeStampInterval");
        timeStampLineWidthAttr = editedRscAttrSet.getRscAttr("timeStampLineWidth");
        arrowStyleAttr = editedRscAttrSet.getRscAttr("arrowStyle");
        arrowWidthAttr = editedRscAttrSet.getRscAttr("arrowWidth");
        arrowSizeAttr = editedRscAttrSet.getRscAttr("arrowSize");
        headSizeAttr = editedRscAttrSet.getRscAttr("headSize");
        colorBarAttr01 = editedRscAttrSet.getRscAttr("colorBar1");
        colorBarAttr02 = editedRscAttrSet.getRscAttr("colorBar2");
        highWindSpeedEnableAttr = editedRscAttrSet.getRscAttr("highWindSpeedEnable");
        lowWindSpeedEnableAttr = editedRscAttrSet.getRscAttr("lowWindSpeedEnable");
        rainFlagEnableAttr = editedRscAttrSet.getRscAttr("rainFlagEnable");
        availabilityFlagEnableAttr = editedRscAttrSet.getRscAttr("availabilityFlagEnable");
        use2ndColorForRainEnableAttr = editedRscAttrSet.getRscAttr("use2ndColorForRainEnable");
        plotCirclesForRainEnableAttr = editedRscAttrSet.getRscAttr("plotCirclesForRainEnable");

        if (skipEnableAttr == null || skipEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("skipEnable is null or not of expected class Boolean?");
            return null;
        }
        if (skipValueAttr == null || skipValueAttr.getAttrClass() != Integer.class) {
            System.out.println("skipValue is null or not of expected class Integer?");
            return null;
        }
        if (densityValueAttr == null || densityValueAttr.getAttrClass() != Integer.class) {
            System.out.println("densityValue is null or not of expected class Integer?");
            return null;
        }
        if (timeStampEnableAttr == null || timeStampEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("timeStampEnable is null or not of expected class Boolean?");
            return null;
        }
        if (timeStampColorAttr == null || timeStampColorAttr.getAttrClass() != RGB.class) {
            System.out.println("timeStampColor is null or not of expected class RGB?");
            return null;
        }
        if (timeStampIntervalAttr == null || timeStampIntervalAttr.getAttrClass() != Integer.class) {
            System.out.println("timeStampInterval is null or not of expected class Integer?");
            return null;
        }
        if (timeStampLineWidthAttr == null || timeStampLineWidthAttr.getAttrClass() != Integer.class) {
            System.out.println("timeStampLineWidth is null or not of expected class Integer?");
            return null;
        }
        if (arrowStyleAttr == null || arrowStyleAttr.getAttrClass() != ArrowStyle.class) {
            System.out.println("arrowStyle is null or not of expected class ArrowStyle?");
            return null;
        }
        if (arrowWidthAttr == null || arrowWidthAttr.getAttrClass() != Integer.class) {
            System.out.println("arrowWidth is null or not of expected class Integer?");
            return null;
        }
        if (arrowSizeAttr == null || arrowSizeAttr.getAttrClass() != Integer.class) {
            System.out.println("arrowSize is null or not of expected class Integer?");
            return null;
        }
        if (headSizeAttr == null || headSizeAttr.getAttrClass() != Integer.class) {
            System.out.println("headSize is null or not of expected class Integer?");
            return null;
        }
        if (highWindSpeedEnableAttr == null || highWindSpeedEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("highWindSpeedEnable is null or not of expected class Boolean?");
            return null;
        }
        if (lowWindSpeedEnableAttr == null || lowWindSpeedEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("lowWindSpeedEnable is null or not of expected class Boolean?");
            return null;
        }
        if (rainFlagEnableAttr == null || rainFlagEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("rainFlagEnable is null or not of expected class Boolean?");
            return null;
        }
        if (availabilityFlagEnableAttr == null || availabilityFlagEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("availabilityFlagEnable is null or not of expected class Boolean?");
            return null;
        }
        if (use2ndColorForRainEnableAttr == null || use2ndColorForRainEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("use2ndColorForRainEnable is null or not of expected class Boolean?");
            return null;
        }
        if (plotCirclesForRainEnableAttr == null || plotCirclesForRainEnableAttr.getAttrClass() != Boolean.class) {
            System.out.println("plotCirclesForRainEnable is null or not of expected class Boolean?");
            return null;
        }
        if (colorBarAttr01 == null || colorBarAttr01.getAttrClass() != ColorBar.class) {
            System.out.println("colorBar1 is null or not of expected class ColorBar?");
            return null;
        }
        if (colorBarAttr02 == null || colorBarAttr02.getAttrClass() != ColorBar.class) {
            System.out.println("colorBar2 is null or not of expected class ColorBar?");
            return null;
        }

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        //  Arrow Type and Attributes

        arrowBarbStyleCombo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        FormData fd = new FormData();
        //TODO:  Consider anchoring label to left, then combo to right of it
        fd.left = new FormAttachment(0, 168);
        fd.top = new FormAttachment(0, 32);
        arrowBarbStyleCombo.setLayoutData(fd);
        arrowBarbStyleCombo.setToolTipText("Type of arrow or wind barb to be drawn at each wind vector cell");

        for (ArrowStyle as : ArrowStyle.values()) {
            arrowBarbStyleCombo.add(as.getDisplayName());
        }

        ArrowStyle arrowStyle = (ArrowStyle) arrowStyleAttr.getAttrValue();
        arrowBarbStyleCombo.select(arrowStyle.ordinal());
        arrowBarbStyleCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                ArrowStyle as = ArrowStyle.values()[arrowBarbStyleCombo.getSelectionIndex()];
                arrowStyleAttr.setAttrValue(as);
                arrowHeadSizeSpinner.setEnabled(as != ArrowStyle.WIND_BARB);
            }
        });

        arrowLabel1 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.right = new FormAttachment(arrowBarbStyleCombo, -6, SWT.LEFT);
        fd.top = new FormAttachment(arrowBarbStyleCombo, 3, SWT.TOP);
        arrowLabel1.setLayoutData(fd);
        arrowLabel1.setText("Show Wind Vectors As");

        arrowWidthSpinner = new Spinner(topComp, SWT.BORDER);
        arrowWidthSpinner.setToolTipText("Width in pixels of each arrow or wind barb");
        fd = new FormData();
        fd.left = new FormAttachment(arrowBarbStyleCombo, 24, SWT.RIGHT);
        fd.top = new FormAttachment(arrowBarbStyleCombo, 0, SWT.CENTER);
        arrowWidthSpinner.setLayoutData(fd);
        arrowWidthSpinner.setDigits(0);
        arrowWidthSpinner.setMinimum(1);
        arrowWidthSpinner.setMaximum(10);
        arrowWidthSpinner.setIncrement(1);
        arrowWidthSpinner.setPageIncrement(1);
        arrowWidthSpinner.setSelection(((Integer) arrowWidthAttr.getAttrValue()).intValue());
        arrowWidthSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                arrowWidthAttr.setAttrValue(new Integer(arrowWidthSpinner.getSelection()));
            }
        });

        arrowLabel2 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(arrowWidthSpinner, 0, SWT.CENTER);
        fd.bottom = new FormAttachment(arrowWidthSpinner, -3, SWT.TOP);
        arrowLabel2.setLayoutData(fd);
        arrowLabel2.setText("Width");

        arrowSizeSpinner = new Spinner(topComp, SWT.BORDER);
        arrowSizeSpinner.setToolTipText("Relative size of each wind barb or arrow");
        fd = new FormData();
        fd.left = new FormAttachment(arrowWidthSpinner, 24, SWT.RIGHT);
        fd.top = new FormAttachment(arrowWidthSpinner, 0, SWT.CENTER);
        arrowSizeSpinner.setLayoutData(fd);
        arrowSizeSpinner.setDigits(1);
        arrowSizeSpinner.setMinimum(1);
        arrowSizeSpinner.setMaximum(100);
        arrowSizeSpinner.setIncrement(1);
        arrowSizeSpinner.setPageIncrement(1);
        arrowSizeSpinner.setSelection(((Integer) arrowSizeAttr.getAttrValue()).intValue());
        arrowSizeSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                arrowSizeAttr.setAttrValue(new Integer(arrowSizeSpinner.getSelection()));
            }
        });

        arrowLabel3 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(arrowSizeSpinner, 0, SWT.CENTER);
        fd.bottom = new FormAttachment(arrowSizeSpinner, -3, SWT.TOP);
        arrowLabel3.setLayoutData(fd);
        arrowLabel3.setText("Size");

        arrowHeadSizeSpinner = new Spinner(topComp, SWT.BORDER);
        arrowHeadSizeSpinner.setToolTipText("Relative size of head on each arrow");
        fd = new FormData();
        fd.left = new FormAttachment(arrowSizeSpinner, 24, SWT.RIGHT);
        fd.top = new FormAttachment(arrowSizeSpinner, 0, SWT.CENTER);
        arrowHeadSizeSpinner.setLayoutData(fd);
        arrowHeadSizeSpinner.setDigits(1);
        arrowHeadSizeSpinner.setMinimum(1);
        arrowHeadSizeSpinner.setMaximum(100);
        arrowHeadSizeSpinner.setIncrement(1);
        arrowHeadSizeSpinner.setPageIncrement(1);
        arrowHeadSizeSpinner.setSelection(((Integer) headSizeAttr.getAttrValue()).intValue());
        arrowHeadSizeSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                headSizeAttr.setAttrValue(new Integer(arrowHeadSizeSpinner.getSelection()));
            }
        });
        arrowHeadSizeSpinner.setEnabled(arrowStyle != ArrowStyle.WIND_BARB);

        arrowLabel4 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(arrowHeadSizeSpinner, 0, SWT.CENTER);
        fd.bottom = new FormAttachment(arrowHeadSizeSpinner, -3, SWT.TOP);
        arrowLabel4.setLayoutData(fd);
        arrowLabel4.setText("Head");

        //  Skip

        skipButton = new Button(topComp, SWT.RADIO);
        fd = new FormData();
        fd.left = new FormAttachment(8, 0);
        fd.top = new FormAttachment(arrowBarbStyleCombo, 12, SWT.BOTTOM);
        skipButton.setLayoutData(fd);
        skipButton.setSelection(((Boolean) skipEnableAttr.getAttrValue()).booleanValue());
        skipButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                skipEnableAttr.setAttrValue(new Boolean(skipButton.getSelection()));
                skipSpinner.setEnabled(skipButton.getSelection());
                skipLabel1.setEnabled(skipButton.getSelection());
                densitySpinner.setEnabled(densityButton.getSelection());
                densityLabel1.setEnabled(densityButton.getSelection());
            }
        });
        skipButton.setText("Skip");
        skipButton.setToolTipText("Omit specified number of rows, and points within each row, between ones drawn");

        //TODO:  As enhancement, allow separate values for rows and columns, optionally linked...
        skipSpinner = new Spinner(topComp, SWT.BORDER);
        skipSpinner.setToolTipText("Skip this many rows and columns between displayed ones");
        fd = new FormData();
        fd.left = new FormAttachment(skipButton, 8, SWT.RIGHT);
        fd.top = new FormAttachment(skipButton, 0, SWT.CENTER);
        skipSpinner.setLayoutData(fd);
        skipSpinner.setDigits(0);
        skipSpinner.setMinimum(0);
        skipSpinner.setMaximum(99);
        skipSpinner.setIncrement(1);
        skipSpinner.setPageIncrement(1);
        skipSpinner.setSelection(((Integer) skipValueAttr.getAttrValue()).intValue());
        skipSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                skipValueAttr.setAttrValue(new Integer(skipSpinner.getSelection()));
            }
        });
        skipSpinner.setEnabled(skipButton.getSelection());

        skipLabel1 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(skipSpinner, 8, SWT.RIGHT);
        fd.top = new FormAttachment(skipButton, 0, SWT.CENTER);
        skipLabel1.setLayoutData(fd);
        skipLabel1.setText("points/rows between displayed ones");
        skipLabel1.setEnabled(skipButton.getSelection());

        //  Density

        densityButton = new Button(topComp, SWT.RADIO);
        fd = new FormData();
        fd.left = new FormAttachment(8, 0);
        fd.top = new FormAttachment(skipButton, 12, SWT.BOTTOM);
        densityButton.setLayoutData(fd);
        densityButton.setSelection(!((Boolean) skipEnableAttr.getAttrValue()).booleanValue());
        densityButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                skipEnableAttr.setAttrValue(new Boolean(!densityButton.getSelection()));
                skipSpinner.setEnabled(skipButton.getSelection());
                skipLabel1.setEnabled(skipButton.getSelection());
                densitySpinner.setEnabled(densityButton.getSelection());
                densityLabel1.setEnabled(densityButton.getSelection());
            }
        });
        densityButton.setText("Density");
        densityButton.setToolTipText("Relative density (0-100) of points regardless of zoom level");

        //TODO:  As enhancement, allow separate values for rows and columns, optionally linked...
        densitySpinner = new Spinner(topComp, SWT.BORDER);
        densitySpinner.setToolTipText("Density this many rows and columns between displayed ones");
        fd = new FormData();
        fd.left = new FormAttachment(densityButton, 8, SWT.RIGHT);
        fd.top = new FormAttachment(densityButton, 0, SWT.CENTER);
        densitySpinner.setLayoutData(fd);
        densitySpinner.setDigits(0);
        densitySpinner.setMinimum(0);
        densitySpinner.setMaximum(99);
        densitySpinner.setIncrement(1);
        densitySpinner.setPageIncrement(1);
        densitySpinner.setSelection(((Integer) densityValueAttr.getAttrValue()).intValue());
        densitySpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                densityValueAttr.setAttrValue(new Integer(densitySpinner.getSelection()));
            }
        });
        densitySpinner.setEnabled(densityButton.getSelection());

        densityLabel1 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(densitySpinner, 8, SWT.RIGHT);
        fd.top = new FormAttachment(densityButton, 0, SWT.CENTER);
        densityLabel1.setLayoutData(fd);
        densityLabel1.setText("(1-99) regardless of zoom level");
        densityLabel1.setEnabled(densityButton.getSelection());

        //  Time Stamp

        timeStampButton = new Button(topComp, SWT.CHECK);
        fd = new FormData();
        fd.left = new FormAttachment(8, 0);
        fd.top = new FormAttachment(densityButton, 30, SWT.BOTTOM);
        timeStampButton.setLayoutData(fd);
        timeStampButton.setSelection(((Boolean) timeStampEnableAttr.getAttrValue()).booleanValue());
        timeStampButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                timeStampEnableAttr.setAttrValue(new Boolean(timeStampButton.getSelection()));
                timeStampLabel1.setEnabled(timeStampButton.getSelection());
                timeStampIntervalSpinner.setEnabled(timeStampButton.getSelection());
                timeStampColorComp.setEnabled(timeStampButton.getSelection());
                timeStampLineWidthSpinner.setEnabled(timeStampButton.getSelection());
                timeStampLabel2.setEnabled(timeStampButton.getSelection());
            }
        });
        timeStampButton.setText("Time Stamp");
        timeStampButton.setToolTipText("Draw lines at specified time intervals across track");

        timeStampLabel1 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(timeStampButton, 8, SWT.RIGHT);
        fd.top = new FormAttachment(timeStampButton, 0, SWT.CENTER);
        timeStampLabel1.setLayoutData(fd);
        timeStampLabel1.setText(" every");
        timeStampLabel1.setEnabled(timeStampButton.getSelection());

        timeStampIntervalSpinner = new Spinner(topComp, SWT.BORDER);
        timeStampIntervalSpinner.setToolTipText("Minutes between time stamps/lines");
        fd = new FormData();
        fd.left = new FormAttachment(timeStampLabel1, 8, SWT.RIGHT);
        fd.top = new FormAttachment(timeStampButton, 0, SWT.CENTER);
        timeStampIntervalSpinner.setLayoutData(fd);
        timeStampIntervalSpinner.setDigits(0);
        timeStampIntervalSpinner.setMinimum(1);
        timeStampIntervalSpinner.setMaximum(60);
        timeStampIntervalSpinner.setIncrement(1);
        timeStampIntervalSpinner.setPageIncrement(1);
        timeStampIntervalSpinner.setSelection(((Integer) timeStampIntervalAttr.getAttrValue()).intValue());
        timeStampIntervalSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                timeStampIntervalAttr.setAttrValue(new Integer(timeStampIntervalSpinner.getSelection()));
            }
        });
        timeStampIntervalSpinner.setEnabled(timeStampButton.getSelection());

        timeStampLabel2 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(timeStampIntervalSpinner, 8, SWT.RIGHT);
        fd.top = new FormAttachment(timeStampIntervalSpinner, 0, SWT.CENTER);
        timeStampLabel2.setLayoutData(fd);
        timeStampLabel2.setText("minutes");
        timeStampLabel2.setEnabled(timeStampButton.getSelection());

        timeStampColorComp = new Composite(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(timeStampLabel2, 28, SWT.RIGHT);
        fd.top = new FormAttachment(timeStampButton, 0, SWT.CENTER);
        timeStampColorComp.setLayoutData(fd);
        timeStampColorComp.setToolTipText("Color for time stamps and associated lines");
        GridLayout gl = new GridLayout();
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        timeStampColorComp.setLayout(gl);
        timeStampColorSelector = new ColorButtonSelector(timeStampColorComp, 50, 25);
        timeStampColorSelector.setColorValue(((RGB) timeStampColorAttr.getAttrValue()));
        timeStampColorSelector.addListener(new IPropertyChangeListener() {
            // forward the property change of the color selector
            public void propertyChange(PropertyChangeEvent event) {
                timeStampColorAttr.setAttrValue(event.getNewValue());
            }
        });
        timeStampColorComp.setEnabled(timeStampButton.getSelection());

        timeStampLabel3 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(timeStampColorComp, 0, SWT.CENTER);
        fd.top = new FormAttachment(timeStampColorComp, -20, SWT.TOP);
        timeStampLabel3.setLayoutData(fd);
        timeStampLabel3.setText("Color");

        timeStampLineWidthSpinner = new Spinner(topComp, SWT.BORDER);
        timeStampLineWidthSpinner.setToolTipText("Width of cross-track line drawn at each time stamp");
        fd = new FormData();
        fd.left = new FormAttachment(timeStampColorComp, 32, SWT.RIGHT);
        fd.top = new FormAttachment(timeStampButton, 0, SWT.CENTER);
        timeStampLineWidthSpinner.setLayoutData(fd);
        timeStampLineWidthSpinner.setDigits(0);
        timeStampLineWidthSpinner.setMinimum(0);
        timeStampLineWidthSpinner.setMaximum(10);
        timeStampLineWidthSpinner.setIncrement(1);
        timeStampLineWidthSpinner.setPageIncrement(1);
        timeStampLineWidthSpinner.setSelection(((Integer) timeStampLineWidthAttr.getAttrValue()).intValue());
        timeStampLineWidthSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                timeStampLineWidthAttr.setAttrValue(new Integer(timeStampLineWidthSpinner.getSelection()));
            }
        });
        timeStampLineWidthSpinner.setEnabled(timeStampButton.getSelection());

        timeStampLabel4 = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(timeStampLineWidthSpinner, 0, SWT.CENTER);
        fd.bottom = new FormAttachment(timeStampLineWidthSpinner, -3, SWT.TOP);
        timeStampLabel4.setLayoutData(fd);
        timeStampLabel4.setText("Line Width");

        //  Color Bar - Main

        colorBarGrp1 = new Group(topComp, SWT.NONE);
        colorBarGrp1.setText("Edit Main Color Bar");
        fd = new FormData();
        fd.left = new FormAttachment(0, 15);
        fd.right = new FormAttachment(100, -15);
        fd.top = new FormAttachment(timeStampButton, 30, SWT.BOTTOM);
        colorBarGrp1.setLayoutData(fd);

        colorBarGrp1.setLayout(new FormLayout());

        //editedColorBar1 = null;

        editedColorBar1 = (ColorBar) colorBarAttr01.getAttrValue();

        colorBarEditor1 = new ColorBarEditor(colorBarGrp1, editedColorBar1, true);

        //  Color Bar - Alternate

        colorBarGrp2 = new Group(topComp, SWT.NONE);
        colorBarGrp2.setText("Edit ALTERNATE Color Bar");
        fd = new FormData();
        fd.left = new FormAttachment(0, 15);
        fd.right = new FormAttachment(100, -15);
        fd.top = new FormAttachment(timeStampButton, 30, SWT.BOTTOM);
        colorBarGrp2.setLayoutData(fd);

        colorBarGrp2.setLayout(new FormLayout());

        //editedColorBar2 = null;

        editedColorBar2 = (ColorBar) colorBarAttr02.getAttrValue();

        colorBarEditor2 = new ColorBarEditor(colorBarGrp2, editedColorBar2, false);

        colorBarGrp2.setVisible(false);

        //  Label to introduce options to include flagged data points

        flagInclusionLabel = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(6, 0);
        fd.top = new FormAttachment(colorBarGrp1, 16, SWT.BOTTOM);
        flagInclusionLabel.setLayoutData(fd);
        flagInclusionLabel.setText("Display Even Points Flagged As ...");

        //  High Wind Speed Option

        highWindSpeedButton = new Button(topComp, SWT.CHECK);
        highWindSpeedButton.setText("High Wind Speed");
        highWindSpeedButton.setToolTipText("Show wind vectors even for points flagged as high wind speed");
        fd = new FormData();
        fd.left = new FormAttachment(10, 0);
        fd.top = new FormAttachment(flagInclusionLabel, 16, SWT.BOTTOM);
        highWindSpeedButton.setLayoutData(fd);
        highWindSpeedButton.setSelection(((Boolean) highWindSpeedEnableAttr.getAttrValue()).booleanValue());
        highWindSpeedButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                highWindSpeedEnableAttr.setAttrValue(new Boolean(highWindSpeedButton.getSelection()));
            }
        });

        //  Low Wind Speed Option

        lowWindSpeedButton = new Button(topComp, SWT.CHECK);
        lowWindSpeedButton.setText("Low Wind Speed");
        lowWindSpeedButton.setToolTipText("Show wind vectors even for points flagged as low wind speed");
        fd = new FormData();
        fd.left = new FormAttachment(10, 0);
        fd.top = new FormAttachment(highWindSpeedButton, 16, SWT.BOTTOM);
        lowWindSpeedButton.setLayoutData(fd);
        lowWindSpeedButton.setSelection(((Boolean) lowWindSpeedEnableAttr.getAttrValue()).booleanValue());
        lowWindSpeedButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                lowWindSpeedEnableAttr.setAttrValue(new Boolean(lowWindSpeedButton.getSelection()));
            }
        });

        //  Availability or Redundancy Flag Option

        boolean isInAscatFamily = EnumSet.range(NcscatMode.ASCAT, NcscatMode.EXASCT_HI).contains(ncscatMode);
        availRedunFlagButton = new Button(topComp, SWT.CHECK);
        availRedunFlagButton.setText(isInAscatFamily ? "Redundant" : "Some Data Unavailable");
        availRedunFlagButton.setToolTipText("Show wind vectors even for points flagged " + (isInAscatFamily ? "as redundant data" : "some data unavailable"));
        fd = new FormData();
        fd.left = new FormAttachment(10, 0);
        fd.top = new FormAttachment(lowWindSpeedButton, 16, SWT.BOTTOM);
        fd.bottom = new FormAttachment(100, -20);
        availRedunFlagButton.setLayoutData(fd);
        availRedunFlagButton.setSelection(((Boolean) availabilityFlagEnableAttr.getAttrValue()).booleanValue());
        availRedunFlagButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                availabilityFlagEnableAttr.setAttrValue(new Boolean(availRedunFlagButton.getSelection()));
            }
        });

        if (ncscatMode == NcscatMode.WSCAT) {
            highWindSpeedButton.setVisible(false);
            lowWindSpeedButton.setVisible(false);
            availRedunFlagButton.setVisible(false);

        }

        //  Rain Flag or QC Flag Option

        rainQcFlagButton = new Button(topComp, SWT.CHECK);
        rainQcFlagButton.setText(isInAscatFamily ? "Quality Control Fail ..." : "Rained ...");
        rainQcFlagButton.setToolTipText("Show wind vectors even for points marked " + (isInAscatFamily ? "quality-control failure" : "rain flag"));
        fd = new FormData();
        fd.left = new FormAttachment(44, 0);
        fd.top = new FormAttachment(flagInclusionLabel, 16, SWT.BOTTOM);
        rainQcFlagButton.setLayoutData(fd);
        rainQcFlagButton.setSelection(((Boolean) rainFlagEnableAttr.getAttrValue()).booleanValue());
        rainQcFlagButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                rainFlagEnableAttr.setAttrValue(new Boolean(rainQcFlagButton.getSelection()));
                use2ndColorForRainQcButton.setEnabled(rainQcFlagButton.getSelection());
                plotCirclesForRainQcButton.setEnabled(rainQcFlagButton.getSelection());
                if (!rainQcFlagButton.getSelection()) {
                    edit2ndColorButton.setSelection(false);
                    colorBarGrp1.setVisible(true);
                    colorBarGrp2.setVisible(false);
                }
                edit2ndColorButton.setEnabled(rainQcFlagButton.getSelection() && use2ndColorForRainQcButton.getSelection());
                edit2ndColorLabel.setEnabled(rainQcFlagButton.getSelection() && use2ndColorForRainQcButton.getSelection());
            }
        });

        //  Use Alternate Color for Rain or QC Flagged Points Option

        use2ndColorForRainQcButton = new Button(topComp, SWT.CHECK);
        use2ndColorForRainQcButton.setText("...using Alternate Colors");
        use2ndColorForRainQcButton.setToolTipText("Use alternate color for wind vectors marked " + (isInAscatFamily ? "quality-control failure" : "rain flag"));
        fd = new FormData();
        fd.left = new FormAttachment(48, 0);
        fd.top = new FormAttachment(rainQcFlagButton, 6, SWT.BOTTOM);
        use2ndColorForRainQcButton.setLayoutData(fd);
        use2ndColorForRainQcButton.setSelection(((Boolean) use2ndColorForRainEnableAttr.getAttrValue()).booleanValue());
        use2ndColorForRainQcButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                use2ndColorForRainEnableAttr.setAttrValue(new Boolean(use2ndColorForRainQcButton.getSelection()));
                if (!use2ndColorForRainQcButton.getSelection()) {
                    edit2ndColorButton.setSelection(false);
                    colorBarGrp1.setVisible(true);
                    colorBarGrp2.setVisible(false);
                }
                edit2ndColorButton.setEnabled(use2ndColorForRainQcButton.getSelection());
                edit2ndColorLabel.setEnabled(use2ndColorForRainQcButton.getSelection());
            }
        });
        use2ndColorForRainQcButton.setEnabled(rainQcFlagButton.getSelection());

        //  Edit Alternate Color (for Rain or QC Flagged Points, if enabled)

        edit2ndColorLabel = new Label(topComp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(use2ndColorForRainQcButton, 2, SWT.RIGHT);
        fd.top = new FormAttachment(use2ndColorForRainQcButton, 0, SWT.CENTER);
        edit2ndColorLabel.setLayoutData(fd);
        edit2ndColorLabel.setText("----");
        edit2ndColorLabel.setEnabled(use2ndColorForRainQcButton.getSelection());

        edit2ndColorButton = new Button(topComp, SWT.TOGGLE);
        edit2ndColorButton.setText("Edit Colors");
        edit2ndColorButton.setToolTipText("Edit Alternate Color Bar");
        fd = new FormData();
        fd.left = new FormAttachment(edit2ndColorLabel, 3, SWT.RIGHT);
        fd.top = new FormAttachment(use2ndColorForRainQcButton, 0, SWT.CENTER);
        edit2ndColorButton.setLayoutData(fd);
        edit2ndColorButton.setSelection(false);
        edit2ndColorButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                colorBarGrp1.setVisible(!edit2ndColorButton.getSelection());
                colorBarGrp2.setVisible(edit2ndColorButton.getSelection());
            }
        });
        edit2ndColorButton.setEnabled(use2ndColorForRainQcButton.getSelection());

        //  Plot Circles for Rain or QC Flagged Points Option

        plotCirclesForRainQcButton = new Button(topComp, SWT.CHECK);
        plotCirclesForRainQcButton.setText("...with Circles");
        plotCirclesForRainQcButton.setToolTipText("Draw circles at base of wind vectors marked " + (isInAscatFamily ? "quality-control failure" : "rain flag"));
        fd = new FormData();
        fd.left = new FormAttachment(48, 0);
        fd.top = new FormAttachment(use2ndColorForRainQcButton, 6, SWT.BOTTOM);
        plotCirclesForRainQcButton.setLayoutData(fd);
        plotCirclesForRainQcButton.setSelection(((Boolean) plotCirclesForRainEnableAttr.getAttrValue()).booleanValue());
        plotCirclesForRainQcButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                plotCirclesForRainEnableAttr.setAttrValue(new Boolean(plotCirclesForRainQcButton.getSelection()));
            }
        });
        plotCirclesForRainQcButton.setEnabled(rainQcFlagButton.getSelection());

        return topComp;
    }

    @Override
    public void initWidgets() {
        // done in createDialog
    }

    @Override
    protected void dispose() {
        super.dispose();
        colorBarEditor1.dispose();
        colorBarEditor2.dispose();
    }

}
