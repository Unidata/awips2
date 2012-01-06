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

package com.raytheon.viz.ui.dialogs;

import java.text.DecimalFormat;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.ui.dialogs.colordialog.ColorUtil;

/**
 * Provides an interface to modify the colormap parameters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 5, 2007              chammack    Initial Creation.
 * Aug 20, 2008			    dglazesk	Updated for the new ColorMap interface
 * Oct 31, 2010             ryu         use Text widgets for alternative entry
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ColormapDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColormapDialog.class);

    private String title;

    private Button okButton;

    private ColorMapCapability cap;

    private ColorMapParameters cmap;

    private Scale minSlider;

    private Scale maxSlider;

    private Text minValueText;

    private Text maxValueText;

    private String[] sliderText;

    private float cmapWidth;

    private float cmapIncrement;

    private float cmapMin;

    private float cmapMax;

    DecimalFormat format = null;

    private float currentMin;

    private float currentMax;

    public ColormapDialog(Shell parentShell, String dialogTitle,
            ColorMapCapability cap) {
        this(parentShell, dialogTitle, cap, 0);
    }

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ColormapDialog(Shell parentShell, String dialogTitle,
            ColorMapCapability cap, int precision) {
        super(parentShell);
        this.title = dialogTitle;
        this.cap = cap;
        this.cmap = cap.getColorMapParameters();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        super.buttonPressed(buttonId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        okButton = createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.OK_LABEL, false);
        okButton.setVisible(true);

        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(final Composite parent) {
        buildColorMapData();
        
        Composite composite = (Composite) super.createDialogArea(parent);

        Composite header = new Composite(composite, SWT.NONE);
        header.setLayout(new GridLayout(2, false));
        header.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label label = new Label(header, SWT.BOLD);
        label.setText("Colormap: ");
        GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        label.setLayoutData(gd);

        new ColormapComp(header, cmap, cap);

        Group minMaxValues = new Group(composite, SWT.SHADOW_ETCHED_IN);
        minMaxValues.setLayout(new GridLayout(3, false));
        minMaxValues.setText("Colormap Range:");
        minMaxValues.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                true));

        Label maxLabel = new Label(minMaxValues, SWT.None);
        maxLabel.setText("Max:");

        maxSlider = new Scale(minMaxValues, SWT.HORIZONTAL);
        maxSlider.setMaximum(255);
        maxSlider.setMinimum(0);
        maxSlider.setIncrement(1);
        maxSlider.setSelection(maxSlider.getMaximum());
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.minimumWidth = 250;
        maxSlider.setLayoutData(layoutData);

        GridData labelLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false,
                false);
        labelLayoutData.widthHint = convertWidthInCharsToPixels(10);
        maxValueText = new Text(minMaxValues, SWT.SINGLE | SWT.BORDER
                | SWT.RIGHT);
        maxValueText.setLayoutData(labelLayoutData);

        Label minLabel = new Label(minMaxValues, SWT.None);
        minLabel.setText("Min:");

        minSlider = new Scale(minMaxValues, SWT.HORIZONTAL);
        minSlider.setMaximum(255);
        minSlider.setMinimum(0);
        minSlider.setIncrement(1);
        minSlider.setSelection(minSlider.getMinimum());
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.minimumWidth = 250;
        minSlider.setLayoutData(layoutData);

        labelLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        labelLayoutData.widthHint = convertWidthInCharsToPixels(10);
        minValueText = new Text(minMaxValues, SWT.SINGLE | SWT.BORDER
                | SWT.RIGHT);
        minValueText.setLayoutData(labelLayoutData);

        maxSlider.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (maxSlider.getSelection() <= minSlider.getSelection()) {
                    maxSlider.setSelection(minSlider.getSelection() + 1);
                }
                maxValueText.setText(selectionToText(maxSlider.getSelection()));
                changeMax(maxSlider.getSelection());
            }

        });

        minSlider.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (minSlider.getSelection() >= maxSlider.getSelection()) {
                    minSlider.setSelection(maxSlider.getSelection() - 1);
                }
                minValueText.setText(selectionToText(minSlider.getSelection()));
                changeMin(minSlider.getSelection());
            }

        });

        maxValueText.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(KeyEvent e) {
                if (e.character == SWT.CR) {
                    maxTextChanged();
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
                // do nothing

            }

        });

        minValueText.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(KeyEvent e) {
                if (e.character == SWT.CR) {
                    minTextChanged();
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
                // do nothing
            }

        });

        // set initial values
        currentMax = cmap.getColorMapMax();
        currentMin = cmap.getColorMapMin();
        maxSlider.setSelection(cmapToSelection(currentMax));
        minSlider.setSelection(cmapToSelection(currentMin));
        setMaxText();
        setMinText();

        applyDialogFont(composite);
        return composite;
    }

    private void maxTextChanged() {
    	String text = maxValueText.getText().trim().split(" ")[0];
        try {
            float f = Float.valueOf(text);
            UnitConverter unitConv = cmap.getImageToDisplayConverter();
            if (unitConv != null) {
                f = (float) unitConv.inverse().convert(f);
            }
            if (currentMin >= f) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Maximum of colormap range cannot be below the minimum.");
                setMaxText();
            } else if (f >= cmapMax) {
                setColorMapMax(cmapMax);
                maxSlider.setSelection(255);
                setMaxText();
            } else {
                setColorMapMax(f);
                maxSlider.setSelection(cmapToSelection(f));
            }
        } catch (NumberFormatException ex) {
            statusHandler.handle(Priority.ERROR,
                    "Maximum of colormap range cannot be parsed: " + text);
            setMaxText();
        } catch (ConversionException ex) {
            statusHandler.handle(Priority.ERROR, 
                    "Unit converter error.", ex);
            setMaxText();  
        }

    }

    private void minTextChanged() {
    	String text = minValueText.getText().trim().split(" ")[0];
        try {
            float f = Float.valueOf(text);
            UnitConverter unitConv = cmap.getImageToDisplayConverter();
            if (unitConv != null) {
                f = (float) unitConv.inverse().convert(f);
            }
            if (f >= currentMax) {
                setMinText();
                statusHandler.handle(Priority.ERROR,
                        "Minimum of colormap range cannot exceed the maximum.");
            } else if (cmapMin >= f) {
                setColorMapMin(cmapMin);
                minSlider.setSelection(0);
                setMinText();
            } else {
                setColorMapMin(f);
                minSlider.setSelection(cmapToSelection(f));
            }
        } catch (NumberFormatException ex) {
            statusHandler.handle(Priority.ERROR,
                    "Minimum of colormap range cannot be parsed: " + text);
            setMinText();
	    } catch (ConversionException ex) {
	        statusHandler.handle(Priority.ERROR,
	                "Unit converter error.", ex);
	        setMinText();
	    }
    }

    private void setColorMapMax(float f) {
        if (currentMax != f) {
            currentMax = f;
            cmap.setColorMapMax(f, true);
            cap.notifyResources();
        }
    }

    private void setColorMapMin(float f) {
        if (currentMin != f) {
            currentMin = f;
            cmap.setColorMapMin(f, true);
            cap.notifyResources();
        }
    }

    private void setMaxText() {
        maxValueText.setText(cmapToText(currentMax));
    }

    private void setMinText() {
        minValueText.setText(cmapToText(currentMin));
    }

    private void changeMax(int position) {
        // slider min and max is based on the color map, so position is the new
        // color map max
        currentMax = selectionToCmap(position);
        cmap.setColorMapMax(currentMax, true);
        cap.notifyResources();
    }

    private void changeMin(int position) {
        // slider min and max is based on the color map, so position is the new
        // color map min
        currentMin = selectionToCmap(position);
        cmap.setColorMapMin(currentMin, true);
        cap.notifyResources();
    }

    // modified from logic in ColorBar.java
    private void buildColorMapData() {
        sliderText = new String[256];
        cmapWidth = cmap.getDataMax() - cmap.getDataMin();
        cmapIncrement = cmapWidth / ColorUtil.MAX_VALUE;
        cmapMin = cmap.getDataMin();
        cmapMax = cmap.getDataMax();
        float start = cmap.getDataMin();
        String units = "";

        UnitConverter unitConv = cmap.getImageToDisplayConverter();

        Double lastVal = Double.NaN;

        // TODO: Handle piecewise pixel converts to show ranges (for radar)
        for (int i = 0; i < sliderText.length; ++i) {
            double value = start;

            // handle precision errors
            if (value > cmapMax) {
                // if the difference is .1 the increment between steps assume
                // that cmapMax is ok
                if ((value - cmapMax) < (.1 * cmapIncrement)) {
                    value = cmapMax;
                }
            }

            String textStr = "";

            if (cmap.isLogarithmic()) {
                // TODO: Handle case where min/max go from neg to pos
                if (cmap.getColorMapMax() >= 0 && cmap.getColorMapMin() >= 0) {
                    double index = (i) / ColorUtil.MAX_VALUE;
                    value = Math.pow(Math.E,
                            (Math.log(cmap.getColorMapMin()) + (index * (Math
                                    .log(cmap.getColorMapMax()) - Math.log(cmap
                                    .getColorMapMin())))));
                }
                if (format == null) {
                    format = new DecimalFormat("0.000");
                }
            }

            if (unitConv != null) {
                value = unitConv.convert(value);

                /*
                 * Check if the last value is non a number.
                 */
                if (lastVal.isNaN()) {
                    // If value is not a number then set the text to
                    // "NO DATA".
                    if (((Double) value).isNaN()) {
                        textStr = "NO DATA";
                    }
                    lastVal = value;
                } else {
                    // If value is not a number then prepend ">"
                    // to the value.
                    if (((Double) value).isNaN()) {
                        textStr = "> " + lastVal;
                    } else {
                        lastVal = value;
                    }
                }
            }

            if (format == null && new Double(value).isNaN() == false) {
                int zeros = 0;
                String val = "" + value;
                char[] vals = val.substring(val.indexOf(".") + 1).toCharArray();
                for (int j = 0; j < vals.length; ++j) {
                    if (vals[j] == '0') {
                        ++zeros;
                    } else {
                        ++zeros;
                        break;
                    }
                }
                zeros = Math.min(3, zeros);

                String f = "0.";
                for (int j = 0; j < zeros; ++j) {
                    f += "0";
                }
                format = new DecimalFormat(f);
            }

            String txt;

            /*
             * If textStr doesn't have any text then set txt to the value in the
             * value variable.
             */
            if (textStr.length() == 0) {
                txt = format.format(value);
            } else {
                txt = textStr;
            }

            if (units != null && units.length() != 0) {
                txt += " " + units;
            }

            sliderText[i] = txt;
            start += cmapIncrement;
        }
    }

    private float selectionToCmap(int selection) {
        float percentOffset = selection / 255.0f;
        float value = percentOffset * cmapWidth + cmapMin;
        return value;
    }

    private int cmapToSelection(float value) {
        int selection = (int) ((value - cmapMin) * 255.0f / cmapWidth);
        return selection;
    }

    private String selectionToText(int selection) {
        String rval = "ERR";

        if (selection > -1 && selection < sliderText.length) {
            // exact match into sliderText array
            rval = sliderText[selection];
        } else {
            statusHandler.handle(Priority.CRITICAL, "index " + selection
                    + " out of range, max " + (sliderText.length - 1));
        }
        return rval;
    }
    
    private String cmapToText(double value) {
        UnitConverter unitConv = cmap.getImageToDisplayConverter();
        String textStr = "";

        if (unitConv != null) {
            value = unitConv.convert(value);
            
            if (((Double) value).isNaN()) {
                textStr = "NO DATA";
            }
        }

        String txt;
        if (textStr.length() == 0) {
            txt = format.format(value);
        } else {
            txt = textStr;
        }

        return txt;
    }
    
}
