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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;

/**
 * Composite for slider bars for ColorMapParameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan  3, 2012            mschenke    Initial creation
 * Nov  8, 2013 2492       mschenke    Rewritten to work with colormap
 *                                     units different from data units
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColorMapSliderComp extends Composite {

    private static final String NaN_STRING = "NO DATA";

    private static final int SLIDER_MIN = 0;

    private static final int SLIDER_MAX = 250;

    private static final int SLIDER_INC = 1;

    private ColorMapParameters cmap;

    private Scale minSlider;

    private Scale maxSlider;

    private Text minValueText;

    private Text maxValueText;

    private float cmapAbsoluteMin;

    private float cmapAbsoluteMax;

    private final float origCmapMin;

    private final float origCmapMax;

    private float currentCmapMin;

    private float currentCmapMax;

    private final DecimalFormat format;

    private UnitConverter displayToColorMap;

    private UnitConverter colorMapToDisplay;

    /**
     * @param parent
     * @param style
     */
    public ColorMapSliderComp(Composite parent, ColorMapParameters cmap) {
        super(parent, SWT.NONE);
        this.cmap = cmap;
        this.cmapAbsoluteMin = this.currentCmapMin = this.origCmapMin = cmap
                .getColorMapMin();
        this.cmapAbsoluteMax = this.currentCmapMax = this.origCmapMax = cmap
                .getColorMapMax();
        this.displayToColorMap = cmap.getDisplayToColorMapConverter();
        this.colorMapToDisplay = cmap.getColorMapToDisplayConverter();
        if (displayToColorMap == null) {
            displayToColorMap = Unit.ONE.getConverterTo(Unit.ONE);
        }
        if (colorMapToDisplay == null) {
            colorMapToDisplay = Unit.ONE.getConverterTo(Unit.ONE);
        }

        updateAbsolutes(cmapAbsoluteMin, cmapAbsoluteMax);

        this.format = getTextFormat();

        initializeComponents();
    }

    public void restore() {
        cmap.setColorMapMin(origCmapMin);
        cmap.setColorMapMax(origCmapMax);
    }

    /**
     * 
     */
    private void initializeComponents() {
        setLayout(new GridLayout(3, false));

        Label maxLabel = new Label(this, SWT.None);
        maxLabel.setText("Max:");

        maxSlider = new Scale(this, SWT.HORIZONTAL);
        maxSlider.setMaximum(SLIDER_MAX);
        maxSlider.setMinimum(SLIDER_MIN);
        maxSlider.setIncrement(SLIDER_INC);
        maxSlider.setSelection(maxSlider.getMaximum());
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.minimumWidth = 250;
        maxSlider.setLayoutData(layoutData);

        int width = 75;

        GridData labelLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false,
                false);
        labelLayoutData.widthHint = width;
        maxValueText = new Text(this, SWT.SINGLE | SWT.BORDER | SWT.RIGHT);
        maxValueText.setLayoutData(labelLayoutData);

        Label minLabel = new Label(this, SWT.None);
        minLabel.setText("Min:");

        minSlider = new Scale(this, SWT.HORIZONTAL);
        minSlider.setMaximum(SLIDER_MAX);
        minSlider.setMinimum(SLIDER_MIN);
        minSlider.setIncrement(SLIDER_INC);
        minSlider.setSelection(minSlider.getMinimum());
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.minimumWidth = 250;
        minSlider.setLayoutData(layoutData);

        labelLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        labelLayoutData.widthHint = width;
        minValueText = new Text(this, SWT.SINGLE | SWT.BORDER | SWT.RIGHT);
        minValueText.setLayoutData(labelLayoutData);

        maxSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setColorMapMax(selectionToColorMapValue(maxSlider
                        .getSelection()));
            }
        });


        minSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setColorMapMin(selectionToColorMapValue(minSlider
                        .getSelection()));
            }
        });

        maxValueText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.character == SWT.CR) {
                    updateMinMaxFromText(maxValueText);
                }
            }
        });

        minValueText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.character == SWT.CR) {
                    updateMinMaxFromText(minValueText);
                }
            }
        });

        setColorMapMin(currentCmapMin);
        setColorMapMax(currentCmapMax);
    }

    private void updateMinMaxFromText(Text text) {
        float newCmapValue = textToColorMapValue(text);
        if (Float.isNaN(newCmapValue)) {
            // Change nothing
            if (text == minValueText) {
                newCmapValue = currentCmapMin;
            } else {
                newCmapValue = currentCmapMax;
            }
        } else {
            // Update colormap range
            if (text == minValueText) {
                currentCmapMin = newCmapValue;
            } else {
                currentCmapMax = newCmapValue;
            }
        }

        updateAbsolutes(currentCmapMin, currentCmapMax);

        setColorMapMin(currentCmapMin);
        setColorMapMax(currentCmapMax);
    }

    /**
     * 
     */
    private void updateAbsolutes(float cmapAbsoluteMin, float cmapAbsoluteMax) {
        double displayAbsMax = colorMapToDisplay.convert(cmapAbsoluteMax);
        double displayAbsMin = colorMapToDisplay.convert(cmapAbsoluteMin);
        if (displayAbsMax < displayAbsMin) {
            float tmp = cmapAbsoluteMax;
            cmapAbsoluteMax = cmapAbsoluteMin;
            cmapAbsoluteMin = tmp;
        }

        // Add a 1/16 buffer on either side for fine tuning
        float buffer = (cmapAbsoluteMax - cmapAbsoluteMin) * .0625f;
        this.cmapAbsoluteMin = cmapAbsoluteMin - buffer;
        this.cmapAbsoluteMax = cmapAbsoluteMax + buffer;
    }

    /**
     * Converts a slider selection index to a colormap value
     * 
     * @param selection
     * @return
     */
    private float selectionToColorMapValue(int selection) {
        double indexValue = Colormapper.getLinearIndex(selection, SLIDER_MIN,
                SLIDER_MAX);
        double colorMapValue = cmapAbsoluteMin
                + (cmapAbsoluteMax - cmapAbsoluteMin) * indexValue;
        return (float) colorMapValue;
    }

    /**
     * Converts a colormap value to a slider selection index
     * 
     * @param colorMapValue
     * @return
     */
    private int colorMapValueToSelection(float colorMapValue) {
        double indexValue = Colormapper.getLinearIndex(colorMapValue,
                cmapAbsoluteMin, cmapAbsoluteMax);
        return (int) (SLIDER_MIN + (SLIDER_MAX - SLIDER_MIN) * indexValue);
    }

    /**
     * Converts a text string to a colormap value
     * 
     * @param text
     * @return
     */
    private float textToColorMapValue(Text textControl) {
        String text = textControl.getText().trim();
        if (cmap.getDataMapping() != null && text.isEmpty() == false) {
            // First check for data mapping entries
            for (DataMappingEntry entry : cmap.getDataMapping().getEntries()) {
                if (entry.getLabel() != null && text.equals(entry.getLabel())) {
                    return entry.getPixelValue().floatValue();
                }
            }
        }
        if (NaN_STRING.equals(text)) {
            // If special NaN String, try to find closest NaN value
            float currentColorMapValue = textControl == maxValueText ? currentCmapMax
                    : currentCmapMin;
            int currentSliderValue = colorMapValueToSelection(currentColorMapValue);
            int minDist = Integer.MAX_VALUE;
            float bestNanValue = currentColorMapValue;
            for (int i = SLIDER_MIN; i < SLIDER_MAX; i += SLIDER_INC) {
                float colorMapValue = selectionToColorMapValue(i);
                if (Double.isNaN(colorMapToDisplay.convert(colorMapValue))) {
                    int dist = Math.abs(i - currentSliderValue);
                    if (dist < minDist) {
                        minDist = dist;
                        bestNanValue = colorMapValue;
                    } else if (i > currentSliderValue) {
                        break;
                    }
                } else {
                    // For now, assume NaN will live on low end of slider
                    break;
                }
            }
            return bestNanValue;
        } else {
            // Attempt to parse and convert
            try {
                float displayValue = Float.parseFloat(text);
                return (float) displayToColorMap.convert(displayValue);
            } catch (Throwable t) {
                // Ignore, NaN will be returned and text will be reverted
            }
        }
        return Float.NaN;
    }

    /**
     * Converts a colormap value into a text display string
     * 
     * @param colorMapValue
     * @return
     */
    private String colorMapValueToText(float colorMapValue) {
        String text = null;
        if (cmap.getDataMapping() != null) {
            text = cmap.getDataMapping().getLabelValueForDataValue(
                    colorMapValue);
        }
        if (text == null || text.trim().isEmpty()) {
            float displayValue = (float) colorMapToDisplay
                    .convert(colorMapValue);
            if (Float.isNaN(displayValue) == false) {
                text = format.format(displayValue);
            } else {
                text = NaN_STRING;
                int selection = colorMapValueToSelection(colorMapValue);
                for (int i = selection; i >= SLIDER_MIN; i -= SLIDER_INC) {
                    displayValue = (float) colorMapToDisplay
                            .convert(selectionToColorMapValue(i));
                    if (Float.isNaN(displayValue) == false) {
                        text = "> " + format.format(displayValue);
                        break;
                    }
                }
            }
        }
        return text;
    }

    /**
     * Sets the colormap min value, updates the text and slider
     * 
     * @param colorMapMin
     */
    private void setColorMapMin(float colorMapMin) {
        if (Float.isNaN(colorMapMin) == false) {
            currentCmapMin = colorMapMin;
        }
        minSlider.setSelection(colorMapValueToSelection(currentCmapMin));
        minValueText.setText(colorMapValueToText(currentCmapMin));

        cmap.setColorMapMin(currentCmapMin, true);
    }

    /**
     * Sets the colormap max value, updates the text and slider
     * 
     * @param colorMapMax
     */
    private void setColorMapMax(float colorMapMax) {
        if (Float.isNaN(colorMapMax) == false) {
            currentCmapMax = colorMapMax;
        }
        maxSlider.setSelection(colorMapValueToSelection(currentCmapMax));
        maxValueText.setText(colorMapValueToText(currentCmapMax));

        cmap.setColorMapMax(currentCmapMax, true);
    }

    private DecimalFormat getTextFormat() {
        if (cmap.isLogarithmic() == false) {
            for (int i = SLIDER_MIN; i < SLIDER_MAX; ++i) {
                double cmapValue = selectionToColorMapValue(i);
                double displayValue = colorMapToDisplay.convert(cmapValue);
                if (Double.isNaN(displayValue)) {
                    continue;
                }

                int zeros = 0;
                String val = "" + displayValue;
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
                return new DecimalFormat(f);
            }
        }
        return new DecimalFormat("0.000");
    }

}
