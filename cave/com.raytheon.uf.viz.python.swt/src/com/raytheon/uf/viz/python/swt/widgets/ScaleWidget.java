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
package com.raytheon.uf.viz.python.swt.widgets;

import java.text.DecimalFormat;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;

/**
 * This widget displays a horizontal scale.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 * Jun 04, 2008  1164     jelkins         Initial creation
 * Dec 11, 2014  638      mgamazaychikov  Add isFloat to explicitly specify the
 *                                        type of return value be either Double
 *                                        or Integer.
 * Nov 28, 2017  6540     randerso        Change Float references to Double for
 *                                        compatibility with Jep 3.6
 * Jan 15, 2018  6684     randerso        Added support for deltaScale
 *
 * </pre>
 *
 * @author jelkins
 */

public class ScaleWidget extends Widget {

    private Scale scale;

    private Label deltaLabel;

    private double minValue;

    private long range;

    private double resolution;

    private int precision;

    private boolean isFloat = false;

    private double initialValue;

    private boolean delta;

    /**
     * Class constructor specifying this scale's label.
     *
     * @param label
     *            the label to assign to this scale.
     * @param delta
     *            true if scale should display delta value
     */
    public ScaleWidget(String label, boolean delta) {
        this();
        setLabel(label);
        this.delta = delta;
    }

    /**
     * Default constructor
     */
    protected ScaleWidget() {
        super();

        range = 100;
        resolution = 1;
        precision = 0;
        delta = false;
    }

    @Override
    public Composite buildComposite(Composite parent) {

        Group group = new Group(parent, SWT.NONE);
        group.setText(makeGuiLabel(getLabel()));

        GridLayout layout = new GridLayout(1, false);
        layout.numColumns = (delta ? 2 : 1);
        group.setLayout(layout);
        group.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label label = new Label(group, SWT.CENTER);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        // dummy label to keep label centered over scale
        if (delta) {
            new Label(group, SWT.CENTER);
        }

        scale = new Scale(group, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.widthHint = (delta ? 200 : 300);
        scale.setLayoutData(layoutData);

        if (delta) {
            deltaLabel = new Label(group, SWT.RIGHT);
        }

        minValue = ((Number) (getOptions().get(0))).doubleValue();
        double maxValue = ((Number) (getOptions().get(1))).doubleValue();

        range = Math.round((maxValue - minValue) / getResolution());

        DecimalFormat format = new DecimalFormat();
        int p = (int) Math.ceil(-Math.log10(resolution));
        if (precision < p) {
            precision = p;
        }
        format.setMaximumFractionDigits(precision);
        format.setMinimumFractionDigits(precision);

        DecimalFormat deltaFormat = (delta ? (DecimalFormat) format.clone()
                : null);
        if (delta) {
            deltaFormat.setPositivePrefix("(+");
            deltaFormat.setPositiveSuffix(")");
            deltaFormat.setNegativePrefix("(-");
            deltaFormat.setNegativeSuffix(")");

            layoutData = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
            GC gc = new GC(deltaLabel);
            layoutData.widthHint = gc
                    .textExtent(deltaFormat.format(maxValue - minValue)).x;
            gc.dispose();
            deltaLabel.setLayoutData(layoutData);
        }

        scale.setMinimum(0);
        scale.setMaximum((int) range);
        scale.setIncrement(1);
        scale.setPageIncrement(1);

        if (getValue() == null) {
            if (isFloat()) {
                setValue(new Float(minValue));
            } else {
                setValue(new Integer((int) minValue));
            }
        }

        double value = ((Number) getValue()).doubleValue();
        setInitialScaleValue(value);
        label.setText(format.format(getScaleValue()));

        if (delta) {
            deltaLabel.setText(deltaFormat.format(value - initialValue));
        }

        scale.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                double value = getScaleValue();

                if (isFloat()) {
                    setValue(new Double(value));
                } else {
                    setValue(new Integer((int) value));
                }

                label.setText(format.format(value));
                if (delta) {
                    deltaLabel
                            .setText(deltaFormat.format(value - initialValue));
                }
            }
        });

        return group;

    }

    /**
     * @return the scaleValue
     */
    private double getScaleValue() {
        return (scale.getSelection() * resolution) + minValue;
    }

    /**
     * Sets the initial scale value
     * <p>
     * Translates the desired scaleValue onto the adjusted widget scale.
     * </p>
     *
     * @param scaleValue
     *            the scaleValue to set
     */
    private void setInitialScaleValue(double scaleValue) {
        this.initialValue = scaleValue;

        long position = Math.round((scaleValue - minValue) / resolution);
        scale.setSelection((int) position);
    }

    /**
     * @return the resolution
     */
    public double getResolution() {
        return resolution;
    }

    /**
     * Scale resolution
     * <p>
     * Default resolution is 1.
     * </p>
     *
     * @param resolution
     *            the resolution to set
     */
    public void setResolution(double resolution) {
        if (resolution != 0.0) {
            this.resolution = resolution;
        }
    }

    /**
     * @return the precision
     */
    public int getPrecision() {
        return precision;
    }

    @Override
    public String toString() {
        char Q = '\''; // quote
        char C = ','; // comma
        return super.toString().substring(0, super.toString().length() - 1) + C
                + Q + resolution + Q + ")";
    }

    /**
     * Set the minimum and maximum scale value for this widget.
     *
     * @param list
     *            of two values
     *            <ol>
     *            <li>Minimum Value</li>
     *            <li>Maximum Value</li>
     *            </ol>
     */
    @Override
    public void setOptions(List<? extends Object> list) {
        super.setOptions(list);
    }

    /**
     * Set the minimum and maximum scale value for this widget.
     *
     * @param options
     *            an array of two values. The first value is taken as the
     *            minimum value and the second value is taken as the maximum
     *            value.
     */
    @Override
    public void setOptions(Object[] options) {
        super.setOptions(options);
    }

    /**
     * @param precision
     */
    public void setPrecision(int precision) {
        this.precision = precision;
    }

    /**
     * @return true if scale is floating point
     */
    public boolean isFloat() {
        return isFloat;
    }

    /**
     * @param isFloat
     */
    public void setFloat(boolean isFloat) {
        this.isFloat = isFloat;
    }
}
