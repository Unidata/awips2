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
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 4, 2008	1164			jelkins	Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class ScaleWidget extends Widget {

    private Label label;

    private Scale scale;

    private float minValue;

    private float maxValue;

    private int range;

    private float resolution;
    
    private int precision;

    private DecimalFormat format;

    /**
     * Class constructor specifying this scale's label.
     * 
     * @param label
     *            the label to assign to this scale.
     */
    public ScaleWidget(String label) {
        this();
        setLabel(label);
    }

    /**
     * Class constructor
     * <p>
     * This scale is
     * </p>
     */
    protected ScaleWidget() {
        super();

        range = 100;
        resolution = 1;
        precision = 3;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.runtimeui.widgets.Widget#buildComposite(org.eclipse
     * .swt.widgets.Composite, int)
     */
    @Override
    public Composite buildComposite(Composite parent) {

        Group group = new Group(parent, SWT.NONE);
        group.setText(makeGuiLabel(getLabel()));

        group.setLayout(new GridLayout());
        group.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        label = new Label(group, SWT.CENTER);
        label.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        scale = new Scale(group, SWT.HORIZONTAL);
        GridData layoutData = new GridData(300, SWT.DEFAULT);
        scale.setLayoutData(layoutData);

        minValue = ((Number) (getOptions().get(0))).floatValue();
        maxValue = ((Number) (getOptions().get(1))).floatValue();

        range = Math.round((maxValue - minValue) / getResolution());

        format = new DecimalFormat();
        format.setMaximumFractionDigits(precision);
        
        scale.setMinimum(0);
        scale.setMaximum(range);
        scale.setIncrement(1);
        scale.setPageIncrement(1);

        if (getValue() == null) {
        	setValue(new Float(minValue));
        }

        setInitialScaleValue(((Number) (getValue())).floatValue());

        label.setText(format.format(getScaleValue()));

        scale.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                float value = getScaleValue();

                label.setText(format.format(value));
                setValue(new Float(value));
            }
        });

        return group;

    }

    /**
     * @return the scaleValue
     */
    private float getScaleValue() {
    	return scale.getSelection() * resolution + minValue;
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
    private void setInitialScaleValue(float scaleValue) {

        int position = Math.round((scaleValue - minValue) / resolution);
        scale.setSelection(position);
    }

    /**
     * @return the resolution
     */
    public float getResolution() {
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
    public void setResolution(float resolution) {
        if (resolution != 0.0f) {
            this.resolution = resolution;
        }
    }

    public int getPrecision() {
        return precision;
	}

	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.runtimeui.widgets.Widget#toString()
     */
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

    public void setPrecision(int precision) {
        this.precision = precision;
    }

}
