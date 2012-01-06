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
package com.raytheon.viz.ui.widgets;

import java.text.DecimalFormat;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;

/**
 * This class displays a composite containing a scale control and a canvas that
 * displays the value above the scale control.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 Oct 2008             lvenable    Initial creation.
 * Nov 14, 2008            randerso    reworked as a subclass of new LabeledScale class
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class LabeledDoubleScale extends LabeledScale {
    private double scale;

    private double offset;

    private double minimum = 0.0;

    private double maximum = 1.0;

    private DecimalFormat df;

    private double resolution = 0.01;

    public LabeledDoubleScale(Composite parent) {
        super(parent);

        df = new DecimalFormat();
        setFractionalDigits(1);

        updateScaleFactors();
    }

    private void updateScaleFactors() {
        setMinimum(0);
        setMaximum((int) Math.min(Integer.MAX_VALUE, Math
                .round((maximum - minimum) / resolution)));

        offset = minimum - getMinimum();
        scale = (maximum - minimum) / (getMaximum() - getMinimum());
    }

    public double getDoubleValue() {
        return getSelection() * scale + offset;
    }

    public void setDoubleValue(double doubleValue) {
        setSelection((int) Math.round((doubleValue - offset) / scale));
    }

    public void setFractionalDigits(int digits) {
        df.setMinimumFractionDigits(digits);
        df.setMaximumFractionDigits(digits);
    }

    @Override
    protected void paint(GC gc) {
        super.drawScaleValue(gc, df.format(getDoubleValue()));
    }

    public double getDoubleMinimum() {
        return minimum;
    }

    public void setDoubleMinimum(double minimum) {
        this.minimum = minimum;
        updateScaleFactors();
    }

    public double getDoubleMaximum() {
        return maximum;
    }

    public void setDoubleMaximum(double maximum) {
        this.maximum = maximum;
        updateScaleFactors();
    }

    public double getResolution() {
        return resolution;
    }

    public void setResolution(double resolution) {
        if (resolution <= 0.0) {
            throw new IllegalArgumentException(
                    "resolution must be greater than 0.0");
        }
        this.resolution = resolution;
        updateScaleFactors();
    }
}
