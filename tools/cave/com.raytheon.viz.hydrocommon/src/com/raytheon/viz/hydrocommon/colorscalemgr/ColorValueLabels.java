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

package com.raytheon.viz.hydrocommon.colorscalemgr;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

/**
 * Class containing the color and value labels that will be displayed on the
 * ColorScaleMgrDlg dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ColorValueLabels {
    /**
     * Parent display (used for changing the color).
     */
    private Display parentDisplay;

    /**
     * Scale color.
     */
    private Color color;

    /**
     * RGB color value.
     */
    private RGB rgb;

    /**
     * Label displaying the scale color.
     */
    private Label colorLabel;

    /**
     * Label displaying the scale value.
     */
    private Label valueLabel;

    /**
     * Unique index ID.
     */
    private int indexID = 0;

    /**
     * Database name of color
     */
    private String colorName = "";

    /**
     * Constructor.
     * 
     * @param index
     *            Index ID.
     */
    public ColorValueLabels(int index) {
        indexID = index;
    }

    /**
     * Get the index ID.
     * 
     * @return The index ID.
     */
    public int getIndexId() {
        return indexID;
    }

    /**
     * Set the color label.
     * 
     * @param colorLbl
     *            Color label.
     */
    public void setColorLabel(Label colorLbl) {
        colorLabel = colorLbl;
    }

    /**
     * Change the label color.
     * 
     * @param rgbColor
     *            The new RGB color value.
     */
    public void changeLabelColor(RGB rgbColor) {
        rgb = rgbColor;
        colorName = DbRGBColors.getName(rgbColor);

        if (color != null) {
            color.dispose();
        }

        color = new Color(parentDisplay, rgb);
        colorLabel.setBackground(color);
    }

    /**
     * Set the value label.
     * 
     * @param valueLbl
     *            SWT label.
     */
    public void setValueLbl(Label valueLbl) {
        valueLabel = valueLbl;
    }

    /**
     * Set the value of the value label.
     * 
     * @param dblValue
     *            New value.
     */
    public void setValueLbl(double dblValue) {
        dblValue = dblValue * 100.0;

        dblValue = Math.round(dblValue) / 100.0;

        valueLabel.setText(String.valueOf(dblValue));
    }

    /**
     * Set the value of the value label.
     * 
     * @param str
     *            New value.
     */
    public void setValueLbl(String str) {
        valueLabel.setText(str);
    }

    /**
     * Check of the label value is equal to the label passed in.
     * 
     * @param lbl
     *            SWT value label.
     * @return True if the labels are equal, false otherwise.
     */
    public boolean valueLblIsEqual(Label lbl) {
        return valueLabel.equals(lbl);
    }

    /**
     * Check of the label color is equal to the label passed in.
     * 
     * @param lbl
     *            SWT color label.
     * @return True if the labels are equal, false otherwise.
     */
    public boolean colorLblIsEqual(Label lbl) {
        return colorLabel.equals(lbl);
    }

    /**
     * Dispose of the SWT labels and the color.
     */
    public void disposeLabels() {
        if (colorLabel != null) {
            colorLabel.dispose();
        }

        if (valueLabel != null) {
            valueLabel.dispose();
        }

        if (color != null) {
            color.dispose();
        }
    }

    /**
     * Get the RGB color of the color label.
     * 
     * @return
     */
    public RGB getRgbColor() {
        return rgb;
    }

    /**
     * Get the string value of the label.
     * 
     * @return The text in the value label.
     */
    public String getValueText() {
        return valueLabel.getText();
    }

    /**
     * Returns the name of the color
     * 
     * @return Name of color as stored in database
     */
    public String getColorName() {
        return colorName;
    }
}
