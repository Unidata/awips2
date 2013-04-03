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
package com.raytheon.uf.viz.gisdatastore.ui;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.RGBColors;

/**
 * Preference field editor for selecting a color for the DataStoreResource
 * highlight
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 27, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class ColorFieldEditor extends AbstractFieldEditor {
    private static final RGB DEFAULT_COLOR = RGBColors.getRGBColor("HotPink");

    private RGB fColor;

    /**
     * 
     */
    public ColorFieldEditor() {
        super();
    }

    /**
     * @param name
     * @param labelText
     * @param parent
     */
    public ColorFieldEditor(String name, String labelText, Composite parent) {
        super(name, labelText, parent);
    }

    @Override
    protected void init(String name, String text) {
        super.init(name, text);
        this.fColor = DEFAULT_COLOR;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.gisdatastore.ui.AbstractFieldEditor#updateImage()
     */
    @Override
    protected void updateImage() {
        super.updateImage();
        fImage = new Image(fButton.getDisplay(), fExtent.x, fExtent.y);
        GC gc = new GC(fImage);
        Color color = new Color(fButton.getDisplay(), this.fColor);
        gc.setBackground(color);
        gc.fillRectangle(0, 0, fExtent.x, fExtent.y);
        color.dispose();
        gc.dispose();
        fButton.setImage(fImage);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.gisdatastore.ui.AbstractFieldEditor#open()
     */
    @Override
    protected void open() {
        ColorDialog colorDialog = new ColorDialog(fButton.getShell());
        colorDialog.setRGB(fColor);
        RGB newColor = colorDialog.open();
        if (newColor != null) {
            this.fColor = newColor;
            updateImage();
        }
    }

    @Override
    protected void setValueFromString(String value) {
        RGB newColor = StringConverter.asRGB(value);
        if (newColor == null) {
            newColor = DEFAULT_COLOR;
        }
        this.fColor = newColor;
        updateImage();
    }

    @Override
    protected String getStringFromValue() {
        return StringConverter.asString(this.fColor);
    }
}
