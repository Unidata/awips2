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

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.viz.ui.dialogs.SetOpacityDialog;

/**
 * Preference field editor for selecting the opacity of a DataStoreResource when
 * loaded as a product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class OpacityFieldEditor extends AbstractFieldEditor {
    private static final RGB COLOR = new RGB(0, 255, 255);

    protected float fOpacity;

    protected OpacityFieldEditor() {
        super();
    }

    /**
     * Creates a line opacity field editor.
     * 
     * @param name
     *            the name of the preference this field editor works on
     * @param labelText
     *            the label text of the field editor
     * @param parent
     *            the parent of the field editor's control
     */
    public OpacityFieldEditor(String name, String labelText, Composite parent) {
        super(name, labelText, parent);
    }

    @Override
    protected void init(String name, String text) {
        super.init(name, text);
        this.fOpacity = 0.35f;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#adjustForNumColumns(int)
     */
    @Override
    protected void adjustForNumColumns(int numColumns) {
        ((GridData) fButton.getLayoutData()).horizontalSpan = numColumns - 1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#getNumberOfControls()
     */
    @Override
    public int getNumberOfControls() {
        return 2;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.gisdatastore.ui.AbstractFieldEditor#open()
     */
    @Override
    protected void open() {
        SetOpacityDialog dlg = new SetOpacityDialog(fButton.getShell(),
                fOpacity, COLOR);
        if (dlg.open() == Window.OK) {
            this.fOpacity = dlg.getOpacity();
            updateImage();
        }
    }

    @Override
    protected void updateImage() {
        super.updateImage();
        fImage = new Image(fButton.getDisplay(), fExtent.x, fExtent.y);
        GC gc = new GC(fImage);

        Rectangle rect1 = fImage.getBounds();
        gc.setBackground(gc.getDevice().getSystemColor(SWT.COLOR_BLACK));
        gc.fillRectangle(rect1);

        int dx = rect1.width / 5;
        int dy = rect1.height / 5;

        Rectangle rect2 = new Rectangle(rect1.x + dx, rect1.y + dy, rect1.width
                - 2 * dx, rect1.height - 2 * dy);
        gc.setForeground(gc.getDevice().getSystemColor(SWT.COLOR_WHITE));
        gc.drawRectangle(rect2);

        Color color = new Color(gc.getDevice(), COLOR);
        gc.setBackground(color);
        gc.setAlpha((int) (fOpacity * 255));
        gc.fillRectangle(rect1);
        color.dispose();

        gc.dispose();
        fButton.setImage(fImage);
    }

    @Override
    protected void setValueFromString(String value) {
        Float opacity = StringConverter.asFloat(value);
        if (opacity == null) {
            this.fOpacity = 0.35f;
        } else {
            this.fOpacity = opacity;
        }
    }

    @Override
    protected String getStringFromValue() {
        return StringConverter.asString(fOpacity);
    }

}
