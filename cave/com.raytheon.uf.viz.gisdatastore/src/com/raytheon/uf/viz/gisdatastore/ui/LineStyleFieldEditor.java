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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

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
 * Nov 26, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class LineStyleFieldEditor extends AbstractFieldEditor {

    protected LineStyle fStyle;

    protected LineStyleFieldEditor() {
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
    public LineStyleFieldEditor(String name, String labelText, Composite parent) {
        super(name, labelText, parent);
    }

    @Override
    protected void init(String name, String text) {
        super.init(name, text);
        this.fStyle = LineStyle.SOLID;
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

    @Override
    protected void open() {
        LineStyleDialog dlg = new LineStyleDialog(fButton.getShell(), fStyle);
        if (dlg.open() == Window.OK) {
            this.fStyle = dlg.getStyle();
            updateImage();
        }
    }

    @Override
    protected void updateImage() {
        super.updateImage();
        fImage = new Image(fButton.getDisplay(), fExtent.x, fExtent.y);
        GC gc = new GC(fImage);
        // gc.setBackground(fButton.getBackground());
        gc.fillRectangle(0, 0, fExtent.x, fExtent.y);
        int y = fExtent.y / 2;
        gc.setLineDash(this.fStyle.getSWTLineStyle());
        gc.drawLine(0, y, fExtent.x, y);
        gc.dispose();
        fButton.setImage(fImage);
    }

    @Override
    protected void setValueFromString(String value) {
        LineStyle style = StringConverter.asLineStyle(value);
        if (style == null) {
            this.fStyle = LineStyle.SOLID;
        } else {
            this.fStyle = style;
        }
    }

    @Override
    protected String getStringFromValue() {
        return StringConverter.asString(fStyle);
    }
}
