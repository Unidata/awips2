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
import org.eclipse.swt.widgets.Composite;

/**
 * Preference field editor for selecting a line width for DataStoreResource
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

public class LineWidthFieldEditor extends AbstractFieldEditor {
    private static final int MAX_WIDTH = 5;

    private static final int MIN_WIDTH = 1;

    private static int DEFAULT_WIDTH = 2;

    private int fWidth;

    /**
     * 
     */
    public LineWidthFieldEditor() {
        super();
    }

    /**
     * @param name
     * @param labelText
     * @param parent
     */
    public LineWidthFieldEditor(String name, String labelText, Composite parent) {
        super(name, labelText, parent);
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
        // gc.setBackground(fButton.getBackground());
        gc.fillRectangle(0, 0, fExtent.x, fExtent.y);
        int y = fExtent.y / 2;
        gc.setLineWidth(this.fWidth);
        gc.drawLine(0, y, fExtent.x, y);
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
        LineWidthDialog dlg = new LineWidthDialog(fButton.getShell(), fWidth,
                MIN_WIDTH, MAX_WIDTH);
        if (dlg.open() == Window.OK) {
            this.fWidth = dlg.getWidth();
            updateImage();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.gisdatastore.ui.AbstractFieldEditor#setValueFromString
     * (java.lang.String)
     */
    @Override
    protected void setValueFromString(String value) {
        int width = MIN_WIDTH;
        Integer i = StringConverter.asInteger(value);
        if (i != null) {
            width = i.intValue();
        }

        if (width < MIN_WIDTH) {
            this.fWidth = MIN_WIDTH;
        } else if (width > MAX_WIDTH) {
            this.fWidth = MAX_WIDTH;
        } else {
            this.fWidth = width;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.gisdatastore.ui.AbstractFieldEditor#getStringFromValue
     * ()
     */
    @Override
    protected String getStringFromValue() {
        return StringConverter.asString(this.fWidth);
    }

}
