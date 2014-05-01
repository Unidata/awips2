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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * Generic <code>Dialog</code> to display a multi-line block of text in a
 * re-sizable and scrollable window.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2013  #1843     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TextDisplayDlg extends CaveJFACEDialog {

    /**
     * Location of this dialog relative to it's parent.
     */
    public static enum Location {
        ABOVE, BELOW, LEFT, RIGHT, CENTERED
    }

    private String title;

    private String contents;

    private Location location;

    /**
     * @param parentShell
     * @param title
     * @param contents
     */
    public TextDisplayDlg(Shell parentShell, String title, String contents,
            Location location) {
        super(parentShell);
        this.title = title;
        this.contents = contents;
        this.location = location;

        this.setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        newShell.setText(this.title);
        super.configureShell(newShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        Text contents = new Text(top, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY
                | SWT.V_SCROLL | SWT.H_SCROLL);

        Rectangle screenBounds = this.getShell().getMonitor().getBounds();
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.widthHint = screenBounds.width / 2;
        layoutData.heightHint = screenBounds.height / 4;

        contents.setLayoutData(layoutData);
        contents.setText(this.contents);

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#getInitialLocation(org.eclipse.swt.graphics
     * .Point)
     */
    @Override
    protected Point getInitialLocation(Point initialSize) {
        Rectangle parentBounds = getParentShell().getBounds();
        Rectangle myBounds = getShell().getBounds();

        int x = parentBounds.x + ((parentBounds.width - myBounds.width) / 2);
        int y = parentBounds.y + ((parentBounds.height - myBounds.height) / 2);
        ;
        switch (this.location) {
        case ABOVE:
            y = parentBounds.y - myBounds.height;
            break;
        case BELOW:
            y = parentBounds.y + parentBounds.height;
            break;
        case LEFT:
            x = parentBounds.x - myBounds.width;
            break;
        case RIGHT:
            x = parentBounds.x + parentBounds.width;
            break;
        case CENTERED:
            break;
        }

        Point location = new Point(x, y);
        return location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        // no buttons
        return null;
    }

}
