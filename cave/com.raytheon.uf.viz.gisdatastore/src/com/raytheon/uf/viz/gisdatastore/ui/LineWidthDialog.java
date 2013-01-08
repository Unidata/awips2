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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

/**
 * Dialog for selecting line width
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

public class LineWidthDialog extends Dialog {
    int width;

    private int min;

    private int max;

    protected LineWidthDialog(Shell parentShell, int width, int min, int max) {
        super(parentShell);
        this.width = width;
        this.min = min;
        this.max = max;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        Label label = new Label(comp, SWT.CENTER);
        label.setText("Double-Click to select");
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        Display d = comp.getDisplay();
        Table table = new Table(comp, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = table.getItemHeight()
                * Math.min(10, max - min + 1);
        table.setLayoutData(layoutData);

        for (int w = min; w <= max; w++) {
            TableItem item = new TableItem(table, SWT.NONE);
            Image image = new Image(d, 128, 10);
            Rectangle bounds = image.getBounds();
            GC gc = new GC(image);
            gc.fillRectangle(bounds);
            gc.setLineWidth(w);
            gc.drawLine(bounds.x, bounds.y + bounds.height / 2,
                    bounds.width - 1, bounds.y + bounds.height / 2);

            gc.dispose();
            item.setImage(image);
            image.dispose();
            item.setData(w);

            if (w == this.width) {
                table.setSelection(item);
            }
        }
        table.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                Table table = (Table) e.widget;
                TableItem item = table.getSelection()[0];
                width = (Integer) item.getData();
                okPressed();
            }
        });

        return comp;
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        return null;
    }

    public int getWidth() {
        return this.width;
    }
}
