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

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetOpacityDialog extends CaveJFACEDialog {

    public static interface IOpacityChangedListener {
        public void opacityChanged(float opacity);
    }

    private ListenerList listenerList;

    private float originalOpacity;

    private float opacity;

    private RGB color;

    public SetOpacityDialog(Shell parentShell, float originalOpacity, RGB color) {
        super(parentShell);
        this.opacity = this.originalOpacity = originalOpacity;
        this.color = color;
        this.listenerList = new ListenerList();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Set Opacity");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) comp.getLayout();
        layout.numColumns = 1;
        layout.makeColumnsEqualWidth = false;

        final Canvas canvas = new Canvas(comp, SWT.BORDER);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = 50;
        canvas.setLayoutData(layoutData);
        canvas.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                repaintCanvas(e);
            }

        });

        Scale scale = new Scale(comp, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.widthHint = 200;
        scale.setLayoutData(layoutData);
        scale.setMinimum(0);
        scale.setMaximum(100);

        scale.setSelection((int) (originalOpacity * 100));

        final Label label = new Label(comp, SWT.CENTER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GC gc = new GC(label.getDisplay());
        gc.setFont(label.getFont());
        layoutData.widthHint = gc.stringExtent("100%").x;
        gc.dispose();
        label.setLayoutData(layoutData);
        label.setText(scale.getSelection() + "%");

        scale.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Scale scale = (Scale) e.widget;
                label.setText(scale.getSelection() + "%");
                opacity = scale.getSelection() / 100.0f;
                canvas.redraw();

                fireListeners(opacity);
            }
        });

        return comp;
    }

    private void repaintCanvas(PaintEvent e) {
        Canvas canvas = (Canvas) e.widget;
        GC gc = e.gc;
        Rectangle rect1 = canvas.getClientArea();
        gc.setBackground(gc.getDevice().getSystemColor(SWT.COLOR_BLACK));
        gc.fillRectangle(rect1);

        int dx = rect1.width / 5;
        int dy = rect1.height / 5;

        Rectangle rect2 = new Rectangle(rect1.x + dx, rect1.y + dy, rect1.width
                - 2 * dx, rect1.height - 2 * dy);
        gc.setForeground(gc.getDevice().getSystemColor(SWT.COLOR_WHITE));
        gc.drawRectangle(rect2);

        Color color = new Color(gc.getDevice(), this.color);
        gc.setBackground(color);
        gc.setAlpha((int) (opacity * 255));
        gc.fillRectangle(rect1);
        color.dispose();
    }

    @Override
    protected void cancelPressed() {
        opacity = originalOpacity;
        fireListeners(opacity);
        super.cancelPressed();
    }

    /**
     * Add listener to be notified when opacity is changed
     * 
     * @param listener
     */
    public void addOpacityChangedListener(IOpacityChangedListener listener) {
        this.listenerList.add(listener);
    }

    /**
     * Remove opacity changed listener
     * 
     * @param listener
     */
    public void removeOpacityChangedListener(IOpacityChangedListener listener) {
        this.listenerList.remove(listener);
    }

    private void fireListeners(float opacity) {
        for (Object listener : listenerList.getListeners()) {
            ((IOpacityChangedListener) listener).opacityChanged(opacity);
        }
    }

    /**
     * Get selected opacity
     * 
     * @return the opacity
     */
    public float getOpacity() {
        return opacity;
    }
}
