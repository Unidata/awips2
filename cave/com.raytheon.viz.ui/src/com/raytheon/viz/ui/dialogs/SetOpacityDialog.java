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

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ShadeableCapability;

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

    private AbstractVizResource<?, ?> resource;

    private float originalOpacity;

    private float opacity;

    public SetOpacityDialog(Shell parentShell, AbstractVizResource<?, ?> rsc) {
        super(parentShell);
        this.resource = rsc;
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
        opacity = originalOpacity = resource.getCapability(
                ShadeableCapability.class).getOpacity();

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
                resource.getCapability(ShadeableCapability.class).setOpacity(
                        opacity);
                resource.issueRefresh();
                canvas.redraw();
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

        Rectangle rect2 = new Rectangle(rect1.x + rect1.width / 2, rect1.y,
                rect1.width / 2, rect1.height);
        gc.setBackground(gc.getDevice().getSystemColor(SWT.COLOR_WHITE));
        gc.fillRectangle(rect2);

        RGB rgb;
        if (resource.hasCapability(ColorableCapability.class)) {
            rgb = resource.getCapability(ColorableCapability.class).getColor();
        } else {
            rgb = new RGB(0, 255, 255);
        }

        Color color = new Color(gc.getDevice(), rgb);
        gc.setBackground(color);
        gc.setAlpha((int) (opacity * 255));
        gc.fillRectangle(rect1);
        color.dispose();
    }

    @Override
    protected void cancelPressed() {
        resource.getCapability(ShadeableCapability.class).setOpacity(
                originalOpacity);
        resource.issueRefresh();
        super.cancelPressed();
    }
}
