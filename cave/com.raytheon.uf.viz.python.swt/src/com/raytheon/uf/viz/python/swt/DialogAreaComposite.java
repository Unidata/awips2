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
package com.raytheon.uf.viz.python.swt;

import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.viz.python.swt.widgets.ButtonWidget;
import com.raytheon.uf.viz.python.swt.widgets.Widget;

/**
 * Generates a composite area of widgets.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 4, 2008	1164     	jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class DialogAreaComposite extends ScrolledComposite {

    private static final double MAX_HEIGHT_RATIO = 0.85;

    private static final double MAX_WIDTH_RATIO = 0.85;

    /**
     * @param parent
     * @param widgets
     * @param style
     */
    public DialogAreaComposite(Composite parent, List<Widget> widgets, int style) {
        super(parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);

        final Composite composite = new Composite(this, SWT.NONE);

        this.setContent(composite);
        this.setLayout(new GridLayout());
        // this.setLayoutData(new GridData(GridData.FILL_BOTH));

        composite.setLayout(new GridLayout());
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Iterator<Widget> wIterator = widgets.iterator();

        while (wIterator.hasNext()) {

            Widget widget = wIterator.next();

            if (widget instanceof ButtonWidget) {

                // place all ButtonWidgets in succession into a row
                Composite rowOfButtons = new Composite(composite, style);
                GridLayout gridLayout = new GridLayout(1, false);
                rowOfButtons.setLayout(gridLayout);
                widget.buildComposite(rowOfButtons, style);

                while (wIterator.hasNext()) {
                    widget = wIterator.next();
                    if (widget instanceof ButtonWidget) {
                        gridLayout.numColumns++;
                        widget.buildComposite(rowOfButtons, style);
                    } else {
                        widget.buildComposite(composite, style);
                        break;
                    }
                }

            } else {
                widget.buildComposite(composite, style);
            }

        }

        Rectangle monitorBounds = this.getDisplay().getPrimaryMonitor()
                .getBounds();
        int maxXSize = (int) (monitorBounds.width * MAX_WIDTH_RATIO);
        int maxYSize = (int) (monitorBounds.height * MAX_HEIGHT_RATIO);

        Point compositeSize = composite.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        int xSize = compositeSize.x;
        int ySize = compositeSize.y;
        if (xSize > maxXSize) {
            xSize = maxXSize;
        }
        if (ySize > maxYSize) {
            ySize = maxYSize;
        }

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = ySize;
        gd.widthHint = xSize;
        this.setLayoutData(gd);

        this.setMinSize(compositeSize);
        this.setExpandHorizontal(true);
        this.setExpandVertical(true);

        // Make sure widgets are scrolled into view when they gain focus
        // see:
        // http://www.java2s.com/Code/Java/SWT-JFace-Eclipse/ScrollSWTwidgetsintoviewwhentheygetfocus.htm
        final DialogAreaComposite sc = this;
        Listener scrollOnFocus = new Listener() {

            @Override
            public void handleEvent(Event event) {
                Control child = (Control) event.widget;
                Rectangle bounds = child.getBounds();
                Rectangle area = sc.getClientArea();
                Point origin = sc.getOrigin();
                if (origin.x > bounds.x) {
                    origin.x = Math.max(0, bounds.x);
                }
                if (origin.y > bounds.y) {
                    origin.y = Math.max(0, bounds.y);
                }
                if (origin.x + area.width < bounds.x + bounds.width) {
                    origin.x = Math
                            .max(0, bounds.x + bounds.width - area.width);
                }
                if (origin.y + area.height < bounds.y + bounds.height) {
                    origin.y = Math.max(0, bounds.y + bounds.height
                            - area.height);
                }
                sc.setOrigin(origin);
            }

        };

        Control[] controls = composite.getChildren();
        for (Control c : controls) {
            c.addListener(SWT.Activate, scrollOnFocus);
        }

    }
}
