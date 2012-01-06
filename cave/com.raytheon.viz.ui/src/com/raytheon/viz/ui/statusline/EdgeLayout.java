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
package com.raytheon.viz.ui.statusline;

import java.util.LinkedList;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.statusline.EdgeLayout.EdgeLayoutData.EdgeAffinity;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 15, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class EdgeLayout extends Layout {
    public static final EdgeLayoutData DEFAULT_DATA = new EdgeLayoutData();

    public static class EdgeLayoutData {
        public static enum EdgeAffinity {
            LEFT, RIGHT
        };

        public boolean grabExcessHorizontalSpace = false;

        public boolean grabExcessVerticalSpace = false;

        public EdgeAffinity edgeAffinity = EdgeAffinity.LEFT;

        public int minWidth = SWT.DEFAULT;

        public int minHeight = SWT.DEFAULT;
    }

    public static enum VerticalAlignment {
        TOP, CENTER, BOTTOM
    };

    public VerticalAlignment verticalAlignment = VerticalAlignment.TOP;

    public EdgeLayout() {
        super();
    }

    public EdgeLayout(VerticalAlignment verticalAlignment) {
        this();
        this.verticalAlignment = verticalAlignment;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Layout#computeSize(org.eclipse.swt.widgets.Composite,
     *      int, int, boolean)
     */
    @Override
    protected Point computeSize(Composite composite, int wHint, int hHint,
            boolean flushCache) {
        Control[] children = composite.getChildren();

        int minWidth = 0;
        int minHeight = SWT.DEFAULT;
        for (Control child : children) {
            EdgeLayoutData data = (EdgeLayoutData) child.getLayoutData();
            if (data == null) {
                data = DEFAULT_DATA;
                child.setLayoutData(data);
            }
            Point size = child.computeSize(data.minWidth, data.minHeight,
                    flushCache);

            minWidth += size.x == SWT.DEFAULT ? 0 : size.x;
            minHeight = Math.max(minHeight, size.y);
        }

        return new Point(Math.max(minWidth, wHint), Math.max(minHeight, hHint));
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Layout#layout(org.eclipse.swt.widgets.Composite,
     *      boolean)
     */
    @Override
    protected void layout(Composite composite, boolean flushCache) {
        Rectangle compRect = composite.getClientArea();

        Control[] children = composite.getChildren();

        // order controls from left to right
        int grabCount = 0;
        int minWidth = 0;
        int left = 0;
        LinkedList<Control> orderedChildren = new LinkedList<Control>();
        for (Control child : children) {
            EdgeLayoutData data = (EdgeLayoutData) child.getLayoutData();
            if (data == null) {
                data = DEFAULT_DATA;
                child.setLayoutData(data);
            }

            if (data.grabExcessHorizontalSpace) {
                grabCount++;
            }

            Point size = child.computeSize(data.minWidth, data.minHeight,
                    flushCache);

            if (data.grabExcessVerticalSpace) {
                size.y = Math.max(size.y, compRect.height);
            }

            child.setSize(size);
            minWidth += size.x;

            if (data.edgeAffinity.equals(EdgeAffinity.LEFT)) {
                orderedChildren.add(left++, child);
            } else {
                orderedChildren.addLast(child);
            }
        }

        // compute size and location of each control
        int extraWidth = compRect.width - minWidth;
        if (extraWidth < 0) {
            extraWidth = 0;
        }

        left = compRect.x;
        int right = compRect.x + compRect.width + 1;

        int top = compRect.y;
        int bottom = compRect.y + compRect.height + 1;
        int w, h;
        for (Control child : orderedChildren) {
            EdgeLayoutData data = (EdgeLayoutData) child.getLayoutData();

            Point size = child.getSize();

            w = size.x;
            h = size.y;
            if (data.grabExcessHorizontalSpace) {
                int extra = extraWidth / grabCount;
                w += extra;
                extraWidth -= extra;
                grabCount--;
            }
            child.setSize(w, h);

            int y;
            switch (verticalAlignment) {
            case BOTTOM:
                y = bottom - h;
                break;
            case CENTER:
                y = top + (compRect.height - h) / 2;
                break;
            default:
                y = top;
            }

            if (data.edgeAffinity.equals(EdgeAffinity.LEFT)) {
                child.setLocation(left, y);
                left += w;
            } else {
                right -= w;
                child.setLocation(right, y);
            }
        }
    }

    public static void main(String[] args) {
        Window window = new Window((Shell) null) {

            @Override
            protected Control createContents(Composite parent) {
                Composite comp = (Composite) super.createContents(parent);
                comp.setLayoutData(new GridData(GridData.FILL_BOTH));
                comp.setLayout(new EdgeLayout(VerticalAlignment.BOTTOM));

                Label l;
                EdgeLayoutData layoutData;

                l = new Label(comp, SWT.BORDER);
                l.setText("Test 1");
                layoutData = new EdgeLayoutData();
                layoutData.edgeAffinity = EdgeAffinity.LEFT;
                l.setLayoutData(layoutData);

                l = new Label(comp, SWT.BORDER);
                l.setText("Test 2");
                layoutData = new EdgeLayoutData();
                layoutData.edgeAffinity = EdgeAffinity.LEFT;
                layoutData.grabExcessHorizontalSpace = true;
                layoutData.grabExcessVerticalSpace = true;
                layoutData.minHeight = 40;
                l.setLayoutData(layoutData);

                l = new Label(comp, SWT.BORDER);
                l.setText("Test 3");
                layoutData = new EdgeLayoutData();
                layoutData.edgeAffinity = EdgeAffinity.RIGHT;
                l.setLayoutData(layoutData);

                l = new Label(comp, SWT.BORDER);
                l.setText("Test 4");
                layoutData = new EdgeLayoutData();
                layoutData.edgeAffinity = EdgeAffinity.RIGHT;
                layoutData.grabExcessHorizontalSpace = true;
                l.setLayoutData(layoutData);

                return comp;
            }

            @Override
            protected Point getInitialSize() {
                Point size = getContents().computeSize(SWT.DEFAULT,
                        SWT.DEFAULT, true);

                Rectangle rect = getShell().computeTrim(0, 0, size.x, size.y);
                size.x = rect.width;
                size.y = rect.height;

                getShell().setMinimumSize(size);
                return size;
            }

        };
        window.setBlockOnOpen(true);
        window.open();

    }
}
