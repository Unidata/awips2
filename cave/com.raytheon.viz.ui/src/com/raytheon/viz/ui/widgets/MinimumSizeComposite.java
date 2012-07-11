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
package com.raytheon.viz.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * This class extends Composite and prevents its computed size from being less
 * then the set minimum.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Jul 11, 2012 #875       rferrel     Move to common package and
 *                                      made minWidth and minHeight 
 *                                      accessible to subclasses.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class MinimumSizeComposite extends Composite {

    protected int minWidth = SWT.DEFAULT;

    protected int minHeight = SWT.DEFAULT;

    public MinimumSizeComposite(Composite parent) {
        this(parent, SWT.NONE);
    }

    public MinimumSizeComposite(Composite parent, int style) {
        super(parent, style);
        GridLayout gd = new GridLayout(1, false);
        gd.marginWidth = 0;
        gd.marginHeight = 0;
        setLayout(gd);
    }

    public void setControl(Control c) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        if (minWidth != SWT.DEFAULT)
            gd.minimumWidth = minWidth;
        if (minHeight != SWT.DEFAULT)
            gd.minimumHeight = minHeight;
        c.setLayoutData(gd);
    }

    public void setMinimumSizeFromComputedSize() {
        setMinimumSize(super.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }

    @Override
    public Point computeSize(int wHint, int hHint, boolean changed) {
        Point sz = super.computeSize(wHint, hHint, changed);
        sz.x = Math.max(minWidth, sz.x);
        sz.y = Math.max(minHeight, sz.y);
        return sz;
    }

    public void setMinimumSize(Point sz) {
        minWidth = sz.x;
        minHeight = sz.y;
    }

    public void setMinimumSize(int width, int height) {
        minWidth = width;
        minHeight = height;
    }

    @Override
    public void layout(boolean changed, boolean all) {
        super.layout(changed, all);
    }

}
