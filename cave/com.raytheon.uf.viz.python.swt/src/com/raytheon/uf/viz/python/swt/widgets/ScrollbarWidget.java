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
package com.raytheon.uf.viz.python.swt.widgets;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;

/**
 * Displays the dialog at a specific height.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 4, 2008	1164		jelkins	Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class ScrollbarWidget extends Widget {

    public ScrollbarWidget(String label) {
        super(label);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.dynamic.ISelectionInput#buildComposite(org.eclipse
     * .swt.widgets.Composite, int)
     */
    @Override
    public Composite buildComposite(Composite parent, int style) {
        parent.getShell().pack();
        Point size = parent.getShell().getSize();
        parent.getShell().setSize(size.x, ((Number) (getValue())).intValue());
        return null;
    }

}
