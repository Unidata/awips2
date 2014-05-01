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
/**
 * 
 */
package com.raytheon.viz.ui.actions;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;

/**
 * Updates the scale
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2007            randerso    Initial Creation.
 * Oct 10, 2013       2104 mschenke    Will truncate text if too long
 * 
 * &#064;author randerso
 * 
 */
public class ScaleButtonHandler extends AbstractGlobalsButtonHandler implements
        IElementUpdater, IGlobalChangedListener {

    private static final int TEXT_LIMIT = 100;

    public ScaleButtonHandler() {
        super(VizConstants.SCALE_ID);
    }

    @Override
    protected void updateGlobalValue(IWorkbenchWindow changedWindow,
            UIElement element, Object value) {
        String scale = (String) value;
        String tooltip = scale;

        GC gc = new GC(Display.getCurrent());
        if (gc.textExtent(scale).x > TEXT_LIMIT) {
            String suffix = "...";
            String text = scale.substring(0, suffix.length()) + suffix;
            for (int i = suffix.length() + 1; i < scale.length(); ++i) {
                String test = scale.substring(0, i) + suffix;
                if (gc.textExtent(test).x < TEXT_LIMIT) {
                    text = test;
                } else {
                    break;
                }
            }
            scale = text;
        }
        gc.dispose();
        element.setText(scale);
        element.setTooltip("Scale: " + tooltip);
    }

}
