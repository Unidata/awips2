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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.viz.ui.actions.AbstractGlobalsButtonHandler;
import com.raytheon.viz.ui.actions.HandlerTextSizer;

/**
 * Updates the load mode button with the current load mode
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2007            randerso    Initial Creation.
 * Mar 31, 2016 5519       bsteffen    Keep toolbar text constant width.
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class LoadModeButtonHandler extends AbstractGlobalsButtonHandler
        implements IGlobalChangedListener {

    public LoadModeButtonHandler() {
        super(VizConstants.LOADMODE_ID);
    }

    @Override
    protected void updateGlobalValue(IWorkbenchWindow changedWindow,
            UIElement element, Object value) {
        LoadMode mode = (LoadMode) value;
        if (mode == null) {
            return;
        }
        HandlerTextSizer sizer = new HandlerTextSizer(Display.getCurrent());
        for (LoadMode loadMode : LoadMode.values()) {
            sizer.setMinIfWider(loadMode.getLabel());
        }
        String text = sizer.createAdjustedText(mode.getLabel());
        sizer.dispose();
        element.setText(text);
    }

}
