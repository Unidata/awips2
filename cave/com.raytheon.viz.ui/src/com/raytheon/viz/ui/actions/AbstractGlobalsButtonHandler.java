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
package com.raytheon.viz.ui.actions;

import java.util.Map;

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.viz.ui.UiUtil;

/**
 * Abstract button handler for global changed listeners
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public abstract class AbstractGlobalsButtonHandler extends
        AbstractDropDownMenuHandler implements IGlobalChangedListener,
        IElementUpdater {

    private UIElement lastElement;

    private IWorkbenchWindow window;

    private final String globalId;

    protected AbstractGlobalsButtonHandler(String globalsId) {
        this.globalId = globalsId;
        VizGlobalsManager.addListener(globalsId, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#dispose()
     */
    @Override
    public void dispose() {
        VizGlobalsManager.removeListener(globalId, this);
        super.dispose();
    }

    @SuppressWarnings("rawtypes")
    public void updateElement(UIElement element, Map parameters) {
        window = (IWorkbenchWindow) parameters
                .get("org.eclipse.ui.IWorkbenchWindow");
        boolean updateCoolBar = true;
        if (lastElement == null) {
            // TODO:
            // This was an attempt to fix dispose error when starting up in
            // localization perspective and switching to d2d. Widget disposed
            // exceptions get thrown bc we have to update the cool bar to
            // relayout the widget when this method is called. Doesn't work 100%
            updateCoolBar = false;
        }

        lastElement = element;
        VizGlobalsManager mgr = VizGlobalsManager.getInstance(window);
        if (mgr != null) {
            updateGlobalValue(window, lastElement, mgr.getPropery(globalId));
            if (updateCoolBar) {
                UiUtil.updateWindowCoolBar(window);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.globals.IGlobalChangedListener#updateValue(org
     * .eclipse.ui.IWorkbenchWindow, java.lang.Object)
     */
    @Override
    public final void updateValue(IWorkbenchWindow changedWindow, Object value) {
        if (window == changedWindow && lastElement != null) {
            updateGlobalValue(window, lastElement, value);
            UiUtil.updateWindowCoolBar(window);
        }
    }

    /**
     * Update the UIElement based on the global value
     * 
     * @param changedWindow
     * @param element
     * @param value
     */
    protected abstract void updateGlobalValue(IWorkbenchWindow changedWindow,
            UIElement element, Object value);
}
