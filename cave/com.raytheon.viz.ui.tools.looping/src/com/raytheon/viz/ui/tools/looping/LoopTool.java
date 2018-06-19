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

package com.raytheon.viz.ui.tools.looping;

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Activate the animation loop
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * Dec 3, 2007  559         njensen     Extends AbstractTool instead
 *                                      of AbstractMapTool
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class LoopTool extends AbstractTool implements IGlobalChangedListener {

    protected UIElement lastElement;

    protected IWorkbenchWindow window;

    public LoopTool() {
        super();
        VizGlobalsManager.addListener(VizConstants.LOOPING_ID, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        if (editor != null) {
            boolean newState = !(editor.getLoopProperties().isLooping());
            editor.getLoopProperties().setLooping(newState);

            setEnabled(newState);

            VizGlobalsManager.getCurrentInstance().updateUI(editor);
            LoopPropertiesDialog.setLooping(newState);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#updateElement(org.eclipse.ui.menus
     * .UIElement, java.util.Map)
     */
    @Override
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        if (element != null) {
            lastElement = element;
        }
        window = (IWorkbenchWindow) parameters
                .get("org.eclipse.ui.IWorkbenchWindow");
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            this.editor = container;
            element.setChecked(editor.getLoopProperties().isLooping());
        }
    }

    @Override
    public void updateValue(IWorkbenchWindow window, Object value) {
        if (this.window == window) {
            Boolean enabled = (Boolean) value;
            lastElement.setChecked(enabled);
        }
    }

}
