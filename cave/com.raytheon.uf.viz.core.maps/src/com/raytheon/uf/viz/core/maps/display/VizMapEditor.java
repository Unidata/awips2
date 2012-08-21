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

package com.raytheon.uf.viz.core.maps.display;

import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;

import com.raytheon.viz.ui.editor.VizMultiPaneEditor;

/**
 * Defines the map editor, class solely used for context activation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 * 7/1/06                      chammack    Initial Creation.
 * 12/3/07         461         bphillip    Added Time Display to Status bar
 * Oct 21, 2008     #1450      randerso    Moved multipanel support down into GLMapEditor
 * 04/09/09        2228        rjpeter     Removed recursive listener adding.
 * Aug 31, 2009    2920        rjpeter     Moved MapContext Activation/Deactivation to include when window loses focus.
 * </pre>
 * 
 * @author chammack
 * 
 */
public class VizMapEditor extends VizMultiPaneEditor {

    public static final String EDITOR_ID = "com.raytheon.viz.ui.glmap.GLMapEditor";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.AbstractEditor#init(org.eclipse.ui.IEditorSite
     * , org.eclipse.ui.IEditorInput)
     */
    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        super.init(site, input);
        setTabTitle("Map");
    }

}
