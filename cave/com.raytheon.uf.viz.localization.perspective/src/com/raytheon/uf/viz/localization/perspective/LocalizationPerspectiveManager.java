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
package com.raytheon.uf.viz.localization.perspective;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.texteditor.ITextEditor;

import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;

/**
 * Implementation for localization perspective, utilizing the localization
 * perspective as a start
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2010            mnash     Initial creation
 * Nov 02, 2012 1302       djohnson  Remove printStackTrace.
 * May 1, 2013   1967    njensen      Separated out pydev specific code
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LocalizationPerspectiveManager extends
        AbstractVizPerspectiveManager {

    /** The edit position restore map */
    private final Map<IEditorInput, Point> restoreMap = new HashMap<IEditorInput, Point>();

    public LocalizationPerspectiveManager() {
        saveEditors = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#open()
     */
    @Override
    protected void open() {
        PydevSetup.preparePydev();
    }

    @Override
    public void activateInternal() {
        super.activateInternal();
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            if (part != null) {
                IEditorInput input = part.getEditorInput();
                if (part instanceof ITextEditor) {
                    ITextEditor editor = (ITextEditor) part;
                    Point offsetLength = restoreMap.get(input);
                    if (offsetLength != null) {
                        editor.selectAndReveal(offsetLength.x, offsetLength.y);
                    }
                }
            }
        }
        restoreMap.clear();
    }

    @Override
    public void deactivate() {
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            if (part != null) {
                IEditorInput input = part.getEditorInput();
                if (part instanceof ITextEditor) {
                    ITextEditor editor = (ITextEditor) part;
                    ITextSelection selection = (ITextSelection) editor
                            .getSelectionProvider().getSelection();
                    if (selection != null) {
                        restoreMap.put(input, new Point(selection.getOffset(),
                                selection.getLength()));
                    }
                }
            }
        }
        super.deactivate();
    }

}
