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
package com.raytheon.viz.mpe.ui.actions;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Handles selection of a specific pane of a multipane map editor. This action
 * is triggered by right clicking in the pane and selecting one of the "load to"
 * options.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2007 560        MW Fegan    Initial creation.
 * Sep 28, 2010 5922       snaples     Extended for MPE.
 * Aug  8, 2018 6891       tgurney     Add text field + cleanup
 *
 * </pre>
 *
 * @author mfegan
 */

public class MPESelectPaneAction extends AbstractRightClickAction {

    /**
     * Identifies by number the pane this action is applied to
     */
    private IDisplayPane pane;

    private String text;

    /**
     * Constructor. Creates a MPESelectPaneAction wired to the specified pane
     * and displaying the specified text.
     *
     * @param pane
     *            the pane this action works on
     * @param text
     *            the text to display in the menu button
     */
    public MPESelectPaneAction(IDisplayPane pane, String text) {
        this.pane = pane;
        this.text = text;
    }

    @Override
    public void run() {
        if (container instanceof IMultiPaneEditor) {
            IMultiPaneEditor editor = (IMultiPaneEditor) container;
            editor.setSelectedPane(IMultiPaneEditor.LOAD_ACTION, pane);
        }
    }

    @Override
    public String getText() {
        return text;
    }

}
