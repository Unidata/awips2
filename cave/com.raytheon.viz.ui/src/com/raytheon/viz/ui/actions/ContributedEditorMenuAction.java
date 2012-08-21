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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.internal.presentations.ISelfUpdatingAction;
import org.eclipse.ui.presentations.IPresentablePart;

/**
 * Every type of user contributed action to the editor should extend this class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class ContributedEditorMenuAction extends Action implements
        ISelfUpdatingAction {

    private IPresentablePart part;

    /**
     * 
     */
    public ContributedEditorMenuAction() {
        super();
    }

    /**
     * @param text
     * @param image
     */
    public ContributedEditorMenuAction(String text, ImageDescriptor image) {
        super(text, image);
    }

    /**
     * @param text
     * @param style
     */
    public ContributedEditorMenuAction(String text, int style) {
        super(text, style);
    }

    /**
     * @param text
     */
    public ContributedEditorMenuAction(String text) {
        super(text);
    }

    @Override
    public void update() {
        // do nothing to for this class
    }

    @Override
    public boolean shouldBeVisible() {
        return true;
    }

    /**
     * @param part
     *            the part to set
     */
    public void setPart(IPresentablePart part) {
        this.part = part;
    }

    /**
     * @return the part
     */
    public IPresentablePart getPart() {
        return part;
    }

}
