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
package com.raytheon.viz.ui.editor;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

/**
 * Specifies an interface for multi-pane editors. Methods are provided for
 * accessing the number of panes and the currently selected pane. <BR>
 * <B>Note:</B> In this context, <em>selected pane</em> refers to the pane of
 * the multi-pane editor to which data is to be loaded. Setting the
 * <em>selected pane</em> to null causes data to be loaded to all panes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Dec2007    560        MW Fegan    Initial Creation.
 * Oct 21, 2008   #1450    randerso    Added support dynamically adding and removing panes
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public interface IMultiPaneEditor extends IDisplayPaneContainer {

    public static final String LOAD_ACTION = "Load";

    public static final String IMAGE_ACTION = "ImageProperties";

    public static final String VISIBLE_PANE = "VisiblePane";

    /**
     * Returns the number of panes in the editor.
     * 
     * @return the number of panes in the editor.
     */
    int getNumberofPanes();

    /**
     * Set the selected pane for a specific action for the editor. The action
     * String can be any arbitrary String, but should be unique. This allows an
     * editor to have panes selected for any arbitrary action, such as loading
     * new resources or changing image properties.
     * 
     * @param pane
     *            the pane to select
     */
    void setSelectedPane(String action, IDisplayPane pane);

    /**
     * Get the selected pane for the specified action.
     * 
     * @return the selected pane or null if no pane is selected
     */
    IDisplayPane getSelectedPane(String action);

    /**
     * Get an array of panes selected for the action
     * 
     * @param action
     * @return
     */
    IDisplayPane[] getSelectedPanes(String action);

    /**
     * Determines if the specified pane is selected for the specified action.
     * 
     * @param pane
     *            the pane to check.
     * 
     * @return true if the specified pane is selected
     */
    boolean isSelectedPane(String action, IDisplayPane pane);

    /**
     * Add a listener to get notified when panes are selected/deselected
     * 
     * @param listener
     */
    void addSelectedPaneChangedListener(ISelectedPanesChangedListener listener);

    /**
     * Remove a selected pane changed listener
     * 
     * @param listener
     */
    void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener);

    /**
     * Add a new pane containing the supplied renderable display.
     * 
     * @param renderableDisplay
     *            the display to be added
     */
    IDisplayPane addPane(IRenderableDisplay renderableDisplay);

    /**
     * Remove the specified pane
     * 
     * @param pane
     *            the pane to be removed
     */
    void removePane(IDisplayPane pane);

    /**
     * Hide the pane so that it is no longer part of the editor draw.
     * 
     * @param pane
     */
    public void hidePane(IDisplayPane pane);

    /**
     * Show the pane so that it is drawn by the editor draw.
     * 
     * @param pane
     */
    public void showPane(IDisplayPane pane);

    /**
     * Get the number of displayed panes (uses visibility, not count)
     * 
     * @return
     */
    public int displayedPaneCount();

    /**
     * Clear the display
     */
    public void clear();
}
