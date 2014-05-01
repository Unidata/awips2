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
package com.raytheon.viz.volumebrowser.vbui;

import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.LeftRightMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * This is a container class to keep the various values used for the dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2010            jelkins     Initial creation
 * Jul 31, 2012 #875       rferrel     Now uses markers.
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class VolumeBrowserDialogSettings {

    private ViewMenu viewSelection;

    private SpaceTimeMenu spaceTimeSelection;

    private LeftRightMenu timeDirectionSelection;

    private IPointNode pointsSelection;

    private HeightScale heightScale;

    /**
     * @param dialogSettings
     */
    public VolumeBrowserDialogSettings(
            VolumeBrowserDialogSettings dialogSettings) {
        this.viewSelection = dialogSettings.viewSelection;
        this.spaceTimeSelection = dialogSettings.spaceTimeSelection;
        this.timeDirectionSelection = dialogSettings.timeDirectionSelection;
        this.pointsSelection = dialogSettings.pointsSelection;
        this.heightScale = dialogSettings.heightScale;
    }

    /**
     * 
     */
    public VolumeBrowserDialogSettings() {
        // TODO Auto-generated constructor stub
    }

    public void setViewSelection(ViewMenu viewSelection) {
        this.viewSelection = viewSelection;
    }

    public ViewMenu getViewSelection() {
        return viewSelection;
    }

    public void setSpaceTimeSelection(SpaceTimeMenu spaceTimeSelection) {
        this.spaceTimeSelection = spaceTimeSelection;
    }

    public SpaceTimeMenu getSpaceTimeSelection() {
        return spaceTimeSelection;
    }

    public void setTimeDirectionSelection(LeftRightMenu timeDirectionSelection) {
        this.timeDirectionSelection = timeDirectionSelection;
    }

    public LeftRightMenu getTimeDirectionSelection() {
        return timeDirectionSelection;
    }

    public void setPointsSelection(IPointNode pointsSelection) {
        this.pointsSelection = pointsSelection;
    }

    public IPointNode getPointsSelection() {
        return pointsSelection;
    }

    private <T> T getSelectionData(Class<T> selectionType, MenuItem mi) {
        return selectionType.cast(mi.getData());
    }

    /**
     * @param event
     */
    public void setPointsSelection(MenuItem mi) {
        pointsSelection = getSelectionData(Point.class, mi);
    }

    /**
     * @param event
     */
    public void setTimeDirectionSelection(MenuItem mi) {
        timeDirectionSelection = getSelectionData(LeftRightMenu.class, mi);
    }

    /**
     * @param event
     */
    public void setViewSelection(MenuItem mi) {
        viewSelection = getSelectionData(ViewMenu.class, mi);
    }

    /**
     * @param event
     */
    public void setSpaceTimeSelection(MenuItem mi) {
        spaceTimeSelection = getSelectionData(SpaceTimeMenu.class, mi);
    }

    public void setHeightScaleSelection(MenuItem mi) {
        this.heightScale = getSelectionData(HeightScale.class, mi);
    }

    public HeightScale getHeightScaleSelection() {
        return heightScale;
    }
}
