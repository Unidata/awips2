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

package com.raytheon.viz.aviation.editor;

import java.util.List;

import com.raytheon.viz.aviation.editor.TafViewerEditorDlg.TafSettings;
import com.raytheon.viz.aviation.guidance.ViewerTab;
import com.raytheon.viz.avnconfig.IShowableHidable;

/**
 * The ITafSettable interface specifies the method that allows the Taf Monitor
 * Dialog to show the Taf Viewer Editor Dialog with the appropriate setting.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/27/2008    933         grichard    Initial creation.
 * 6/16/2008    937         grichard    Improved viewer/editor interaction.
 * 11/29/2011   11612       rferrel     Added getViewerTabList
 * Oct 16, 2015 4645        skorolev    Added updateWordWrap
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 * 
 */

public interface ITafSettable extends IShowableHidable {

    /**
     * Update the settings
     * 
     * @param setting
     *            TAF setting.
     * @param theStation
     *            The station name.
     */
    void updateSettings(TafSettings setting, String stationName);

    /**
     * Populate the stations in the viewer.
     * 
     * @param theStation
     *            -- the station
     */
    void populateViewerStation(String theStation);

    /**
     * Dispose dialog.
     */
    void disposeDialog();

    /**
     * Update the insert capability to either overwrite or insert text.
     * 
     * @param updateInsertChk
     *            Flag indicating the insert check box should be updated.
     */
    void updateInsert(boolean updateInsertChk);

    /**
     * Update the WordWrap capability.
     * 
     * @param updateWordWrapChk
     */
    void updateWordWrap(boolean updateWordWrapChk);

    /**
     * Obtain the current station in the viewer.
     * 
     * @return stationName
     */
    String getCurrentViewerStation();

    /**
     * Obtain list of active Viewer Tabs.
     * 
     * @return viewerTabList
     */
    public List<ViewerTab> getViewerTabList();
}