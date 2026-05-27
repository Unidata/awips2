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
package com.raytheon.viz.xdat;

import java.util.List;

import org.eclipse.swt.graphics.Point;

import com.raytheon.viz.xdat.XdatDlg.UpdateType;

/**
 * 
 * Interface defining the callbacks used for displaying information on the
 * display and retrieving user selected information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2009 1883       lvenable     Initial creation
 * 10 Feb 2009             wkwock      Added functions.
 * 04 Aug 2016  5800       mduff       Changed getSelectedText to return the List Interface
 * 12 Mar 2018  DCS18260   astrakovsky Changed rows to be selected in one click and
 *                                     fixed GUI not updating.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public interface ITextDisplay {

    /**
     * Save the current text area selection.
     */
    public void saveSelection();

    /**
     * Restore the last saved text area selection.
     */
    public void restoreSelection();

    /**
     * Set the text on the display.
     * 
     * @param text
     *            String array of text.
     */
    void setDisplayText(String[] text);

    /**
     * Set the text on the display.
     * 
     * @param text
     *            Text string.
     */
    void setDisplayText(String text);

    /**
     * Get the text on the display.
     * 
     * @return the text area content.
     */
    public String getDisplayText();

    /**
     * Get the currently selected line.
     * 
     * @return the line number.
     */
    int getCurrentLineNumber();

    /**
     * Get the offset at line.
     * 
     * @return the offset.
     */
    public int getOffsetAtLine(int line);

    /**
     * Get the line indicated
     */
    String getLine(int line);

    /**
     * Get the line count
     */
    int getLineCount();

    /**
     * Set the top index
     * 
     * @param int
     *            the top index to set.
     */
    void setTopIndex(int index);

    /**
     * Get the top index
     */
    int getTopIndex();

    /**
     * Set the text selection
     * 
     * @param point
     *            the selection point to set.
     */
    void setSelection(Point point);

    /**
     * Set the text selection
     * 
     * @param int
     *            the selection coordinates to set.
     */
    void setSelection(int x, int y);

    /**
     * Get the text selection
     */
    Point getSelection();

    /**
     * Get the last update
     * 
     * @return
     */
    public UpdateType getLastUpdate();

    /**
     * Set the last update
     * 
     * @param lastUpdate
     */
    public void setLastUpdate(UpdateType lastUpdate);

    /**
     * Refresh the contents of the text area.
     * 
     */
    public void refreshTextArea();

    /**
     * Get the starting date in a string format.
     * 
     * @return start date
     */
    String getStartDate();

    /**
     * Get the ending date in a String format.
     * 
     * @return end date
     */
    String getEndDate();

    /**
     * Display the data associated with the selected ID.
     */
    public void displayIdSelection(String selectedId);

    /**
     * Update the GUI with the new value and changes without reloading from DB.
     */
    public void updateTextAreaValue(String newValue);

    /**
     * Display COOP Precipitation
     */
    public void displayCoopPrecip();

    /**
     * Display Precipitation Accumulation
     */
    public void displayPrecipAccumulation(int hour, int duration);

    /**
     * Retrieve and display the group data.
     */
    public void retrieveAndDisplayGroupData(String selectedGroup);

    /**
     * Get the entered PE or the selected PE.
     * 
     * @return PE
     */
    String getSelectedPE();

    /**
     * Get the selected text from the main display area.
     * 
     * @return text
     */
    List<String> getSelectedText();

    /**
     * Get the selected site ID from the text box.
     * 
     * @return
     */
    String getSelectedSite();
}
