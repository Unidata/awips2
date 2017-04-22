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
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public interface ITextDisplay {

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
