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
package com.raytheon.viz.texteditor.dialogs;

/**
 * Interface to retrieve the WarnGen Confirmation Dialog message values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013 2176       jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public interface IWarnGenConfirmationable {
    /**
     * Returns true if the WarnGen Confirmation Dialog needs to pop-up.
     * 
     * @param header
     * @param body
     * @param nnn
     * @return
     */
    public boolean checkWarningInfo(String header, String body, String nnn);

    /**
     * 
     * @return the title for the WarnGen Confirmation Dialog
     */
    public String getTitle();

    /**
     * 
     * @return the product message in the WarnGen Confirmation Dialog
     */
    public String getProductMessage();

    /**
     * 
     * @return the mode message in the WarnGen Confirmation Dialog
     */
    public String getModeMessage();
}
