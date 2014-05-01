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

package com.raytheon.viz.texteditor.msgs;

/**
 * The ITextCharWrapCallback interface specifies methods that the Text Char Wrap
 * Dialog uses to set fields that are contained in the Text Editor Dialog for
 * use in updating the character wrap-around setting in the text editor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/10/2008    722         grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * 
 */

public interface ITextCharWrapCallback {

    /**
     * Set character wrap column setting in Text Editor Dialog.
     */
    void setCharWrapCol(int charWrapCol);

}