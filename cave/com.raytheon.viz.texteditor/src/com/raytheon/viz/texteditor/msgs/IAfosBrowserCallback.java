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

import com.raytheon.viz.texteditor.command.ICommand;

/**
 * The IAfosBrowserCallback interface specifies methods that query for a text
 * product and set an Afos command field string.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/27/2007   520         grichard    Initial creation.
 * 12/7/2007    582         grichard    Implemented build 12 features.
 * 
 * </pre>
 * 
 * @author grichard
 * 
 */

public interface IAfosBrowserCallback {
    public void executeCommand(ICommand command);

    public void setAfosCmdField(String cmd);
}
