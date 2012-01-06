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
package com.raytheon.uf.edex.services;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2008       1538 jkorman     Initial implementation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface TextDBSrvInterface {

    /**
     * 
     * @return
     */
    public String getServiceName();

    /**
     * Get a count of messages processed since startup or the last reset.
     * 
     * @return Message count.
     */
    public int getMessageCount();

    /**
     * Reset the message count to zero.
     */
    public void clearMessageCount();

    /**
     * Execute an arbitrary string command..
     * 
     * @param command
     *            A command to execute.
     */
    public void execute(String command);

}
