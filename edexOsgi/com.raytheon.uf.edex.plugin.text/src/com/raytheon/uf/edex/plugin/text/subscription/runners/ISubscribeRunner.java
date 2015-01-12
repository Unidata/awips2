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
package com.raytheon.uf.edex.plugin.text.subscription.runners;

import java.util.List;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;

/**
 * Defines the interface for the Subscription Runners. Implementations of this
 * interface define three methods {@link #initialize(Message)},
 * {@link #execute()} and {@link #getResults()}. Implementations must define a
 * no-arg constructor; they should also define a constructor that takes a
 * {@link Message} instance as its sole argument.
 * <P>
 * Each implementation of this class should ensure the {@link #getResults()}
 * returns a valid, though possibly empty, List.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public interface ISubscribeRunner {
    public static final String RESPONSE_NORMAL = "STDOUT";
    public static final String RESPONSE_ERROR = "STRERR";
    /**
     * Initializes the runner.
     * 
     * @param message the message to process
     */
    public void initialize(Message message);
    /**
     * Executes the work as specified in the previously set message.
     * 
     * @return true if execution was successful, false otherwise
     */
    public boolean execute();
    /**
     * Returns the results as previously computed by the execute() method.
     */
    public List<Property> getResults();
}
