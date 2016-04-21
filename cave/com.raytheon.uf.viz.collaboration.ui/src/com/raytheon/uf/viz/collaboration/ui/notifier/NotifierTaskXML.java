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
package com.raytheon.uf.viz.collaboration.ui.notifier;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * XML Wrapper for the {@link NotifierTask}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2014    2632    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class NotifierTaskXML {

    /** Array of notifier tasks */
    @DynamicSerializeElement
    private NotifierTask[] notifierTasks;

    /**
     * @return the notifierTasks
     */
    public NotifierTask[] getNotifierTasks() {
        if (notifierTasks == null) {
            notifierTasks = new NotifierTask[0];
        }

        return notifierTasks;
    }

    /**
     * @param notifierTasks
     *            the notifierTasks to set
     */
    public void setNotifierTasks(NotifierTask[] notifierTasks) {
        this.notifierTasks = notifierTasks;
    }
}
