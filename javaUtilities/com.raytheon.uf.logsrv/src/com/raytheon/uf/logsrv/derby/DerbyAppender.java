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
package com.raytheon.uf.logsrv.derby;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import com.raytheon.uf.logsrv.LogService;
import com.raytheon.uf.logsrv.LogServiceException;
import com.raytheon.uf.logsrv.StoredMsg;

/**
 * A logback appender that stores a logging event to the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DerbyAppender extends AppenderBase<ILoggingEvent> {

    private DerbyDao dao;

    public DerbyAppender() {
        super();
        dao = DerbyDao.getInstance();
    }

    @Override
    protected void append(ILoggingEvent eventObject) {
        if (shouldStoreMsg(eventObject)) {
            StoredMsg msg = new StoredMsg(eventObject);
            try {
                dao.insert(msg);
            } catch (LogServiceException e) {
                LogService.getLogger().error(
                        "Error inserting message into derby database", e);
            }
        }
    }

    /**
     * Checks whether or not the appender should store the logging event
     * 
     * @param event
     * @return
     */
    private boolean shouldStoreMsg(ILoggingEvent event) {
        return true;
    }
}
