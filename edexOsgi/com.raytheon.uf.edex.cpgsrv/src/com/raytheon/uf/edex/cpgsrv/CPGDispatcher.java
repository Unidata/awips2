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
package com.raytheon.uf.edex.cpgsrv;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.msg.DataURINotificationMessage;

/**
 * Provides a dispatcher for CPG services
 * 
 * This provides a thread pool of workers to dispatch the messages to.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class CPGDispatcher {

    private Log log = LogFactory.getLog(CPGDispatcher.class);

    private List<CompositeProductGenerator> generatorSet;

    public CPGDispatcher() {
        this.generatorSet = new ArrayList<CompositeProductGenerator>();
    }

    public CPGDispatcher register(CompositeProductGenerator generator) {
        generatorSet.add(generator);
        return this;
    }

    public void matchURIs(DataURINotificationMessage msg) {
        for (CompositeProductGenerator gen : generatorSet) {
            try {
                gen.matchURIs(msg);
            } catch (Throwable e) {
                // Catch everything so thread doesn't die
                log.error("Uncaught error occurred during processing", e);
            }
        }
    }
}
