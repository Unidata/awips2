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
package com.raytheon.uf.edex.ndm.ingest;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EdexException;

/**
 * National Dataset Maintenance ingester.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2011            bfarmer     Initial creation
 * Aug 11,2011  9965      rferrel     Added logging to processEvent
 * Aug 24,2011  10775     rferrel     Fixed error in processEvent and added
 *                                    check on statusHandler messages.
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class NationalDatasetIngester implements IDataSetIngester {

    private final Map<String, List<INationalDatasetSubscriber>> listeners = new HashMap<String, List<INationalDatasetSubscriber>>();

    /**
     * Register a filename to be processed and the subscription listener that
     * will process the file.
     * 
     * @param filename
     * @param listener
     * @return
     */
    @Override
    public INationalDatasetSubscriber registerListener(String filename,
            INationalDatasetSubscriber listener) {
        if (listeners.get(filename) == null) {
            listeners
                    .put(filename, new ArrayList<INationalDatasetSubscriber>());
        }
        listeners.get(filename).add(listener);
        return listener;
    }

    /**
     * Remove a subscription listener for a given file.
     * 
     * @param filename
     * @param listener
     * @return Returns the listener reference that was removed, otherwise a null
     *         reference is returned.
     */
    @Override
    public INationalDatasetSubscriber removeListener(String filename,
            INationalDatasetSubscriber listener) {

        INationalDatasetSubscriber storedListener = null;
        if (filename != null) {
            if (listeners.containsKey(filename)) {
                List<INationalDatasetSubscriber> list = listeners.get(filename);
                for (int i = 0; i < list.size(); i++) {

                    if (list.get(i).equals(listener)) {
                        storedListener = list.get(i);
                        list.remove(i);
                        break;
                    }
                }
            }
        }
        return storedListener;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
     */
    public void processEvent(File file) throws EdexException {
        String fileName = file.getName();
        IUFStatusHandler statusHandler = UFStatus
                .getHandler(NationalDatasetIngester.class);

        if (listeners.get(fileName) != null) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(
                        Priority.INFO,
                        String.format("Start processing file \"%1$s\".",
                                file.getAbsoluteFile()));
            }

            for (INationalDatasetSubscriber listener : listeners.get(fileName)) {
                try {
                    listener.notify(fileName, file);
                } catch (Throwable ex) {
                    if (statusHandler.isPriorityEnabled(Priority.ERROR)) {
                        statusHandler.handle(Priority.ERROR, String.format(
                                "Problem notifying listener for \"%1$s\".",
                                file.getAbsoluteFile()), ex);
                    }
                }
            }

            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, String.format(
                        "Finished processing file \"%1$s\".",
                        file.getAbsoluteFile()));
            }
        } else {
            if (statusHandler.isPriorityEnabled(Priority.WARN)) {
                statusHandler.handle(
                        Priority.WARN,
                        String.format("No listeners for file: \"%1$s\".",
                                file.getAbsoluteFile()));
            }
        }
    }

}
