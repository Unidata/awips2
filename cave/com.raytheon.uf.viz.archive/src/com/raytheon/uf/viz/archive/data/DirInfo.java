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
package com.raytheon.uf.viz.archive.data;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager.DisplayData;
import com.raytheon.uf.common.util.FileUtil;

/**
 * This class uses a obtains information on a File in a Job in order to remove
 * from the UI thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2013 1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class DirInfo {
    private static final SizeJob sizeJob = new DirInfo.SizeJob();

    final private DisplayData displayInfo;

    private final List<File> files = new ArrayList<File>();

    private Calendar startCal;

    private Calendar endCal;

    public static void clearQueue() {
        sizeJob.clearQueue();
    }

    public static void addUpdateListener(IUpdateListener listener) {
        sizeJob.listeners.add(listener);
    }

    public static void removeUpdateListener(IUpdateListener listener) {
        sizeJob.listeners.remove(listener);
    }

    public DirInfo(DisplayData displayInfo, Calendar startCal, Calendar endCal) {
        this.displayInfo = displayInfo;
        this.startCal = startCal;
        this.endCal = endCal;
        displayInfo.setSize(-1);
        DirInfo.sizeJob.queue(this);
    }

    public DisplayData getDisplayInfo() {
        return displayInfo;
    }

    static private class SizeJob extends Job {
        private LinkedList<DirInfo> queueList = new LinkedList<DirInfo>();

        private boolean isShutdown = false;

        List<IUpdateListener> listeners = new ArrayList<IUpdateListener>();

        protected void queue(DirInfo fileInfo) {
            synchronized (queueList) {
                queueList.add(fileInfo);
                if (getState() == Job.NONE) {
                    System.out.println("schedule queue size: "
                            + queueList.size());
                    schedule();
                }
            }
        }

        protected void clearQueue() {
            synchronized (queueList) {
                queueList.clear();
                if (getState() != Job.NONE) {
                    isShutdown = true;
                }
            }
        }

        public SizeJob() {
            super("Size Job");
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
            System.out.println("starting SizeJob");
            long startTime = System.currentTimeMillis();
            while (!isShutdown && !queueList.isEmpty()) {
                List<DirInfo> list = null;
                synchronized (queueList) {

                    list = new ArrayList<DirInfo>(queueList);

                    queueList.clear();
                }
                System.out.println("sizeJob Processing: " + list.size());
                long t1 = System.currentTimeMillis();

                for (DirInfo dirInfo : list) {
                    long t2 = System.currentTimeMillis();
                    if (isShutdown) {
                        break;
                    }
                    DisplayData displayInfo = dirInfo.displayInfo;
                    Calendar startCal = dirInfo.startCal;
                    Calendar endCal = dirInfo.endCal;
                    displayInfo.setSize(-1);

                    List<File> files = manager.getDisplayFiles(displayInfo,
                            startCal, endCal);
                    dirInfo.files.clear();
                    dirInfo.files.addAll(files);
                    long size = 0L;
                    for (File file : dirInfo.files) {
                        if (isShutdown) {
                            break;
                        }
                        if (file.isDirectory()) {
                            size += FileUtil.sizeOfDirectory(file);
                        } else {
                            size += file.length();
                        }
                    }
                    long t3 = System.currentTimeMillis();
                    System.out.println("-- \"" + displayInfo.getDisplayLabel()
                            + "\": oldSize: " + displayInfo.getSize()
                            + ", newSize: " + size + ", time: " + (t3 - t2)
                            + " ms");
                    displayInfo.setSize(size);
                }

                if (!isShutdown) {
                    for (IUpdateListener listener : listeners) {
                        listener.update(list);
                    }
                } else {
                    synchronized (queueList) {
                        isShutdown = false;
                    }
                }
            }
            System.out.println("Ending SizeJob");

            return Status.OK_STATUS;
        }
    }
}
