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
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.util.FileUtil;

/**
 * This class uses a obtains information on a File in a Job in order to remove from the UI thread.
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

public class FileInfo {
    private static final SizeJob sizeJob = new FileInfo.SizeJob();

    private File file;

    private long size = -1L;

    public FileInfo(File file) {
        this.file = file;
        FileInfo.sizeJob.queue(this);
    }

    public long getSize() {
        return size;
    }

    static private class SizeJob extends Job {
        private LinkedList<FileInfo> queueList = new LinkedList<FileInfo>();

        List<IUpdateListener> listeners = new ArrayList<IUpdateListener>();

        protected void queue(FileInfo fileInfo) {
            synchronized (queueList) {
                queueList.add(fileInfo);
                if (getJobManager().currentJob() == null) {
                    schedule();
                }
            }
        }

        public SizeJob() {
            super("Size Job");
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (!queueList.isEmpty()) {
                FileInfo[] list = null;
                synchronized (queueList) {
                    list = queueList.toArray(new FileInfo[0]);
                    queueList.clear();
                }

                for (FileInfo fileInfo : list) {
                    File file = fileInfo.file;
                    if (file.isDirectory()) {
                        fileInfo.size = FileUtil.sizeOfDirectory(file);
                    } else {
                        fileInfo.size = file.length();
                    }
                }

                for (IUpdateListener listener : listeners) {
                    listener.update(list);
                }
            }

            return Status.OK_STATUS;
        }
    }
}
