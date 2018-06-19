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
package com.raytheon.uf.viz.image.export.job;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.DateTimeSelection;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.ImageFormat;

/**
 * Base {@link Job} for saving a sequence of images. The primary function of
 * this class is to generate filenames, the actual saving of images is handled
 * by subclasses.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 07, 2015  4607     bsteffen    Extracted from ExportImageHandler
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1
 */
public abstract class AbstractSaveImagesJob extends Job {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractSaveImagesJob.class);

    protected static final String DATE_TIME_FORMAT = "yyyyMMdd_HHmmss";

    protected final ImageExportOptions options;

    protected final LinkedHashMap<DataTime, BufferedImage> dtbiHash;

    public AbstractSaveImagesJob(ImageExportOptions options,
            LinkedHashMap<DataTime, BufferedImage> dtbiHash) {
        super("Saving image");
        this.options = options;
        this.dtbiHash = dtbiHash;
        this.schedule();
    }

    /**
     * Prepare any objects that may be necessary to reuse when saving multiple
     * images.
     * 
     * @return true on success, false on failure. Failure should ahve already
     *         been reported to the user and will end this job.
     */
    protected abstract boolean initialize();

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        monitor.beginTask("Saving Images", dtbiHash.size());
        if (initialize() == false) {
            return Status.OK_STATUS;
        }
        String path = options.getFileLocation().getAbsolutePath();
        String ppath = path;
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_TIME_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        NumberFormat twoDigit = new DecimalFormat("00");

        String suffix = path.substring(path.lastIndexOf('.') + 1);
        String basePath = path.substring(0, path.lastIndexOf('.'));

        try {
            if (options.getImageFormat() == ImageFormat.SEQUENCE) {
                int i = 0;
                for (Map.Entry<DataTime, BufferedImage> entry : dtbiHash
                        .entrySet()) {
                    i++;
                    BufferedImage bi = entry.getValue();
                    if (options.getDateTimeSelection() == DateTimeSelection.DATETIME) {
                        DataTime key = entry.getKey();
                        Date validTime = key.getValidTimeAsDate();
                        if (validTime != null && !isFakeTime(key)) {
                            path = basePath + "-" + sdf.format(validTime) + "."
                                    + suffix;
                            if (path.equals(ppath)) {
                                path = basePath + "-" + sdf.format(validTime)
                                        + "-" + twoDigit.format(i).toString()
                                        + "." + suffix;
                            }
                        } else {
                            path = basePath + "-"
                                    + twoDigit.format(i).toString() + "."
                                    + suffix;
                        }
                    } else if (dtbiHash.size() > 1) {
                        path = basePath + "-" + twoDigit.format(i).toString()
                                + "." + suffix;
                    } else {
                        path = basePath + "." + suffix;
                    }
                    ppath = path;
                    writeImage(new File(path), bi);
                    if (monitor.isCanceled()) {
                        dtbiHash.clear();
                        break;
                    }
                    monitor.worked(1);
                }
                dtbiHash.clear();
            } else if (options.getImageFormat() == ImageFormat.ANIMATION) {
                writeAnimation(monitor);
            } else {
                String reason = "Unrecognized format "
                        + String.valueOf(options.getImageFormat());
                statusHandler.handle(Priority.PROBLEM, reason);
            }
        } catch (IOException e) {
            String reason = "Error occurred while writing image";
            statusHandler.handle(Priority.PROBLEM, reason, e);
        }
        return Status.OK_STATUS;
    }

    protected abstract void writeImage(File file, BufferedImage image)
            throws IOException;

    protected abstract void writeAnimation(IProgressMonitor monitor)
            throws IOException;

    /**
     * There may be cases in which a valid time is not associated with a frame.
     * In such cases, a valid time is set to a number of milliseconds from the
     * Epoch time based on the frame number. Here we check if that is one of
     * those such cases.
     * 
     * @param a
     *            DataTime
     * @return true if the DataTime is close to the Epoch time
     */
    public boolean isFakeTime(DataTime dt) {
        return dt.getValidTime().getTimeInMillis() < 1000;
    }

}