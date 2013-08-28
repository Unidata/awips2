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
package com.raytheon.uf.common.archive.config;

import java.io.File;
import java.util.Calendar;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.IOFileFilter;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Filter files based on a file date parsed using the given file date helper.
 * Accept returns true for files that fall between the Start and End times. If
 * start is null, then all after start checks will return true. If end is null,
 * then all before end checks will return true.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013 1965       bgonzale    Initial creation
 * Aug 28, 2013 2299       rferrel     Reject hidden directories.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class FileDateFilter implements IOFileFilter {

    private IFileDateHelper helper;

    private final Calendar start;

    private final Calendar end;

    /**
     * Initialization constructor. This filter uses file last modified time as
     * the filter time.
     * 
     * @param startDate
     * @param endDate
     */
    public FileDateFilter(Calendar start, Calendar end) {
        this(start, end, DEFAULT_FILE_DATE_HELPER);
    }

    /**
     * Initialization constructor.
     * 
     * @param startDate
     * @param endDate
     * @param helper
     */
    public FileDateFilter(Calendar start, Calendar end, IFileDateHelper helper) {
        this.helper = helper == null ? DEFAULT_FILE_DATE_HELPER : helper;
        this.start = start;
        this.end = end;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.commons.io.filefilter.IOFileFilter#accept(java.io.File)
     */
    @Override
    public boolean accept(File file) {
        String filePath = file.getAbsolutePath();
        String dirName = FilenameUtils.getFullPath(filePath);
        String fileName = FilenameUtils.getName(filePath);
        return accept(new File(dirName), fileName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.commons.io.filefilter.IOFileFilter#accept(java.io.File,
     * java.lang.String)
     */
    @Override
    public boolean accept(File dir, String name) {
        File file = new File(dir, name);
        Calendar fileDate = helper.getFileDate(file);
        boolean isAfterEqualsStart = start == null || fileDate.after(start)
                || fileDate.equals(start);
        boolean isBeforeEqualsEnd = end == null || fileDate.before(end)
                || fileDate.equals(end);
        return isAfterEqualsStart && isBeforeEqualsEnd;
    }

    /**
     * This File Date helper returns a file's last modified time.
     */
    private static final IFileDateHelper DEFAULT_FILE_DATE_HELPER = new IFileDateHelper() {
        @Override
        public Calendar getFileDate(File file) {
            // use file last modified date
            long lastModifiedMillis = file.lastModified();
            Calendar result = TimeUtil.newGmtCalendar();
            result.setTimeInMillis(lastModifiedMillis);
            return result;
        }
    };

}
