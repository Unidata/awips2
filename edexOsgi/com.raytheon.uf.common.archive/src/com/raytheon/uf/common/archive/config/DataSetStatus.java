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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

/**
 * This class used by IFileDateHelper to contains additional information about a
 * file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2013  2603      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class DataSetStatus {

    /** The file the status is for. */
    private final File file;

    /** Set to true when file is contains in a data set. */
    private boolean inDataSet = false;

    private final List<String> displayLabels = new ArrayList<String>(1);

    /** The file's time based on IFileDataHelper. */
    private Calendar time = null;

    /**
     * The constructor with default values set.
     * 
     * @param file
     *            should not be null.
     */
    DataSetStatus(File file) {
        this.file = file;
    }

    /**
     * The file the information is for.
     * 
     * @return file
     */
    public File getFile() {
        return file;
    }

    /**
     * 
     * @return true when file is in a data set.
     */
    public boolean isInDataSet() {
        return inDataSet;
    }

    /**
     * Set data set status.
     * 
     * @param inDataSet
     */
    public void setInDataSet(boolean inDataSet) {
        this.inDataSet = inDataSet;
    }

    /**
     * 
     * @return non-null only when file is in a data set.
     */
    public List<String> getDisplayLabels() {
        return displayLabels;
    }

    /**
     * Set the select status. Should only be true when in a data set.
     * 
     * @param isSelected
     */
    public void addDisplayLabel(String displayLabel) {
        this.displayLabels.add(displayLabel);
    }

    /**
     * The file's time
     * 
     * @return time
     */
    public Calendar getTime() {
        return time;
    }

    /**
     * Set the file's time.
     * 
     * @param time
     */
    public void setTime(Calendar time) {
        this.time = time;
    }
}
