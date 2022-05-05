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
package com.raytheon.uf.common.mpe.dqcpreprocessor;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Container for DQC PreProcessor configuration properties that were originally
 * specified via the command-line. Will use the default when EDEX runs the DQC
 * PreProcessor. Can also be set via CAVE when the DQC PreProcessor is manually
 * ran on-demand.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2018 7184       bkowal      Initial creation
 * Apr 06, 2018 7184       bkowal      Relocated to common.
 *
 * </pre>
 *
 * @author bkowal
 */
@DynamicSerialize
public class DQCPreProcRunConfiguration {

    public static final int DEFAULT_NUM_DAYS = 10;

    @DynamicSerializeElement
    private int numDays = DEFAULT_NUM_DAYS;

    @DynamicSerializeElement
    private Calendar runDate = TimeUtil.newGmtCalendar();

    @DynamicSerializeElement
    private List<String> areas = null;

    @DynamicSerializeElement
    private boolean setZero = false;

    public DQCPreProcRunConfiguration() {
    }

    /**
     * @return the numDays
     */
    public int getNumDays() {
        return numDays;
    }

    /**
     * @param numDays
     *            the numDays to set
     */
    public void setNumDays(int numDays) {
        this.numDays = numDays;
    }

    /**
     * @return the runDate
     */
    public Calendar getRunDate() {
        return runDate;
    }

    /**
     * @param runDate
     *            the runDate to set
     */
    public void setRunDate(Calendar runDate) {
        this.runDate = runDate;
    }

    /**
     * @return the subAreas
     */
    public List<String> getAreas() {
        return areas;
    }

    /**
     * @param areas
     *            the subAreas to set. Setting to {@code null} will include ALL
     *            sub areas.
     */
    public void setAreas(List<String> areas) {
        this.areas = areas;
    }

    /**
     * @return the setZero
     */
    public boolean isSetZero() {
        return setZero;
    }

    /**
     * @param setZero
     *            the setZero to set
     */
    public void setSetZero(boolean setZero) {
        this.setZero = setZero;
    }
}