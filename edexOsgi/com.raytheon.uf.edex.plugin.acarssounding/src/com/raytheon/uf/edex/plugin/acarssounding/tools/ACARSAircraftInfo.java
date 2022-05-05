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
package com.raytheon.uf.edex.plugin.acarssounding.tools;

import java.io.Serializable;

/**
 * Contains metadata about the ACARS data for a single tail number.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2010            jkorman     Initial creation
 * Aug 18, 2014 3530       bclement    removed ISerializableObject
 * Aug 08, 2016 5757       nabowle     Add javadoc.
 * 
 * </pre>
 * 
 * @author jkorman
 */
public class ACARSAircraftInfo implements Serializable {

    private static final long serialVersionUID = 1L;

    private String tailNumber;

    private String filePath;

    private long startTime = Long.MAX_VALUE;

    private long stopTime = Long.MIN_VALUE;

    /**
     *
     */
    public ACARSAircraftInfo() {
    }

    /**
     *
     * @param tailNumber
     * @param filePath
     */
    public ACARSAircraftInfo(String tailNumber, String filePath) {
        this.tailNumber = tailNumber;
        this.filePath = filePath;
    }

    /**
     *
     * @return
     */
    public String getFilePath() {
        return filePath;
    }

    /**
     *
     * @param filePath
     */
    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    /**
     *
     * @return
     */
    public String getTailNumber() {
        return tailNumber;
    }

    /**
     *
     * @param tailNumber
     */
    public void setTailNumber(String tailNumber) {
        this.tailNumber = tailNumber;
    }

    /**
     * @return the startTime
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * @param startTime the startTime to set
     */
    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the stopTime
     */
    public long getStopTime() {
        return stopTime;
    }

    /**
     * @param stopTime the stopTime to set
     */
    public void setStopTime(long stopTime) {
        this.stopTime = stopTime;
    }

}
