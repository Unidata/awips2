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
package com.raytheon.viz.hydro.flashfloodguidance;

import java.io.File;
import java.util.Date;

/**
 * POJO to store FFG information used by the Flash Flood Guidance dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2016 5483       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class FFGGuidanceData {
    /*
     * Maps to id or type depending on selections in the Flash Flood Guidance
     * dialog.
     */
    private final String identifier;

    private final int duration;

    private final String formattedDuration;

    private final Date dateTime;

    private final String formattedDateTime;

    private File xmrgFile;

    public FFGGuidanceData(String identifier, int duration,
            String formattedDuration, Date dateTime, String formattedDateTime) {
        if (identifier == null) {
            throw new IllegalArgumentException(
                    "Required argument identifier cannot be NULL.");
        }
        this.identifier = identifier;
        this.duration = duration;
        this.formattedDuration = formattedDuration;
        this.dateTime = dateTime;
        this.formattedDateTime = formattedDateTime;
    }

    public String getIdentifier() {
        return identifier;
    }

    public int getDuration() {
        return duration;
    }

    public String getFormattedDuration() {
        return formattedDuration;
    }

    public Date getDateTime() {
        return dateTime;
    }

    public String getFormattedDateTime() {
        return formattedDateTime;
    }

    public File getXmrgFile() {
        return xmrgFile;
    }

    public void setXmrgFile(File xmrgFile) {
        this.xmrgFile = xmrgFile;
    }
}