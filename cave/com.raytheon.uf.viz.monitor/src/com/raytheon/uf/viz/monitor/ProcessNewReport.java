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
package com.raytheon.uf.viz.monitor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * The ProcessNewReport abstract class contains the business logic to process a
 * report that may be a SAFESEAS, SNOW, or Fog Monitor report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009 1999       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public abstract class ProcessNewReport {

    // The time of the threat
    protected Date threatTime = ObUtil.getThreatTime();

    // The drop time
    protected Date dropTime = ObUtil.getDropTime();

    // The latest observation time
    protected Date latestObTime = ObUtil.getDropTime();

    // The frozen precipitation time
    protected Date latestFrozenPrecipTime = ObUtil.getDropTime();

    // The time of the observation
    protected Date obTime;

    // New data received indicator
    protected boolean newDataReceived = false;

    // The threat level
    protected ObConst.ThreatLevel threatLevel = ObConst.ThreatLevel.GRAY;

    // Frozen precipitation string
    protected String frozenPrecip = "";

    // AlertViz (f/k/a Guardian) frozen precipitation string
    protected String guardianFrozenPrecip = "";

    // Present weather vector
    protected List<String> presentWxVec = new ArrayList<String>();

    // Intensity character vector
    protected List<String> intensityCharVec = new ArrayList<String>();

    // Intensity level vector
    protected List<ObConst.IntensityLevel> intensityLevelVec = new ArrayList<ObConst.IntensityLevel>();

}
