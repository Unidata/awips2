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
package com.raytheon.viz.pointdata;

import com.raytheon.uf.common.time.DataTime;

/**
 * Object which PlotResource2 is expecting when update alerts arrive
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PlotInfo {

    public Integer id;

    public String stationId;

    public Double latitude;

    public Double longitude;

    public DataTime dataTime;

    public String dataURI;

    public PlotData pdv;

    /**
     * whether or not the plot has already been queued for data retrieval and
     * plot creation
     */
    public boolean plotQueued;

    /**
     * whether or not the plot has already been queued for sampling
     */
    public boolean sampleQueued;

    public PlotInfo() {

    }

    public PlotInfo(Integer id, String stationId, Double latitude,
            Double longitude, DataTime dataTime, String dataURI) {
        this.id = id;
        this.stationId = stationId;
        this.latitude = latitude;
        this.longitude = longitude;
        this.dataTime = dataTime;
        this.dataURI = dataURI;
        this.plotQueued = false;
        this.sampleQueued = false;
    }
}
