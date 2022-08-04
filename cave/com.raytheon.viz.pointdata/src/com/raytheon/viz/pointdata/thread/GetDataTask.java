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
package com.raytheon.viz.pointdata.thread;

import java.util.List;

import com.raytheon.viz.pointdata.PlotInfo;

/**
 * A task to retrieve plot data for a set of stations, possibly requesting the
 * parameters necessary for the plot image or the sample text or both.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            njensen     Initial creation
 * Mar 21, 2014 2868       njensen     Improved javadoc
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GetDataTask {

    public enum Params {
        PLOT_ONLY, SAMPLE_ONLY, PLOT_AND_SAMPLE
    };

    private List<PlotInfo[]> stations;

    private Params requestType;

    public GetDataTask(List<PlotInfo[]> stations, Params params) {
        this.stations = stations;
        this.requestType = params;
    }

    public Params getRequestType() {
        return requestType;
    }

    public void setRequestType(Params requestType) {
        this.requestType = requestType;
    }

    public List<PlotInfo[]> getStations() {
        return stations;
    }

    public void setStations(List<PlotInfo[]> stations) {
        this.stations = stations;
    }

}
