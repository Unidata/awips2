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
package com.raytheon.viz.hydrocommon.data;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;

/**
 * River Data structure.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2018  7379      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class RiverData {
    private List<Observation> obsHeightList = new ArrayList<>();

    private List<Observation> obsDischargeList = new ArrayList<>();

    private List<Observation> observationList = new ArrayList<>();

    private List<Riverstatus> riverStatusList = new ArrayList<>();

    public List<Observation> getObsHeightList() {
        return obsHeightList;
    }

    public void setObsHeightList(List<Observation> obsHeightList) {
        this.obsHeightList = obsHeightList;
    }

    public List<Observation> getObsDischargeList() {
        return obsDischargeList;
    }

    public void setObsDischargeList(List<Observation> obsDischargeList) {
        this.obsDischargeList = obsDischargeList;
    }

    public List<Observation> getObservationList() {
        return observationList;
    }

    public void setObservationList(List<Observation> observationList) {
        this.observationList = observationList;
    }

    public List<Riverstatus> getRiverStatusList() {
        return riverStatusList;
    }

    public void setRiverStatusList(List<Riverstatus> riverStatusList) {
        this.riverStatusList = riverStatusList;
    }

}
