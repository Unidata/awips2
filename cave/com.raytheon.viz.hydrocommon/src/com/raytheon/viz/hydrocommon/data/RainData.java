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

import com.raytheon.uf.common.dataplugin.shef.tables.Curpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;

/**
 * Rain Data object holding lists of {@link Curpp} and {@linkCurpc} data.
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

public class RainData {

    private List<Curpp> ppList = new ArrayList<>();

    private List<Curpc> pcList = new ArrayList<>();

    public List<Curpp> getPpList() {
        return ppList;
    }

    public void setPpList(List<Curpp> ppList) {
        this.ppList = ppList;
    }

    public List<Curpc> getPcList() {
        return pcList;
    }

    public void setPcList(List<Curpc> pcList) {
        this.pcList = pcList;
    }
}
