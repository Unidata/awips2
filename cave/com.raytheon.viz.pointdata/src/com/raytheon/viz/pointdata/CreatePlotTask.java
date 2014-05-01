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

import com.raytheon.uf.common.pointdata.PointDataView;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CreatePlotTask {

    private PlotInfo[] infos;

    private PointDataView pdv;

    public CreatePlotTask(PlotInfo[] infos, PointDataView pdv) {
        this.pdv = pdv;
        this.infos = infos;
    }

    public PlotInfo[] getInfos() {
        return infos;
    }

    public void setInfos(PlotInfo[] infos) {
        this.infos = infos;
    }

    public PointDataView getPdv() {
        return pdv;
    }

    public void setPdv(PointDataView pdv) {
        this.pdv = pdv;
    }

}
