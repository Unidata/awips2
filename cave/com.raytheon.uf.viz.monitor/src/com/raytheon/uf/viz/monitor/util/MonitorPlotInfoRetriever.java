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
package com.raytheon.uf.viz.monitor.util;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever;

/**
 * MonitorPlotInfoRetriever filters stations for specific CWA.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2010 6920       skorolev     Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class MonitorPlotInfoRetriever extends PointDataPlotInfoRetriever {

    @Override
    protected void addColumns(DbQuery dq) {
        dq.addConstraint("location.stationId", ConstraintType.IN,
                MonitoringArea.getPlatformIdList());
        super.addColumns(dq);
    }

}
