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
package com.raytheon.viz.pointdata.rsc.retrieve;

import java.util.Map;

import org.locationtech.jts.geom.Envelope;
import org.geotools.api.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.PlotResource;

/**
 * Base class for all objects which can retrieve {@link PlotInfo} objects for
 * use in {@link PlotResource}. For most applications the
 * {@link PointDataPlotInfoRetriever} should be used however other instances can
 * provide more advanced features such as incremental loading or retrieving
 * additional data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 23, 2013  2491     bsteffen  Remove ISerializableObject
 * Jun 06, 2014  2061     bsteffen  Remove old PlotResource
 * Dec 07, 2021  8341     randerso  Move plot performance logging into perf log.
 *
 * </pre>
 *
 * @author unknown
 */
public abstract class AbstractPlotInfoRetriever {
    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    protected static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("PlotInfoRetrieval");

    public abstract void getStations(IResourceDataChanged listener,
            DataTime time, Map<String, RequestConstraint> metadataMap)
            throws VizException;

    public void updateActiveFrame(DataTime time, Envelope envelope,
            CoordinateReferenceSystem coordinateReferenceSystem) {
        // Do nothing by default
    }

    public void cancel() {
        // Do nothing by default
    }

}
