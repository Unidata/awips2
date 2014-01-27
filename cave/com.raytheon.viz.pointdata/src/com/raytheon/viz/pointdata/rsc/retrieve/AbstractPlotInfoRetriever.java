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

import java.util.HashMap;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.PlotResource2;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Base class for all objects which can retrieve {@link PlotInfo} objects for
 * use in {@link PlotResource2}. For most applications the
 * {@link PointDataPlotInfoRetriever} should be used however other instances can
 * provide more advanced features such as incremental loading or retrieving
 * additional data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 23, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public abstract class AbstractPlotInfoRetriever {

    public abstract void getStations(IResourceDataChanged listener,
            DataTime time, HashMap<String, RequestConstraint> metadataMap)
            throws VizException;

    public void updateActiveFrame(DataTime time, Envelope envelope,
            CoordinateReferenceSystem coordinateReferenceSystem) {
        ;// Do nothing by default
    }

    public void cancel() {
        ;// Do nothing by default
    }

}
