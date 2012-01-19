package com.raytheon.viz.pointdata.rsc.retrieve;

import java.util.HashMap;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.vividsolutions.jts.geom.Envelope;

public abstract class AbstractPlotInfoRetriever implements ISerializableObject {

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
