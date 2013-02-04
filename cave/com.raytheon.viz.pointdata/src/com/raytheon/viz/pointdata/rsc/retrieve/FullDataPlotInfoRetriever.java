package com.raytheon.viz.pointdata.rsc.retrieve;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.pointdata.PlotInfo;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FullDataPlotInfoRetriever extends AbstractPlotInfoRetriever {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FullDataPlotInfoRetriever.class);

    @XmlAttribute
    private String levelKey;

    @XmlAttribute
    private String parameters;

    @XmlAttribute
    private String stationId = null;

    @XmlAttribute
    private String latitude = "latitude";

    @XmlAttribute
    private String longitude = "longitude";

    @XmlAttribute
    private String validTime = "validTime";

    @XmlAttribute
    private String fcstTime = null;

    private String[] getParameters() {
        List<String> parameters = new ArrayList<String>();
        parameters.addAll(Arrays.asList(this.parameters.split(",")));
        if (stationId != null) {
            parameters.add(stationId);
        }
        parameters.add(latitude);
        parameters.add(longitude);
        parameters.add(validTime);
        if (fcstTime != null) {
            parameters.add(fcstTime);
        }
        parameters.add("dataURI");
        return parameters.toArray(new String[0]);
    }

    @Override
    public void getStations(final IResourceDataChanged listener, DataTime time,
            final HashMap<String, RequestConstraint> metadataMap)
            throws VizException {
        new Job("Retrieving Data") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                String plugin = metadataMap.get("pluginName")
                        .getConstraintValue();
                PointDataContainer pdc = null;
                try {
                    pdc = DataCubeContainer.getPointData(plugin,
                            getParameters(), levelKey, metadataMap);
                } catch (VizException e) {
                    statusHandler.handle(Priority.ERROR,
                            e.getLocalizedMessage(), e);
                    return Status.OK_STATUS;
                }
                if (pdc == null) {
                    return Status.OK_STATUS;
                }

                List<PlotInfo> result = new ArrayList<PlotInfo>(
                        pdc.getCurrentSz());

                for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                    PointDataView pdv = pdc.readRandom(uriCounter);
                    PlotInfo info = new PlotInfo();
                    info.pdv.addData(pdv);
                    if (stationId != null) {
                        info.stationId = pdv.getString(stationId);
                    }
                    info.latitude = pdv.getNumber(latitude).doubleValue();
                    info.longitude = pdv.getNumber(longitude).doubleValue();
                    info.dataURI = pdv.getString("dataURI");
                    Date vTime = new Date(pdv.getLong(validTime));
                    if (fcstTime != null) {
                        info.dataTime = new DataTime(vTime,
                                pdv.getInt(fcstTime));
                    } else {
                        info.dataTime = new DataTime(vTime);
                    }
                    result.add(info);
                }
                listener.resourceChanged(ChangeType.DATA_UPDATE,
                        result.toArray(new PlotInfo[0]));
                return Status.OK_STATUS;
            }

        }.schedule();

    }

}
