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
package com.raytheon.uf.viz.bufrsigwx.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * Generic resource for SigWx data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2009 3099       bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class SigWxResource extends
        AbstractVizResource<SigWxResourceData, MapDescriptor> {

    protected static final String NO_DATA = "No Data Available";

    private Map<DataTime, Collection<SigWxData>> recordsToParse = new HashMap<DataTime, Collection<SigWxData>>();

    private Map<DataTime, PointDataContainer> recordsToDisplay = new HashMap<DataTime, PointDataContainer>();

    protected DataTime displayedDataTime;

    protected IFont font;

    protected SigWxResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(new IResourceDataChanged() {
            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] pdo = (PluginDataObject[]) object;
                    for (PluginDataObject p : pdo) {
                        if (p instanceof SigWxData) {
                            addRecord((SigWxData) p);
                        }
                    }
                }
                issueRefresh();
            }
        });
        this.dataTimes = new ArrayList<DataTime>();
    }

    /**
     * Adds a new record to this resource
     * 
     * @param obj
     */
    protected void addRecord(SigWxData obj) {
        DataTime dataTime = obj.getDataTime();
        Collection<SigWxData> toParse = recordsToParse.get(dataTime);
        if (toParse == null) {
            dataTimes.add(dataTime);
            Collections.sort(this.dataTimes);
            toParse = new ArrayList<SigWxData>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime curDataTime = paintProps.getDataTime();
        if (curDataTime == null) {
            this.displayedDataTime = null;
            return;
        }

        Collection<SigWxData> toParse = recordsToParse.get(curDataTime);
        if (toParse != null && toParse.size() > 0) {
            updateRecords(paintProps.getDataTime());
        }
        this.displayedDataTime = curDataTime;
        PointDataContainer pdc = recordsToDisplay.get(curDataTime);
        if (pdc != null) {
            for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                paintInternal(target, paintProps, pdc.readRandom(uriCounter));
            }
        }
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (font != null) {
            font.dispose();
        }
        this.font = target.initializeFont("Monospace", 10,
                new Style[] { Style.BOLD });
    }

    /**
     * Process records from Records to parse and add the data to the pdc for
     * this dataTime
     * 
     * @param dataTime
     * @throws VizException
     */
    protected void updateRecords(DataTime dataTime) throws VizException {

        RequestConstraint constraint = new RequestConstraint();

        constraint.setConstraintType(ConstraintType.IN);

        for (SigWxData record : recordsToParse.get(dataTime)) {
            constraint.addToConstraintValueList(record.getDataURI());
        }
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("dataURI", constraint);
        // Request the point data
        PointDataContainer pdc = PointDataRequest.requestPointDataAllLevels(
                dataTime, resourceData.getMetadataMap().get("pluginName")
                        .getConstraintValue(), getParameters(), null,
                constraints);
        if (recordsToDisplay.containsKey(dataTime)) {
            recordsToDisplay.get(dataTime).combine(pdc);
        } else {
            recordsToDisplay.put(dataTime, pdc);
        }
        recordsToParse.get(dataTime).clear();
    }

    /**
     * Get the Point Data Parameters to request for this resource
     * 
     * @return
     */
    protected abstract String[] getParameters();

    /**
     * Paint a single set of point data to this resource
     * 
     * @param target
     * @param paintProps
     * @param pdv
     * @throws VizException
     */
    protected abstract void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps, PointDataView pdv) throws VizException;

    /**
     * Determine the appropriate scale to use on both axis for constant sized
     * objects
     * 
     * @param paintProps
     * @return
     */
    protected double[] getScale(PaintProperties paintProps) {
        IExtent extent = paintProps.getView().getExtent();
        Rectangle canvasBounds = paintProps.getCanvasBounds();
        double[] scale = new double[2];
        scale[0] = extent.getWidth() / canvasBounds.width;
        scale[1] = extent.getHeight() / canvasBounds.height;
        return scale;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        StringBuilder result = new StringBuilder();
        PointDataContainer pdc = recordsToDisplay.get(this.displayedDataTime);
        if (pdc != null) {
            for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                String line = inspect(coord, pdc.readRandom(uriCounter));
                if (line != null && !line.isEmpty()) {
                    if (result.length() != 0) {
                        result.append("\n");
                    }
                    result.append(line);
                }
            }
        }
        return result.toString();
    }

    /**
     * Determine if this resource has any info at the specified point
     * 
     * @param coord
     * @param pdv
     * @return
     * @throws VizException
     */
    protected String inspect(ReferencedCoordinate coord, PointDataView pdv)
            throws VizException {
        return "";
    }

    @Override
    public void remove(DataTime dataTime) {
        recordsToDisplay.remove(dataTime);
        recordsToParse.remove(dataTime);
    }

}
