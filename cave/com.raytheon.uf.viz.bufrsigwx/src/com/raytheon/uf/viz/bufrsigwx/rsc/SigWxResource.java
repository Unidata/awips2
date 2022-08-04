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
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
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
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * Generic resource for SigWx data
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 28, 2009  3099     bsteffen  Initial creation
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Feb 04, 2016  5309     tgurney   Remove dependency on dataURI
 * Sep 12, 2016  5886     tgurney   Update paintInternal signature, add
 *                                  needsUpdate flag, fix point data container
 *                                  iteration.
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * 
 * </pre>
 *
 * @author bsteffen
 */
public abstract class SigWxResource
        extends AbstractVizResource<SigWxResourceData, MapDescriptor> {

    protected static final String NO_DATA = "No Data Available";

    private Map<DataTime, Collection<SigWxData>> recordsToParse = new HashMap<>();

    private Map<DataTime, PointDataContainer> recordsToDisplay = new HashMap<>();

    protected DataTime displayedDataTime;

    protected IFont font;

    protected DataTime prevDataTime = null;

    private boolean needsUpdate = true;

    protected SigWxResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        resourceData.addChangeListener((ChangeType type, Object object) -> {
            if (type == ChangeType.DATA_UPDATE) {
                PluginDataObject[] pdo = (PluginDataObject[]) object;
                for (PluginDataObject p : pdo) {
                    if (p instanceof SigWxData) {
                        addRecord((SigWxData) p);
                    }
                }
            }
            setUpdateNeeded(true);
            issueRefresh();
        });
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
            toParse = new ArrayList<>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(obj);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime curDataTime = paintProps.getDataTime();
        if (prevDataTime == null || !prevDataTime.equals(curDataTime)) {
            setUpdateNeeded(true);
        }
        prevDataTime = curDataTime;
        if (curDataTime == null) {
            this.displayedDataTime = null;
            return;
        }

        Collection<SigWxData> toParse = recordsToParse.get(curDataTime);
        if (toParse != null && !toParse.isEmpty()) {
            updateRecords(paintProps.getDataTime());
        }
        this.displayedDataTime = curDataTime;
        PointDataContainer pdc = recordsToDisplay.get(curDataTime);
        if (pdc != null) {
            paintInternal(target, paintProps, pdc);
        }
        setUpdateNeeded(false);
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
        this.font = target.initializeFont("Monospace", 8,
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
        // Request the point data
        PointDataContainer pdc = PointDataRequest.requestPointDataAllLevels(
                dataTime,
                resourceData.getMetadataMap().get("pluginName")
                        .getConstraintValue(),
                getParameters(), null, resourceData.getMetadataMap());
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
     * @param pdc
     * @throws VizException
     */
    protected abstract void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps, PointDataContainer pdc)
            throws VizException;

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
            for (int uriCounter = 0; uriCounter < pdc
                    .getCurrentSz(); uriCounter++) {
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

    public boolean isUpdateNeeded() {
        return needsUpdate;
    }

    public void setUpdateNeeded(boolean needsUpdate) {
        this.needsUpdate = needsUpdate;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        setUpdateNeeded(true);
        issueRefresh();
    }
}
