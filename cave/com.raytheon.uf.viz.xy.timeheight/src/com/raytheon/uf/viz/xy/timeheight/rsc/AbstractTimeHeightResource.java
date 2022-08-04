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
package com.raytheon.uf.viz.xy.timeheight.rsc;

import java.util.List;

import javax.measure.Unit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.AbstractStylePreferences;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineOperation;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.labeling.DataTimeLabel;
import com.raytheon.uf.viz.xy.graph.labeling.DoubleLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor.TimeDirection;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.map.GeoUtil;

import tec.uom.se.AbstractUnit;

/**
 * 
 * Abstract resource for cross sections, TODO: Extract DataCube needs out and
 * into the adapters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 04, 2007  625      njensen   Initial creation
 * Feb 20, 2009  1960     njensen   Refactored to new rsc architecture
 * Feb 14, 2011  8244     bkowal    enabled magnification capability to adjust
 *                                  magnification of x-axis.
 * Jun 18, 2014  3242     njensen   Replaced deprecated calls
 * Aug 15, 2014  3535     njensen   Bigger inset map point
 * Aug 12, 2016  5822     bsteffen  add disposeRenderables for handling data
 *                                  refresh.
 * May 02, 2017  6048     bsteffen  Fix NullPointerException
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Feb 19, 2018  6666     bsteffen  Get data from adapter using loadPreparedData
 * Feb 28, 2018  7231     njensen   Made statusHandler protected
 * 
 * </pre>
 * 
 * @author njensen
 */
public abstract class AbstractTimeHeightResource extends
        AbstractVizResource<TimeHeightResourceData, TimeHeightDescriptor>
        implements IInsetMapResource, IGraphableResource<DataTime, Double> {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    protected static final float DEFAULT_INTERPOLATION = 1.0f;

    protected AbstractVarHeightAdapter<?> adapter;

    protected AbstractStylePreferences prefs;

    protected AbstractTimeHeightResource secondaryResource;

    protected CombineOperation combineOperation;

    protected float[] interpolatedData;

    protected GridGeometry2D geometry = null;

    protected Job loadDataJob = new Job("Loading Time Height Data") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            loadInterpolatedData();
            return Status.OK_STATUS;
        }

        @Override
        public boolean shouldRun() {
            if (interpolatedData != null) {
                this.setName("Updating Time Height Data");
            }
            return super.shouldRun();
        }

    };

    protected String parameter;

    protected String parameterName;

    private String source;

    protected String pointLetter;

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getParameter() {
        return parameter;
    }

    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    public String getParameterName() {
        return parameterName;
    }

    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    public String getPointLetter() {
        return pointLetter;
    }

    public void setPointLetter(String pointLetter) {
        this.pointLetter = pointLetter;
    }

    protected AbstractTimeHeightResource(TimeHeightResourceData data,
            LoadProperties props, AbstractVarHeightAdapter<?> adapter) {
        super(data, props, false);
        this.adapter = adapter;
        ICombinedResourceData combinedResourceData = getResourceData();

        if (combinedResourceData != null) {
            this.secondaryResource = (AbstractTimeHeightResource) combinedResourceData
                    .getSecondaryResource();
            this.combineOperation = combinedResourceData.getCombineOperation();
        }

        data.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE) {
                    synchronized (AbstractTimeHeightResource.this) {
                        PluginDataObject[] pdos = (PluginDataObject[]) object;
                        for (PluginDataObject pdo : pdos) {
                            addRecord(pdo);
                        }
                    }
                }
                issueRefresh();
            }

        });
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        if (secondaryResource != null) {
            secondaryResource.descriptor = this.descriptor;
            secondaryResource.init(target);
        }
        if (prefs == null) {
            try {
                prefs = GraphPrefsFactory
                        .buildPreferences(resourceData.getParameter(), null);
            } catch (StyleException e) {
                throw new VizException(e.getLocalizedMessage(), e);
            }
        }
        if (interpolatedData == null) {
            loadDataJob.schedule();
        }
    }

    protected void loadInterpolatedData() {
        if (geometry == null || descriptor == null
                || descriptor.getGraph(this) == null
                || !descriptor.getGraph(this).isReady()) {
            // try again later
            loadDataJob.schedule(250);
            return;
        }
        if (this.dataTimes.isEmpty()) {
            return;
        }

        DataTime[] myDataTimes = null;
        synchronized (this.dataTimes) {
            myDataTimes = this.dataTimes
                    .toArray(new DataTime[this.dataTimes.size()]);
        }

        IGraph graph = descriptor.getGraph(this);

        float[][] columns = new float[myDataTimes.length][(int) geometry
                .getGridRange2D().getHeight()];
        float[] times = new float[myDataTimes.length];
        TimeDirection direction = this.getDescriptor().getTimeDirection();

        for (int i = 0; i < myDataTimes.length; i++) {
            int dataTimesIndex = (direction != TimeDirection.LEFT_TO_RIGHT ? i
                    : myDataTimes.length - 1 - i);
            DataTime myDataTime = myDataTimes[dataTimesIndex];
            try {
                List<XYData> dataList = adapter.loadPreparedData(myDataTime,
                        getUnit());
                columns[i] = InterpUtils.makeColumn(dataList,
                        (int) geometry.getGridRange2D().getHeight(), graph,
                        descriptor.getHeightScale().getMinVal() < descriptor
                                .getHeightScale().getMaxVal(),
                        Float.NaN);
                times[i] = myDataTime.getValidTime().getTimeInMillis();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading Time Height Data for time "
                                + myDataTime.getLegendString() + ": "
                                + e.getLocalizedMessage(),
                        e);
            }
        }
        interpolatedData = InterpUtils.makeRows(columns, times,
                (int) geometry.getGridRange2D().getWidth(), graph,
                direction == TimeDirection.LEFT_TO_RIGHT, Float.NaN);

        // reset any cached images/contours
        this.disposeRenderables();
        this.issueRefresh();
    }

    @Override
    public Geometry getInsetMapLocation() {
        return IInsetMapResource.factory.createPoint(resourceData.getPoint());
    }

    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        // paint a point
        Coordinate point = resourceData.getPoint();
        double[] pixels = insetMapDescriptor
                .worldToPixel(new double[] { point.x, point.y });
        target.drawPoint(pixels[0], pixels[1], 0.0,
                getCapability(ColorableCapability.class).getColor(),
                PointStyle.STAR, 1.5f);
    }

    public String getUnitString() {
        String unitString = prefs.getDisplayUnitLabel();
        if (unitString == null) {
            return "?";
        }
        return unitString;
    }

    public Unit<?> getUnit() {
        Unit<?> xPrefUnit = prefs == null ? null : prefs.getDisplayUnits();
        if (xPrefUnit != null && xPrefUnit != AbstractUnit.ONE) {
            return xPrefUnit;
        }
        Unit<?> xDataUnit = adapter.getXUnit();
        if (xDataUnit != null && xDataUnit != AbstractUnit.ONE) {
            return xDataUnit;
        }
        return null;
    }

    @Override
    public String getName() {
        if (getStatus() != ResourceStatus.INITIALIZED) {
            return "NO DATA";
        }
        String completeName = "";
        String stnID = "";

        completeName += resourceData.getSource();

        String pointLetter = resourceData.getPointLetter();
        if (pointLetter != null) {
            completeName += " " + "pt" + pointLetter;
        }
        // If point data, include station ID in name
        if (resourceData.getMetadataMap().get("location.stationId") != null) {

            stnID = resourceData.getMetadataMap().get("location.stationId")
                    .getConstraintValue();
            if (stnID == null) {
                stnID = "";
            }
        }
        if (!"".equals(stnID)) {
            completeName += " " + stnID;
        } else {
            if (resourceData.getPointCoordinate() != null) {
                String formattedPoint = GeoUtil
                        .formatCoordinate(resourceData.getPointCoordinate());
                completeName += " " + formattedPoint;
            }
        }

        String parameterName = resourceData.getParameterName();

        completeName += " " + parameterName;
        if (getCapability(DisplayTypeCapability.class)
                .getDisplayType() == DisplayType.IMAGE) {
            completeName += " Img";
        }
        completeName += " (" + getUnitString() + ")";

        if (secondaryResource != null) {
            return ICombinedResourceData.CombineUtil.getName(completeName,
                    secondaryResource.getName(), combineOperation);
        } else {
            return completeName;
        }
    }

    @Override
    protected void disposeInternal() {
        if (secondaryResource != null) {
            secondaryResource.dispose();
        }
        disposeRenderables();
    }

    protected abstract void disposeRenderables();

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (!descriptor.getGraph(this).isReady()) {
            return;
        }

        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();
        descriptor.getGraph(this).setCurrentMagnification(magnification);

        if (geometry == null) {
            IExtent extent = descriptor.getGraph(this).getExtent().clone();
            // To be numerically accurate the grid geometry should be 1 grid
            // cell larger than the graph
            extent.scale(1.01);
            GeneralEnvelope env = new GeneralEnvelope(
                    new double[] { extent.getMinX(), extent.getMinY() },
                    new double[] { extent.getMaxX(), extent.getMaxY() });
            env.setCoordinateReferenceSystem(descriptor.getGridGeometry()
                    .getCoordinateReferenceSystem());
            GeneralGridEnvelope range = new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { 100, 100 }, false);
            geometry = new GridGeometry2D(range, env);
        }
        if (interpolatedData == null) {
            loadDataJob.schedule();
        }
    }

    public void addRecord(PluginDataObject pdo) {
        adapter.addRecord(pdo);
        DataTime pdoTime = pdo.getDataTime().clone();
        pdoTime.setLevelValue(null);
        synchronized (dataTimes) {
            if (dataTimes.add(pdoTime)) {
                if (descriptor != null) {
                    IGraph graph = descriptor.getGraph(this);
                    if (graph != null) {
                        graph.reconstruct();
                    }
                }
            }
        }
        loadDataJob.schedule();
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (dataTimes) {
            dataTimes.remove(dataTime);
        }
        adapter.remove(dataTime);
        if (descriptor != null) {
            IGraph graph = descriptor.getGraph(this);
            if (graph != null) {
                graph.reconstruct();
            }
        }
        // Should clear out cached images and such.
        loadDataJob.schedule();
    }

    @Override
    public void setDescriptor(TimeHeightDescriptor descriptor) {
        adapter.setHeightScale(descriptor.getHeightScale());
        if (this.secondaryResource != null) {
            secondaryResource.setDescriptor(descriptor);
        }
        super.setDescriptor(descriptor);
        interpolatedData = null;
        loadDataJob.schedule();
    }

    /**
     * @param sliceData
     * @param sliceData2
     * @return
     */
    protected float[] combineResourceData(Object sliceData, Object sliceData2) {

        float[] data1 = (float[]) sliceData;
        float[] data2 = (float[]) sliceData2;
        float[] combinedData = new float[data1.length];

        for (int i = 0; i < combinedData.length; i++) {
            if (this.combineOperation == CombineOperation.DIFFERENCE) {
                float n1 = data1[i];
                float n2 = data2[i];
                combinedData[i] = n1 - n2;
            }
        }

        return combinedData;
    }

    @Override
    public boolean isTimeAgnostic() {
        return true;
    }

    @Override
    public Object getGraphKey() {
        return AbstractTimeHeightResource.class;
    }

    @Override
    public DataTimeLabel[] getXRangeData() {
        DataTimeLabel[] labels = null;
        synchronized (dataTimes) {
            labels = new DataTimeLabel[dataTimes.size()];
            int i = 0;
            for (DataTime time : dataTimes) {
                labels[i] = new DataTimeLabel(time);
                i += 1;
            }
        }
        return labels;
    }

    @Override
    public IGraphLabel<Double>[] getYRangeData() {
        HeightScale heightScale = descriptor.getHeightScale();
        double min = heightScale.getMinVal();
        double max = heightScale.getMaxVal();
        return new DoubleLabel[] { new DoubleLabel(min), new DoubleLabel(max) };
    }

    @Override
    public void redraw() {
        // we need to trigger recalculating contours and images
        loadDataJob.schedule();
    }

}
