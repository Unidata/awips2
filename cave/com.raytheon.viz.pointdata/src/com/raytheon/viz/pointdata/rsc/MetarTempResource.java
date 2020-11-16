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
package com.raytheon.viz.pointdata.rsc;

import java.awt.Font;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;

import javax.measure.unit.Unit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.pointdata.rsc.progdisc.GenericProgressiveDisclosure;
import com.raytheon.viz.pointdata.rsc.progdisc.GenericProgressiveDisclosure.PlotItem;
import com.raytheon.viz.pointdata.util.MetarTempDataContainer;
import com.raytheon.viz.pointdata.util.MetarTempDataContainer.TempData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Resource for displaying colored Metar Temp values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 05, 2016           mjames   Copied from MetarPrecipResource
 * Feb 06, 2019           mjames   Updated for reprojection, default color map.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MetarTempResource extends AbstractVizResource<MetarTempResourceData, IMapDescriptor> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetarTempResource.class);

	private RGB color = new RGB(126, 126, 126);
    
    public RGB getColorByValue(float value) {
    	ColorMapParameters parameters = getCapability(ColorMapCapability.class).getColorMapParameters();
        if (parameters != null) {
            Color color = parameters.getColorByValue(value);
            if (color != null) {
                return new RGB((int) (color.getRed() * 255),
                        (int) (color.getGreen() * 255),
                        (int) (color.getBlue() * 255));
            }
        }
        return getColor();
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(RGB color) {
        this.color = color;
    }
    private static final int PLOT_PIXEL_SIZE = 30;

    private class RenderableTempData extends TempData implements PlotItem {

        public final DrawableString string;

        public RenderableTempData(TempData data, DrawableString string) {
            super(data.getTimeObs(), data.getStationName(),
                    data.getTempValue(), data.getLatLon().x, data.getLatLon().y);
            this.string = string;
        }

        @Override
        public Coordinate getLocation() {
            return new Coordinate(string.basics.x, string.basics.y);
        }

    }

    // To avoid synchronization issues with data request, updates, and removals
    // do it all on this thread.
    private Job dataProcessJob = new Job("Loading Sfc Temps") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            processNewFrames(monitor);
            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }
            processUpdates(monitor);

            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }
            processRemoves();
            return Status.OK_STATUS;
        }

    };

    private LinkedBlockingQueue<PluginDataObject> updates = new LinkedBlockingQueue<PluginDataObject>();

    private LinkedBlockingQueue<DataTime> removes = new LinkedBlockingQueue<DataTime>();

    private boolean reproject = false;

    private Map<DataTime, GenericProgressiveDisclosure<RenderableTempData>> data = new HashMap<>();

    private IFont font = null;

    protected MetarTempResource(MetarTempResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);

    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }
        dataProcessJob.cancel();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            return;
        }
        GenericProgressiveDisclosure<RenderableTempData> disclosure = null;
        synchronized (data) {
            disclosure = data.get(time);
        }
        if (disclosure == null) {
            dataProcessJob.schedule();
            return;
        }
        IExtent extent = paintProps.getView().getExtent();
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();
        Double density = getCapability(DensityCapability.class).getDensity();
        double threshold = (PLOT_PIXEL_SIZE * magnification) / density;
        threshold = (threshold * extent.getWidth())
                / paintProps.getCanvasBounds().width;
        Collection<RenderableTempData> temps = disclosure.runDisclosure(
                extent, threshold);
        if (temps.isEmpty()) {
            return;
        }

        if (font == null) {
            font = target.initializeFont(Font.DIALOG, 8,
                    new Style[] { Style.BOLD });
            font.setMagnification(magnification.floatValue());
        }

        List<DrawableString> strings = new ArrayList<DrawableString>();

        for (RenderableTempData data : temps) {
            data.string.font = this.font;
            strings.add(data.string);
        }

        target.drawStrings(strings);
    }

    private List<RenderableTempData> getTempsData(DataTime time) {
        GenericProgressiveDisclosure<RenderableTempData> currData = null;
        synchronized (data) {
            currData = data.get(time);
        }
        if (currData != null) {
            return currData.getAll();
        }
        return null;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        dataProcessJob.schedule();
        ColorMapParameters params = new ColorMapParameters();

        try {
            params.setColorMap(ColorMapLoader.loadColorMap("Grid/Gridded Data"));
        } catch (ColorMapException e) {
            throw new VizException(e);
        }

        DataMappingPreferences preferences = new DataMappingPreferences();

        DataMappingEntry entry = new DataMappingEntry();
        entry.setDisplayValue(-50.);
        entry.setPixelValue(0.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(-40.);
        entry.setPixelValue(1.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(-30.);
        entry.setPixelValue(2.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(-20.);
        entry.setPixelValue(3.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(-10.);
        entry.setPixelValue(4.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(0.0);
        entry.setPixelValue(5.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(10.);
        entry.setPixelValue(6.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(20.);
        entry.setPixelValue(7.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(30.);
        entry.setPixelValue(8.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(40.);
        entry.setPixelValue(9.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(50.);
        entry.setPixelValue(10.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(60.);
        entry.setPixelValue(11.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(70.);
        entry.setPixelValue(12.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(80.);
        entry.setPixelValue(13.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(90.);
        entry.setPixelValue(14.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(100.);
        entry.setPixelValue(15.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(110.);
        entry.setPixelValue(16.0);
        preferences.addEntry(entry);


        params.setDisplayUnit(Unit.ONE);
        params.setDataMapping(preferences);
        params.setColorMapMin(0);
        params.setColorMapMax(17);

        getCapability(ColorMapCapability.class).setColorMapParameters(params);
        
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object object) {
        super.resourceDataChanged(type, object);
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                if (font != null) {
                    font.dispose();
                    font = null;
                }
            }
            issueRefresh();
        } else if (type == ChangeType.DATA_UPDATE) {
            if (object instanceof PluginDataObject[]) {
                PluginDataObject[] pdos = (PluginDataObject[]) object;
                for (PluginDataObject pdo : pdos) {
                    updates.offer(pdo);
                    dataProcessJob.schedule();
                }
            }
        }
    }

    @Override
    public String getName() {
    	return "Surface Temps [F]";
    }

    @Override
    public void remove(DataTime dataTime) {
        // This will be handled asynchronously by the update job
        removes.offer(dataTime);
        dataProcessJob.schedule();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        // This will be handled asynchronously by the update job
        reproject = true;
        dataProcessJob.schedule();
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Coordinate pixel = null;
        try {
            pixel = coord.asPixel(descriptor.getGridGeometry());
        } catch (TransformException e) {
            throw new VizException(e);
        } catch (FactoryException e) {
            throw new VizException(e);
        }

        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        List<RenderableTempData> temps = getTempsData(descriptor
                .getTimeForResource(this));

        if ((temps == null) || temps.isEmpty()) {
            return null;
        }

        IRenderableDisplay rDisplay = descriptor.getRenderableDisplay();

        double bestDist = PLOT_PIXEL_SIZE * magnification;
        if (rDisplay != null) {
            bestDist *= rDisplay.getView().getExtent().getWidth()
                    / rDisplay.getBounds().width;
        } else {
            bestDist *= 100;
        }

        TempData bestData = null;

        for (RenderableTempData temp : temps) {
            double xDist = temp.string.basics.x - pixel.x;
            double yDist = temp.string.basics.y - pixel.y;
            double dist = Math.hypot(xDist, yDist);
            if (dist < bestDist) {
                bestDist = dist;
                bestData = temp;
            }
        }
        if (bestData != null) {
            return bestData.getStationName();
        }
        return "No Data";
    }

    /**
     *
     * Re-project to the new crs. Returns true to reload the frame.
     *
     * @return boolean
     */
    private boolean processReproject() {

        if (reproject) {
            reproject = false;

            GridEnvelope2D envelope = GridGeometry2D.wrap(
                    descriptor.getGridGeometry()).getGridRange2D();

            synchronized (data) {
                for (GenericProgressiveDisclosure<RenderableTempData> disclosure : data
                        .values()) {
                    List<RenderableTempData> dataList = disclosure.getAll();
                    Iterator<RenderableTempData> it = dataList.iterator();
                    while (it.hasNext()) {
                        RenderableTempData temp = it.next();
                        Coordinate latLon = temp.getLatLon();
                        double[] px = descriptor.worldToPixel(new double[] {
                                latLon.x, latLon.y });
                        if (envelope.contains(px[0], px[1])) {
                            temp.string.setCoordinates(px[0], px[1], px[2]);
                        } else {
                            it.remove();
                        }
                    }
                }
            }

            return true;
        }
        issueRefresh();
        return false;
    }

    private void processRemoves() {
        synchronized (data) {
            while (!removes.isEmpty()) {
                DataTime toRemove = removes.poll();
                this.dataTimes.remove(toRemove);
                this.data.remove(toRemove);
            }
        }
    }

    private void processUpdates(IProgressMonitor monitor) {
        if (updates.isEmpty()) {
            return;
        }
        HashMap<String, RequestConstraint> rcMap = resourceData
                .getMetadataMap();
        rcMap = new HashMap<String, RequestConstraint>(rcMap);
        RequestConstraint rc = new RequestConstraint(null, ConstraintType.IN);
        long earliestTime = Long.MAX_VALUE;
        Set<String> newStations = new HashSet<String>();
        // Get the envelope and math transform to ensure we only bother
        // processing updates on screen.
        MathTransform toDescriptor = null;
        try {
            toDescriptor = MapUtil.getTransformFromLatLon(descriptor.getCRS());
        } catch (FactoryException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error processing updates for MetarTemps, Ignoring all updates.",
                            e);
            updates.clear();
            return;
        }

        Envelope2D envelope = new Envelope2D(descriptor.getGridGeometry()
                .getEnvelope());

        while (!updates.isEmpty()) {
            PluginDataObject pdo = updates.poll();
            try {
                Map<String, Object> map = DataURIUtil.createDataURIMap(pdo);
                double lon = ((Number) map.get("location.longitude")).doubleValue();
                double lat = ((Number) map.get("location.latitude")).doubleValue();
                DirectPosition2D dp = new DirectPosition2D(lon, lat);
                toDescriptor.transform(dp, dp);
                if (envelope.contains((DirectPosition) dp)) {
                    newStations.add(map.get("location.stationId").toString());
                    long validTime = pdo.getDataTime().getMatchValid();
                    if (validTime < earliestTime) {
                        earliestTime = validTime;
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                		"Error processing updates for MetarTemps, Ignoring an update.", e);
            }
        }
        
        if (newStations.isEmpty()) {
            return;
        }
        
        rc.setConstraintValueList(newStations.toArray(new String[0]));
        
        rcMap.put("location.stationId", rc);
        
        MetarTempDataContainer container = new MetarTempDataContainer(
                rcMap);
        
        for (Entry<DataTime, GenericProgressiveDisclosure<RenderableTempData>> entry : data
                .entrySet()) {
            DataTime time = entry.getKey();
            if (time.getMatchValid() < earliestTime) {
                // No need to reprocess times after the earliest update.
                continue;
            }
            GenericProgressiveDisclosure<RenderableTempData> newValue = new GenericProgressiveDisclosure<>();
            for (RenderableTempData data : entry.getValue().getAll()) {
                if (!newStations.contains(data.getStationName())) {
                    newValue.add(data);
                }
            }
            entry.setValue(newValue);
            addData(time, container.getBaseTempData(time));
            if (monitor.isCanceled()) {
                return;
            }
        }
    }

    private void processNewFrames(IProgressMonitor monitor) {
        // load data in two steps, first load base data then any derived data.
        // Always try to load the current frame, then nearby frames.
        MetarTempDataContainer container = new MetarTempDataContainer(
                resourceData.getMetadataMap(),
                descriptor.getGridGeometry().getEnvelope());
        Set<DataTime> reprojectedFrames = new HashSet<DataTime>();
        Set<DataTime> baseOnly = new HashSet<DataTime>();
        boolean modified = true;
        while (modified) {
            // don't want to miss a reproject if retrieval takes awhile.
        	
            if (processReproject()) {
                // We must create a new container and re request all the data
                // for the new area.
            	data.clear();
                reprojectedFrames = new HashSet<DataTime>(data.keySet());
                container = new MetarTempDataContainer(
                        resourceData.getMetadataMap(), descriptor
                                .getGridGeometry().getEnvelope());
            }
            
            if (monitor.isCanceled()) {
                return;
            }
            modified = false;
            // If the current frame changes while we are processing we will
            // begin requesting data for the new frame
            FramesInfo frameInfo = descriptor.getFramesInfo();
            DataTime[] times = frameInfo.getTimeMap().get(
                    MetarTempResource.this);
            if (times == null) {
                return;
            }
            int curIndex = frameInfo.getFrameIndex();
            int count = frameInfo.getFrameCount();
            // This will generate the number series 0, -1, 1, -2, 2, -3, 3...
            for (int i = 0; i < ((count / 2) + 1); i = i < 0 ? -i : -i - 1) {
                int index = (count + curIndex + i) % count;
                DataTime next = times[index];
                if (next != null) {
                    if (!data.containsKey(next)
                            || reprojectedFrames.contains(next)) {
                        List<TempData> baseData = container
                                .getBaseTempData(next);
                        addData(next, baseData);
                        baseOnly.add(next);
                        modified = true;
                        break;
                    }
                    if (baseOnly.contains(next)) {
                        List<TempData> derivedData = container
                                .getBaseTempData(next);
                        addData(next, derivedData);
                        baseOnly.remove(next);
                        reprojectedFrames.remove(next);
                        modified = true;
                        break;
                    }
                }
            }
            issueRefresh();
        }

        synchronized (data) {
            // This will only happen if frames were removed while we were
            // processing. Don't leave any half created frames
            for (DataTime time : baseOnly) {
                this.dataTimes.remove(time);
                this.data.remove(time);
            }
        }
    }

    private void addData(DataTime time, List<TempData> temps) {
        if (temps.isEmpty()) {
            if (dataTimes.add(time)) {
                GenericProgressiveDisclosure<RenderableTempData> disclosure = 
                		new GenericProgressiveDisclosure<RenderableTempData>();
                synchronized (data) {
                    data.put(time, disclosure);
                }
            }
        }
        if (data.containsKey(time)) {
            temps = new ArrayList<TempData>(temps);
            temps.addAll(getTempsData(time));
        }
        Collections.sort(temps, new Comparator<TempData>() {

            @Override
            public int compare(TempData o1, TempData o2) {
                return o2.getTempValue().compareTo(o1.getTempValue());
            }

        });

        GenericProgressiveDisclosure<RenderableTempData> newTemps = new GenericProgressiveDisclosure<RenderableTempData>();

        //RGB color = getCapability(ColorableCapability.class).getColor();

        GridEnvelope2D envelope = GridGeometry2D.wrap(
                descriptor.getGridGeometry()).getGridRange2D();

        for (int i = 0; i < temps.size(); i++) {
            TempData tempFromTenths = temps.get(i);
            RenderableTempData data = null;
            if (tempFromTenths instanceof RenderableTempData) {
                data = (RenderableTempData) tempFromTenths;
            } else {
                double[] px = descriptor.worldToPixel(new double[] {
                        tempFromTenths.getLatLon().x, tempFromTenths.getLatLon().y });
                if (!envelope.contains(px[0], px[1])) {
                    continue;
                }
                Float amount = temps.get(i).getTempValue().floatValue();
                RGB rgbval = getColorByValue(amount);
                DrawableString string = new DrawableString(formatValues(temps
                        .get(i).getTempValue()), rgbval);
                string.setCoordinates(px[0], px[1], px[2]);
                string.verticalAlignment = VerticalAlignment.MIDDLE;
                string.horizontalAlignment = HorizontalAlignment.CENTER;
                data = new RenderableTempData(tempFromTenths, string);
            }
            newTemps.add(data);
        }
        synchronized (data) {
            data.put(time, newTemps);
            dataTimes.add(time);
        }
        issueRefresh();
    }

    
    private String formatValues(Double tempValue) {
        return String.format("%6.0f", tempValue).substring(1);
    }
    
}