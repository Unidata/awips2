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
import java.util.List;
import java.util.Map;
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
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.PlotResource.Station;
import com.raytheon.viz.pointdata.rsc.progdisc.GenericProgressiveDisclosure;
import com.raytheon.viz.pointdata.rsc.progdisc.AbstractProgDisclosure.IProgDiscListener;
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
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class MetarTempResource extends
    AbstractVizResource<PlotResourceData, MapDescriptor> implements
    IResourceDataChanged, IPlotModelGeneratorCaller, IProgDiscListener {

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

            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }
            processRemoves();
            return Status.OK_STATUS;
        }

    };

    private LinkedBlockingQueue<PluginDataObject> updates = new LinkedBlockingQueue<PluginDataObject>();

    private LinkedBlockingQueue<DataTime> removes = new LinkedBlockingQueue<DataTime>();

    private Map<DataTime, GenericProgressiveDisclosure<RenderableTempData>> data = new HashMap<>();
    
    private IFont font = null;

    protected MetarTempResource(MetarTempResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);

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
        dataTimes = new ArrayList<DataTime>();
        dataProcessJob.schedule();
        ColorMapParameters params = new ColorMapParameters();

        try {
            params.setColorMap(ColorMapLoader.loadColorMap("colortemp"));
        } catch (ColorMapException e) {
            throw new VizException(e);
        }

        DataMappingPreferences preferences = new DataMappingPreferences();

        DataMappingEntry entry = new DataMappingEntry();
        entry.setDisplayValue(20.);
        entry.setPixelValue(0.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(30.);
        entry.setPixelValue(1.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(40.);
        entry.setPixelValue(2.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(50.5);
        entry.setPixelValue(3.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(60.);
        entry.setPixelValue(4.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(70.);
        entry.setPixelValue(5.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(80.);
        entry.setPixelValue(6.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(90.);
        entry.setPixelValue(7.0);
        preferences.addEntry(entry);

        entry = new DataMappingEntry();
        entry.setDisplayValue(100.);
        entry.setPixelValue(8.0);
        preferences.addEntry(entry);

        params.setDisplayUnit(Unit.ONE);
        params.setDataMapping(preferences);
        params.setColorMapMin(0);
        params.setColorMapMax(8);

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
    
    private void processRemoves() {
        synchronized (data) {
            while (!removes.isEmpty()) {
                DataTime toRemove = removes.poll();
                this.dataTimes.remove(toRemove);
                this.data.remove(toRemove);
            }
        }
    }

    private void processNewFrames(IProgressMonitor monitor) {
        MetarTempDataContainer container = new MetarTempDataContainer(
                resourceData.getMetadataMap(),
                descriptor.getGridGeometry().getEnvelope());
        Set<DataTime> reprojectedFrames = new HashSet<DataTime>();
        Set<DataTime> baseOnly = new HashSet<DataTime>();
        boolean modified = true;
        while (modified) {
            if (monitor.isCanceled()) {
                return;
            }
            modified = false;
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
                        reprojectedFrames.remove(next);
                        modified = true;
                        break;
                    }
                }
            }
        }
    }

    private void addData(DataTime time, List<TempData> temps) {
        if (temps.isEmpty()) {
            if (!dataTimes.contains(time)) {
                GenericProgressiveDisclosure<RenderableTempData> disclosure = 
                		new GenericProgressiveDisclosure<RenderableTempData>();
                synchronized (data) {
                    data.put(time, disclosure);
                }
                dataTimes.add(time);
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

        GridEnvelope2D envelope = GridGeometry2D.wrap(
                descriptor.getGridGeometry()).getGridRange2D();

        for (int i = 0; i < temps.size(); i++) {
            TempData temperature = temps.get(i);
            RenderableTempData data = null;
            if (temperature instanceof RenderableTempData) {
                data = (RenderableTempData) temperature;
            } else {
                double[] px = descriptor.worldToPixel(new double[] {
                        temperature.getLatLon().x, temperature.getLatLon().y });
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
                data = new RenderableTempData(temperature, string);
            }
            newTemps.add(data);
        }
        synchronized (data) {
            data.put(time, newTemps);
            if (!dataTimes.contains(time)) {
                dataTimes.add(time);
            }
        }
        issueRefresh();
    }

    private String formatValues(Double tempValue) {
        return String.format("%6.0f", tempValue).substring(1);
    }

	@Override
	public void disclosureComplete(DataTime time, List<Station> disclosed) {
		// TODO Auto-generated method stub
	}

	@Override
	public void modelGenerated(PlotInfo[] key, IImage image) {
		// TODO Auto-generated method stub
	}

	@Override
	public void clearImages() {
		// TODO Auto-generated method stub
	}

	@Override
	public void messageGenerated(PlotInfo[] key, String message) {
		// TODO Auto-generated method stub
	}

	@Override
	public void resourceChanged(ChangeType type, Object object) {
		// TODO Auto-generated method stub
	}
    
}
