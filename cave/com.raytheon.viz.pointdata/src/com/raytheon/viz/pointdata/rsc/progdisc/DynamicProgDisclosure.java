package com.raytheon.viz.pointdata.rsc.progdisc;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.rsc.PlotResource2.Station;
import com.vividsolutions.jts.geom.Coordinate;

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
public class DynamicProgDisclosure extends AbstractProgDisclosure {

    private static class DynamicProgDiscInfo {

        public double[] projCoords;

    }

    public DynamicProgDisclosure(IProgDiscListener listener,
            IMapDescriptor descriptor) {
        super(listener);
        setDescriptor(descriptor);
    }

    @Override
    protected List<Station> progDisc(Task task) {

        IMapDescriptor desc = task.descriptor;

        int displayWidth = (int) (desc.getMapWidth() * task.zoomLevel);

        double kmPerPixel = (displayWidth / task.canvasWidth) / 1000.0;
        LinkedList<Station> imageStations = new LinkedList<Station>();
        LinkedList<Station> selectedStations = new LinkedList<Station>();
        LinkedList<Station> newStations = new LinkedList<Station>();

        for (Station station : task.stations) {
            Coordinate location = station.pixelLocation;
            if (!task.extent.contains(new double[] { location.x, location.y })) {
                continue;
            }
            if (station.plotImage != null) {
                imageStations.add(station);
            } else {
                newStations.add(station);
            }
        }
        List<Station> stationList = new ArrayList<Station>(task.stations.size());
        // First consider the stations that were selected last time
        stationList.addAll(selectedStations);
        // Then the stations that already have an image
        stationList.addAll(imageStations);
        // then any other stations
        stationList.addAll(newStations);

        // get meters per pixel
        double mPerPixel = kmPerPixel * 1000;
        double pixelDist = mPerPixel * task.pixelSizeHint / 2;

        pixelDist /= task.density;

        pixelDist *= task.magnification;

        List<Station> toUse = new ArrayList<Station>();
        // Go through twice, first check only plots where we have an image,
        // second plots for which we have no image
        for (Station a : stationList) {
            boolean use = true;
            for (Station b : toUse) {
                // Make sure a does not overlap with b
                double deltaX = getProjCoords(a, desc)[0]
                        - getProjCoords(b, desc)[0];
                double deltaY = getProjCoords(a, desc)[1]
                        - getProjCoords(b, desc)[1];
                // Absolute value logic inlined for performance
                deltaX = (deltaX <= 0.0D) ? 0.0D - deltaX : deltaX;
                deltaY = (deltaY <= 0.0D) ? 0.0D - deltaY : deltaY;

                if (deltaX < pixelDist && deltaY < pixelDist) {
                    use = false;
                    break;
                }
            }
            if (use) {
                toUse.add(a);
            }
        }

        return toUse;
    }

    private double[] getProjCoords(Station station, IDescriptor desc) {

        if (station.progDiscInfo instanceof DynamicProgDiscInfo) {
            return ((DynamicProgDiscInfo) station.progDiscInfo).projCoords;
        } else {
            double[] in = new double[] { station.info[0].longitude,
                    station.info[0].latitude, 0 };
            double[] out = new double[3];
            try {
                MathTransform fromLatLon = MapUtil.getTransformFromLatLon(desc
                        .getCRS());
                fromLatLon.transform(in, 0, out, 0, 1);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            DynamicProgDiscInfo info = new DynamicProgDiscInfo();
            info.projCoords = out;
            station.progDiscInfo = info;
            return out;
        }
    }

}
