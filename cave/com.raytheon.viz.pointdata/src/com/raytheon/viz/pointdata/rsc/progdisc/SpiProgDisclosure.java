package com.raytheon.viz.pointdata.rsc.progdisc;

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.common.pointdata.vadriver.VA_Advanced;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import com.raytheon.viz.pointdata.rsc.PlotResource.Station;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Progressive disclosure algorithm that uses information form an spi file to
 * determine which stations to display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 12, 2011           bsteffen    Initial creation
 * Jun 06, 2014  2061     bsteffen    Remove old PlotResource
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SpiProgDisclosure extends AbstractProgDisclosure {

    private class SpiProgDiscInfo {
        Double distValue;

        Double origDistValue;

        Integer goodnessValue;
    }

    private StaticPlotInfoPV spi;

    private VA_Advanced progDisc;

    private double minDist = Double.MAX_VALUE;

    public SpiProgDisclosure(IProgDiscListener listener,
            IMapDescriptor descriptor, String spiFile) {
        super(listener);
        this.setDescriptor(descriptor);
        this.spi = StaticPlotInfoPV.readStaticPlotInfoPV(spiFile);
        for (SPIEntry entry : spi.getSpiList().values()) {
            if (entry.distance < minDist) {
                minDist = entry.distance;
            }
        }
        this.progDisc = new VA_Advanced();
    }

    @Override
    protected List<Station> progDisc(Task task) {
        int displayWidth = (int) (task.descriptor.getMapWidth() * task.zoomLevel);

        double kmPerPixel = (displayWidth / task.canvasWidth) / 1000.0;
        double displayHintSize = task.pixelSizeHint * task.magnification;
        double threshold = (displayHintSize * kmPerPixel) / task.density;
        LinkedList<Station> stationList = new LinkedList<Station>();
        for (Station station : task.stations) {
            SpiProgDiscInfo info = getInfo(station, task.descriptor);
            Coordinate location = station.pixelLocation;
            if (!task.extent.contains(new double[] { location.x, location.y })) {
                continue;
            }

            if (info.distValue == null) {
                calculateProgDisc(task.stations, task.descriptor);
            }

            if (info.distValue >= threshold) {
                stationList.addLast(station);
            }
        }
        return stationList;
    }

    private void calculateProgDisc(List<Station> stations, IDescriptor desc) {
        int size = stations.size();
        Coordinate[] latLonArray = new Coordinate[size];
        Integer[] goodnessArray = new Integer[size];
        Double[] distArray = new Double[size];
        int i = 0;
        int dynamic = 0;
        for (Station station : stations) {
            latLonArray[i] = new Coordinate(station.info[0].longitude,
                    station.info[0].latitude);
            goodnessArray[i] = getInfo(station, desc).goodnessValue;
            distArray[i] = getInfo(station, desc).origDistValue;
            if (distArray[i] == -1) {
                dynamic++;
            }
            ++i;
        }
        progDisc.setVaJustGoodness(false);
        if (dynamic * dynamic / size < 3000.0) {
            progDisc.setVaDistPass(true);
            distArray = progDisc.getVaAdvanced(latLonArray, goodnessArray,
                    distArray);
        } else {
            progDisc.setVaDistPass(false);
            distArray = progDisc.getVaAdvanced(latLonArray, goodnessArray,
                    distArray);
        }

        for (i = 0; i < size; ++i) {
            getInfo(stations.get(i), desc).distValue = distArray[i];
        }
    }

    private SpiProgDiscInfo getInfo(Station station, IDescriptor desc) {
        if (station.progDiscInfo instanceof SpiProgDiscInfo) {
            return (SpiProgDiscInfo) station.progDiscInfo;
        } else {
            SpiProgDiscInfo info = new SpiProgDiscInfo();
            SPIEntry obsStation = null;
            if (spi != null) {
                obsStation = spi.getSPIEntry(station.info[0].stationId);
            }
            if (obsStation != null) {
                Coordinate thisLocation = obsStation.latlon;
                double[] thisLocationLatLon = { thisLocation.x, thisLocation.y };
                double[] thisLocationPixel = desc
                        .worldToPixel(thisLocationLatLon);
                station.pixelLocation = new Coordinate(thisLocationPixel[0],
                        thisLocationPixel[1]);
                if (obsStation.distance < minDist) {
                    info.origDistValue = minDist;
                } else {
                    info.origDistValue = obsStation.distance;
                }
            } else {
                info.origDistValue = -1.0;
            }
            info.goodnessValue = 0;
            station.progDiscInfo = info;
            return info;
        }
    }

}
