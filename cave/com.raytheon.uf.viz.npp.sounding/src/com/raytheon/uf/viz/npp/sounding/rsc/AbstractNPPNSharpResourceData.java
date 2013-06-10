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
package com.raytheon.uf.viz.npp.sounding.rsc;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData;
import com.raytheon.uf.viz.npp.sounding.Activator;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract NPP Sounding resource data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractNPPNSharpResourceData extends
        D2DNSharpResourceData {

    public static class NPPNsharpStationInfo extends NsharpStationInfo {

        private TimeRange timeRange;

        public NPPNsharpStationInfo(NsharpStationInfo info, DataTime time) {
            this.latitude = info.getLatitude();
            this.longitude = info.getLongitude();
            this.reftime = new Timestamp(info.getReftime().getTime());
            if (info.getRangestarttime() != null) {
                this.rangestarttime = new Timestamp(info.getRangestarttime()
                        .getTime());
            }
            this.sndType = info.getSndType();
            this.stnDisplayInfo = info.getStnDisplayInfo();
            this.timeLineSpList = new ArrayList<NsharpStationInfo.timeLineSpecific>(
                    info.getTimeLineSpList());
            setDataTime(time);
        }

        public void setDataTime(DataTime time) {
            this.reftime = new Timestamp(time.getMatchRef());
            if (time.getUtilityFlags().contains(FLAG.PERIOD_USED)) {
                this.timeRange = time.getValidPeriod();
            }
        }

        public DataTime getDataTime() {
            if (timeRange != null) {
                return new DataTime(getReftime().getTime(), timeRange);
            } else {
                return new DataTime(new Date(getReftime().getTime()));
            }
        }
    }

    protected static final Unit<?> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    protected static final Unit<?> HEIGHT_UNIT = SI.METER;

    protected static final Unit<?> TEMPERATURE_UNIT = SI.CELSIUS;

    protected static final Unit<?> TEMPERATURE_CALC_UNIT = SI.KELVIN;

    protected static final Unit<?> H2O_UNIT = SI.GRAM.divide(SI.KILOGRAM);

    protected static final Unit<?> DEWPOINT_UNIT = SI.CELSIUS;

    private final String plugin;

    private final String[] parameters;

    public AbstractNPPNSharpResourceData() {
        // This constructor only exists so JAXB won't complain
        throw new UnsupportedOperationException();
    }

    protected AbstractNPPNSharpResourceData(String name, String plugin,
            String[] parameters) {
        super(name);
        this.plugin = plugin;
        this.parameters = parameters;
    }

    @Override
    protected void preparePointInfo() throws VizException {
        // everything should already be set
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData#createStationInfo
     * (com.raytheon.uf.common.time.DataTime)
     */
    @Override
    protected NsharpStationInfo createStationInfo(DataTime time) {
        return new NPPNsharpStationInfo(super.createStationInfo(time), time);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData#getSoundingCube
     * (gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo)
     */
    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        DataTime time = new DataTime(stnInfo.getReftime());
        if (stnInfo instanceof NPPNsharpStationInfo) {
            time = ((NPPNsharpStationInfo) stnInfo).getDataTime();
        }
        try {
            PointDataContainer pdc = PointDataRequest
                    .requestPointDataAllLevels(time, plugin, parameters, null,
                            getMetadataMap());
            PointDataView pdv = null;
            Coordinate closest = null;
            for (int i = 0; i < pdc.getCurrentSz(); i++) {
                PointDataView testPdv = pdc.readRandom(i);
                Coordinate p = new Coordinate(
                        testPdv.getFloat(NPPSoundingRecord.LONGITUDE),
                        testPdv.getFloat(NPPSoundingRecord.LATITUDE));
                if (closest == null
                        || coordinate.distance(p) < coordinate
                                .distance(closest)) {
                    pdv = testPdv;
                    closest = p;
                }
            }
            if (pdv == null) {
                return null;
            }

            List<NcSoundingLayer> layers = getSoundingLayers(pdv);
            Collections.sort(layers,
                    NsharpDataHandling.reversePressureHeightWindComparator());

            // We have to interpolate everything so that height,temperature, and
            // dewpoint are available on all levels.
            interpolateHeight(layers);
            interpolateTemperature(layers);
            interpolateDewpoint(layers);
            Iterator<NcSoundingLayer> iter = layers.iterator();
            while (iter.hasNext()) {
                NcSoundingLayer layer = iter.next();
                if (layer.getPressure() < 0) {
                    iter.remove();
                } else if (layer.getGeoHeight() < 0) {
                    // TODO: Needed?
                    // iter.remove();
                } else if (layer.getTemperature() < -300) {
                    iter.remove();

                } else if (layer.getDewpoint() < -300) {
                    iter.remove();
                }
            }
            NcSoundingProfile profile = new NcSoundingProfile();
            profile.setSoundingLyLst(layers);
            // TODO populate other fields in profile
            NcSoundingCube cube = new NcSoundingCube(Arrays.asList(profile));
            cube.setRtnStatus(QueryStatus.OK);
            return cube;
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
        return null;
    }

    /**
     * @param pdv
     * @return
     */
    protected abstract List<NcSoundingLayer> getSoundingLayers(PointDataView pdv)
            throws VizException;

    private static void interpolateHeight(List<NcSoundingLayer> layers) {
        int belowIndex = -1;
        for (int i = 0; i < layers.size(); i++) {
            NcSoundingLayer layerAbove = layers.get(i);
            if (layerAbove.getGeoHeight() < 0) {
                continue;
            }
            if (belowIndex == -1) {
                belowIndex = i;
                continue;
            }
            NcSoundingLayer layerBelow = layers.get(belowIndex);
            double diff = layerAbove.getGeoHeight() - layerBelow.getGeoHeight();
            double pchg1 = Math.log(layerBelow.getPressure()
                    / layerAbove.getPressure());
            for (int j = belowIndex + 1; j < i; j += 1) {
                NcSoundingLayer layer = layers.get(j);
                double pchg2 = Math.log(layerBelow.getPressure()
                        / layer.getPressure());
                layer.setGeoHeight((float) (layerBelow.getGeoHeight() + ((pchg2 / pchg1) * diff)));
            }
            belowIndex = i;
        }
    }

    private static void interpolateTemperature(List<NcSoundingLayer> layers) {
        int belowIndex = -1;
        for (int i = 0; i < layers.size(); i++) {
            NcSoundingLayer layerAbove = layers.get(i);
            if (layerAbove.getTemperature() < -300) {
                continue;
            }
            if (belowIndex == -1) {
                belowIndex = i;
                continue;
            }
            NcSoundingLayer layerBelow = layers.get(belowIndex);
            double diff = layerAbove.getTemperature()
                    - layerBelow.getTemperature();
            double pchg1 = Math.log(layerBelow.getPressure()
                    / layerAbove.getPressure());
            for (int j = belowIndex + 1; j < i; j += 1) {
                NcSoundingLayer layer = layers.get(j);
                double pchg2 = Math.log(layerBelow.getPressure()
                        / layer.getPressure());
                layer.setTemperature((float) (layerBelow.getTemperature() + ((pchg2 / pchg1) * diff)));
            }
            belowIndex = i;
        }
    }

    private static void interpolateDewpoint(List<NcSoundingLayer> layers) {
        int belowIndex = -1;
        for (int i = 0; i < layers.size(); i++) {
            NcSoundingLayer layerAbove = layers.get(i);
            if (layerAbove.getDewpoint() < -300) {
                continue;
            }
            if (belowIndex == -1) {
                belowIndex = i;
                continue;
            }
            NcSoundingLayer layerBelow = layers.get(belowIndex);
            double diff = layerAbove.getDewpoint() - layerBelow.getDewpoint();
            double pchg1 = Math.log(layerBelow.getPressure()
                    / layerAbove.getPressure());
            for (int j = belowIndex + 1; j < i; j += 1) {
                NcSoundingLayer layer = layers.get(j);
                double pchg2 = Math.log(layerBelow.getPressure()
                        / layer.getPressure());
                layer.setDewpoint((float) (layerBelow.getDewpoint() + ((pchg2 / pchg1) * diff)));
            }
            belowIndex = i;
        }
    }

    // convert h2o in g/kg and pressure in hPa to dewpoint in kelvin.
    protected static float convertH2OtoDewpoint(float h2o, float pressure) {
        double eee = pressure * h2o / (622.0 + 0.378 * h2o);
        double b = 26.66082 - Math.log(eee);
        return (float) ((b - Math.sqrt(b * b - 223.1986)) / 0.0182758048);
    }

    // convert h2o in g/kg and pressure in hPa to relative humidity.
    protected static float convertH20ToRelativeHumidity(float h20,
            float temperature, float pressure) {
        double a = 22.05565;
        double b = 0.0091379024;
        double c = 6106.396;
        double epsilonx1k = 622.0;

        double shxDenom = h20 * 0.378;
        shxDenom += epsilonx1k;

        double tDenom = -b * temperature;
        tDenom += a;
        tDenom -= c / temperature;

        double RH = pressure * h20;
        RH /= shxDenom;
        RH /= Math.exp(tDenom);

        return (float) RH;
    }
}
