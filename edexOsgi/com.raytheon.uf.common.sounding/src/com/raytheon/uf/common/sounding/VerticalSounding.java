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
package com.raytheon.uf.common.sounding;

import static com.raytheon.uf.common.sounding.SoundingLayer.MISSING;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.SoundingLayer.DATA_TYPE;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Geometry;

/**
 * VerticalSounding is an ordered collection of SoundingLayers sorted by
 * pressure, where the highest pressure is at the beginning of the collection.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * 20071127            382 jkorman     Moved from Cave graphing.
 * 15Jan2008           682 ebabin      Updated to remove non calculated parameters.
 * 16Jan2008           682 ebabin      Updates for grib model traps on multiple loads.
 * 04Oct2008               dhladky     Many changes.
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class VerticalSounding implements Iterable<SoundingLayer>, Cloneable {

    private static final float FREEZING = 273.15f;

    /**
     * Is the value not null and above the missing/invalid values.
     * 
     * @param value
     *            A value to check.
     * @return Is the value valid.
     */
    protected static boolean isValid(double value) {
        return value < MISSING;
    }

    // *************************************************
    private SurfaceObsLocation spatialInfo;

    private Calendar obsTime = null;

    private DataTime dataTime = null;

    private List<SoundingLayer> layerData = new ArrayList<SoundingLayer>();

    private Map<Float, SoundingLayer> layerMap = new HashMap<Float, SoundingLayer>();

    private List<SoundingLayer> maxWinds = new ArrayList<SoundingLayer>();

    private SoundingLayer sfcLayer = null;

    private String displayFormat;

    // *************************************************
    // * Computed data.
    // *************************************************
    private SoundingLayer maxTemperatureLayer = null;

    private SoundingLayer minTemperatureLayer = null;

    private boolean initialLoad = true;

    private String name = "";

    /**
     * Create an empty sounding.
     */
    public VerticalSounding() {
    }

    /**
     * @return the displayFormat
     */
    public String getDisplayFormat() {
        return displayFormat;
    }

    /**
     * @param displayFormat
     *            the displayFormat to set
     */
    public void setDisplayFormat(String displayFormat) {
        this.displayFormat = displayFormat;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        VerticalSounding clone = (VerticalSounding) super.clone();
        clone.layerData = new ArrayList<SoundingLayer>();
        clone.layerMap = new HashMap<Float, SoundingLayer>();
        clone.maxWinds = new ArrayList<SoundingLayer>();
        clone.spatialInfo = (SurfaceObsLocation) spatialInfo.clone();
        clone.setInitialLoad(true);

        for (SoundingLayer layer : layerData) {
            clone.addLayer((SoundingLayer) layer.clone());
        }
        clone.invalidate();

        return clone;
    }

    /**
     * Check to see if a surface layer exists. If it does not then find the
     * lowest layer and set this as the surface.
     */
    public void checkSfcLayer() {

        // Did the data declare a surface layer?
        if (sfcLayer == null) {
            // Did we set Elevation? If so use that to calculate the surface
            // layer.
            if (this.getElevation() != null) {
                int press = (int) WxMath.heightToPressure(this.getElevation());
                sfcLayer = this.getLayerNearest(press);
                if (press != sfcLayer.getPressure()) {
                    SoundingLayer sLayer = new SoundingLayer();
                    sLayer.setGeoHeight(this.getElevation());
                    sLayer.setPressure(press);
                    sLayer.setTemperature((float) (Math.pow(
                            press / sfcLayer.getPressure(), 0.286) * sfcLayer
                            .getTemperature()));
                    sLayer.setWindU(sfcLayer.getWindU());
                    sLayer.setWindV(sfcLayer.getWindV());
                    sLayer.setWindDirection(sfcLayer.getWindDirection());
                    sLayer.setWindSpeed(sfcLayer.getWindSpeed());
                    sfcLayer = sLayer;
                    addLayer(sfcLayer);
                }
            } else {
                SoundingLayer lowestLayer = new SoundingLayer();
                // Start at 1 hPa. We won't consider anything higher that that.
                lowestLayer.setPressure(1.0f);
                for (SoundingLayer layer : layerData) {
                    if (layer.isLowerThan(lowestLayer)) {
                        lowestLayer = layer;
                    }
                }
                sfcLayer = lowestLayer;
            }
        }
    }

    /**
     * Get the surface layer if defined.
     */
    public SoundingLayer getSfcLayer() {
        return sfcLayer;
    }

    /**
     * @return the layerData
     */
    public List<SoundingLayer> getLayerData() {
        return layerData;
    }

    /**
     */
    public void removeBelowSfcLayers() {
        if (layerData != null) {
            if (sfcLayer != null) {
                Iterator<SoundingLayer> layers = layerData.iterator();
                while (layers.hasNext()) {
                    SoundingLayer layer = layers.next();
                    if (layer.isLowerThan(sfcLayer)) {
                        layers.remove();
                    }
                }
            }
        }
    }

    private void rebuildMap() {
        layerMap.clear();
        for (SoundingLayer layer : layerData) {
            layerMap.put(new Float(layer.getPressure()), layer);
        }
    }

    /**
     * Add a layer to this sounding.
     * 
     * @param layer
     *            A sounding layer to add.
     */
    /**
     * Add a layer to this sounding.
     * 
     * @param layer
     *            A sounding layer to add.
     */
    public void addLayer(SoundingLayer layer) {
        if (okToAdd(layer)) {
            sortLayerIntoSounding(layer);
            if (layer.getPressure() != MISSING) {
                // update values
                SoundingLayer currLayer = layerMap.get(layer.getPressure());
                if (currLayer != null) {
                    if (layer.getTemperature() != MISSING) {
                        currLayer.setTemperature(layer.getTemperature());
                    }
                    if (layer.getDewpoint() != MISSING) {
                        currLayer.setDewpoint(layer.getDewpoint());
                    }
                    if (layer.getWindDirection() != MISSING) {
                        currLayer.setWindDirection(layer.getWindDirection());
                    }
                    if (layer.getWindSpeed() != MISSING) {
                        currLayer.setWindSpeed(layer.getWindSpeed());
                    }
                } else {
                    layerMap.put(new Float(layer.getPressure()), layer);
                }
            }

            invalidate();
        }
    }

    /**
     * 
     * @param layers
     */
    public void addLayers(List<SoundingLayer> layers) {
        initialLoad = true;
        layerData.clear();
        layerMap.clear();
        for (SoundingLayer layer : layers) {
            addLayer(layer);
        }
    }

    /**
     * 
     */
    public void invalidate() {
        if (!isInitialLoad()) {
            sortByPressure();
            rebuildMap();
            updateMaxMinTemp();
        }
    }

    /**
     * Checks a layer to be inserted to determine if a layer with the same
     * pressure already exists.
     * 
     * @param layer
     *            The layer to be added.
     */
    private boolean okToAdd(SoundingLayer layer) {
        boolean shouldAdd = false;
        if (layer != null) {
            float p = layer.getPressure();
            if ((p != MISSING) && (layer.getLayerType() != null)) {
                SoundingLayer currLayer = layerMap.get(p);
                if (currLayer != null) {
                    switch (layer.getLayerType()) {
                    case SURFACE: {
                        if (LayerType.MAN_PRESSURE.equals(currLayer
                                .getLayerType())) {
                            int i = layerData.indexOf(currLayer);
                            layerData.remove(i);
                            layerMap.remove(currLayer.getPressure());
                            shouldAdd = true;
                        } else if (LayerType.SIG_PRESSURE.equals(currLayer
                                .getLayerType())) {
                            int i = layerData.indexOf(currLayer);
                            layerData.remove(i);
                            layerMap.remove(currLayer.getPressure());
                            shouldAdd = true;
                        }
                        break;
                    }
                    case MAN_PRESSURE: {
                        if (LayerType.SIG_PRESSURE.equals(currLayer
                                .getLayerType())) {
                            int i = layerData.indexOf(currLayer);
                            layerData.remove(i);
                            layerMap.remove(currLayer.getPressure());
                            shouldAdd = true;
                        }
                        break;
                    }
                    case SIG_PRESSURE: {
                        // TODO : Make sure to check other layer types.
                        break;
                    }
                    default: {
                        shouldAdd = true;
                    }
                    } // end switch

                } else {
                    shouldAdd = true;
                }
            } else {
                layerMap.put(new Float(layer.getPressure()), layer);
            }
        }
        return shouldAdd;
    }

    /**
     * Remove Data layer
     * 
     * @param layer
     */
    public void removeLayer(SoundingLayer layer) {

        if (layerMap.containsKey(layer.getPressure())) {
            layerMap.remove(layer.getPressure());
        }
        layerData.remove(layer);
        invalidate();
    }

    /**
     * Gets the layer at the specified pressure level
     * 
     * @param level
     * @return the layer
     */
    public SoundingLayer getLayer(float level) {
        return layerMap.get(level);
    }

    /**
     * Get the layer at the specified index
     * 
     * @param index
     * @return the layer
     */
    public SoundingLayer get(int index) {
        return layerData.get(index);
    }

    /**
     * Sort a layer into this sounding. The primary sort key is the layer
     * pressure. If pressure is not available, the geopotential height is used.
     * 
     * @param layer
     *            The layer that is being added.
     */
    private void sortLayerIntoSounding(SoundingLayer layer) {
        // If empty, just add the layer.
        if (LayerType.MAX_WIND.equals(layer.getLayerType())) {
            maxWinds.add(layer);
            return;
        }
        if (LayerType.SURFACE.equals(layer.getLayerType())) {
            sfcLayer = layer;
        }
        if (layerData.size() == 0) {
            layerData.add(layer);
        } else {
            boolean entered = false;
            for (int i = 0; i < layerData.size(); i++) {
                SoundingLayer cLayer = layerData.get(i);
                if (layer.isLowerThan(cLayer)) {
                    layerData.add(i, layer);
                    entered = true;
                    break;
                }
            }
            if (!entered) {
                layerData.add(layer);
            }
        }
    }

    /**
     * Gets the initialLoad boolean
     * 
     * @return
     */
    public boolean isInitialLoad() {
        return initialLoad;
    }

    /**
     * Sets the initialLoad param
     * 
     * @param initalLoad
     */
    public void setInitialLoad(boolean initialLoad) {
        this.initialLoad = initialLoad;
    }

    /**
     * Return the layer closest to a given pressure.
     * 
     * @param pressure
     *            The target pressure.
     * @return The SoundingLayer instance nearest to the target.
     */
    public SoundingLayer getLayerNearest(float pressure) {
        SoundingLayer retLayer = new SoundingLayer(pressure, MISSING, MISSING,
                MISSING, MISSING, MISSING, MISSING);

        if (layerData.size() > 0) {
            retLayer = layerData.get(0);
            if (layerData.size() == 1) {
                return retLayer;
            }

            SoundingLayer loLayer = null;
            SoundingLayer hiLayer = null;
            SoundingLayer sLayer = new SoundingLayer(pressure, MISSING,
                    MISSING, MISSING, MISSING, MISSING, MISSING);
            int hiIndex = 0;
            for (int i = 0; i < layerData.size(); i++) {
                SoundingLayer layer = layerData.get(i);
                if (layer.getPressure() == pressure) {
                    return layer;
                } else if (layer.isHigherThan(sLayer)) {
                    // Make sure we have a lower pressure level.
                    if (layer.getPressure() < MISSING) {
                        hiLayer = layer;
                        hiIndex = i;
                        break;
                    }
                }
            }
            if (hiIndex > 0) {
                SoundingLayer layer = layerData.get(0);
                loLayer = layer;

                for (int i = hiIndex - 1; i > 0; i--) {
                    layer = layerData.get(i);
                    if (layer.isLowerThan(sLayer)) {
                        // Make sure we have a lower pressure level.
                        if (layer.getPressure() < MISSING) {
                            loLayer = layer;
                            break;
                        }
                    }
                }
            } else {
                if (pressure > layerData.get(0).getPressure()) {
                    retLayer = layerData.get(0);
                } else {
                    retLayer = layerData.get(layerData.size() - 1);
                }
            }

            if ((loLayer != null) && (hiLayer != null)) {
                double hPoint = (loLayer.getPressure() + hiLayer.getPressure()) / 2;

                if (pressure < hPoint) {
                    retLayer = hiLayer;
                } else {
                    retLayer = loLayer;
                }
            }
        }
        return retLayer;
    }

    /**
     * Return the layer closest to a given windspeed and winddir.
     * 
     * @param windspeed
     *            , winddir The target pressure.
     * @return The SoundingLayer instance nearest to the target.
     */
    public SoundingLayer getLayerNearest(float windspeed, float winddir) {
        SoundingLayer retLayer = null;
        SoundingLayer loLayer = null;
        SoundingLayer hiLayer = null;
        SoundingLayer sLayer = new SoundingLayer(MISSING, MISSING, MISSING,
                MISSING, windspeed, winddir, MISSING);
        int hiIndex = 0;

        // Check and find closest wind speed
        for (int i = 0; i < layerData.size(); i++) {
            SoundingLayer layer = layerData.get(i);
            if (layer.getWindSpeed() == sLayer.getWindSpeed()

            && layer.getWindDirection() == sLayer.getWindDirection()) {
                return layer;
            } else if ((layer.getWindSpeed() > sLayer.getWindSpeed())
                    && layer.getWindSpeed() != 99999.0) {
                hiLayer = layer;
                hiIndex = i;
                break;
            }
        }
        if (hiIndex > 0) {
            for (int i = hiIndex + 1; i < layerData.size(); i++) {
                SoundingLayer layer = layerData.get(i);
                if (layer.getWindSpeed() < sLayer.getWindSpeed()) {
                    loLayer = layer;
                    break;
                }
            }
        } else {
            if (windspeed > layerData.get(0).getPressure()) {
                retLayer = layerData.get(0);
            } else {
                retLayer = layerData.get(layerData.size() - 1);
            }
        }

        if ((loLayer != null) && (hiLayer != null)) {
            double hPoint = (loLayer.getWindSpeed() + hiLayer.getWindSpeed()) / 2;

            if (windspeed < hPoint) {
                retLayer = hiLayer;
            } else {
                retLayer = loLayer;
            }
        }
        return retLayer;
    }

    /**
     * Get an iterator to the internal layer data.
     * 
     * @return The layer data iterator.
     */
    public Iterator<SoundingLayer> iterator() {
        return layerData.iterator();
    }

    /**
     * Get this observation's geometry.
     * 
     * @return The geometry for this observation.
     */
    public Geometry getObsGeometry() {
        Geometry obsGeometry = null;
        if (spatialInfo != null) {
            obsGeometry = spatialInfo.getGeometry();
        }
        return obsGeometry;
    }

    /**
     * Get the geometry latitude.
     * 
     * @return The geometry latitude.
     */
    public double getLatitude() {
        return spatialInfo.getLatitude();
    }

    /**
     * Get the geometry longitude.
     * 
     * @return The geometry longitude.
     */
    public double getLongitude() {
        return spatialInfo.getLongitude();
    }

    /**
     * Set the geometry latitude.
     * 
     */
    public void setLatitude(float latitude) {
        spatialInfo.setLatitude(latitude);
    }

    /**
     * Set the geometry longitude.
     * 
     */
    public void setLongitude(float longitude) {
        spatialInfo.setLongitude(longitude);
    }

    /**
     * Get the station identifier for this observation.
     * 
     * @return the stationId
     */
    public String getStationId() {
        String stationId = null;
        if (spatialInfo != null) {
            stationId = (spatialInfo).getStationId();

        }
        return stationId;
    }

    /**
     * Set the station identifier for this observation.
     * 
     * @param stationId
     *            the stationId to set
     */
    public void setStationId(String stationId) {
        if (spatialInfo == null) {
            spatialInfo = new SurfaceObsLocation();
        }
        (spatialInfo).setStationId(stationId);
    }

    /**
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Integer getElevation() {
        Integer elevation = null;
        if (spatialInfo != null) {
            elevation = (spatialInfo).getElevation();
        }
        return elevation;
    }

    /**
     * Set the elevation, in meters, of the observing platform or location.
     * 
     * @param elevation
     *            The elevation to set
     */
    public void setElevation(Integer elevation) {
        if (spatialInfo == null) {
            spatialInfo = new SurfaceObsLocation();
        }
        (spatialInfo).setElevation(elevation);
    }

    /**
     * @return the timeObs
     */
    public Calendar getObsTime() {
        return obsTime;
    }

    /**
     * @param timeObs
     *            the timeObs to set
     */
    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    /**
     * @return the dataTime
     */
    public DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    /**
     * Get the three char ICAO climo station name associated with this sounding.
     * 
     * @return The ICAO climo station name.
     */
    public String getName() {
        return name;
    }

    /**
     * Set the three char ICAO climo station name associated with this sounding.
     * 
     * @param name
     *            The ICAO climo station name.
     */
    public void setName(String name) {
        this.name = name;
    }

    // *************************************************

    /**
     * Gets the maximum pressure layer, always first in array unless added to.
     * 
     * @return
     */
    public SoundingLayer getMaxPressurelayer() {
        int i = 0;
        while (layerData.get(i).getPressure() >= MISSING) {
            i++;
        }

        return layerData.get(i);
    }

    /**
     * Returns the top of the sounding layer
     * 
     * @return SoundingLayer
     */
    public SoundingLayer getMinPressureLayer() {
        int i = layerData.size();
        while (layerData.get(--i).getPressure() >= MISSING) {
        }

        return layerData.get(i);
    }

    /**
     * Get the layer containing the maximum temperature in the sounding.
     * 
     * @return The layer containing the maximum temperature in the sounding.
     */
    public SoundingLayer getMaxTempLayer() {
        return maxTemperatureLayer;
    }

    /**
     * Get the layer containing the minimum temperature in the sounding.
     * 
     * @return The layer containing the minimum temperature in the sounding.
     */
    public SoundingLayer getMinTempLayer() {
        return minTemperatureLayer;
    }

    /**
     * Determine the pressure level of the first occurrence of a specified
     * temperature.
     * 
     * @param temperature
     *            The temperature to find.
     * @return The pressure of the first occurrence. Returns a null value if the
     *         temperature does not exist.
     */
    // public Double tempLevel(double temperature) {
    // Double temperatureLevel = null;
    // updateMaxMinTemp();
    // // If either of the following layers are null don't bother checking
    // // further.
    // if ((maxTemperatureLayer != null) && (minTemperatureLayer != null)) {
    // // Check if the temperature is in this sounding's range.
    // if (isValid(maxTemperatureLayer.getTemperature())
    // && isValid(minTemperatureLayer.getTemperature())) {
    // if (temperature < maxTemperatureLayer.getTemperature()
    // && temperature > minTemperatureLayer.getTemperature()) {
    // SoundingLayer layerBelow = null;
    // SoundingLayer layerAbove = null;
    //
    // // find the first level that contains a valid temperature.
    // for (SoundingLayer layer : layerData) {
    // if (isValid(layer.getTemperature())) {
    // // we're looking for a base layer
    // if (layerBelow == null) {
    // layerBelow = layer;
    // } else {
    // // we have a base layer, now find the next layer
    // // with a temperature.
    // layerAbove = layer;
    //
    // // now we have two layers with temperature.
    // // Find out if the temperature is here.
    // Double p = isContained(layerBelow, layerAbove,
    // temperature);
    // // we found it.
    // if (isValid(p)) {
    // temperatureLevel = p;
    // break;
    // }
    // }
    // }
    // }
    // }
    // }
    // }
    // return temperatureLevel;
    // }
    /**
     * 
     * @param type
     * @return
     */
    public float[] getValues(DATA_TYPE type) {

        float f[] = new float[layerData.size()];
        for (int i = 0; i < layerData.size(); i++) {
            f[i] = layerData.get(i).getValue(type);
        }
        return f;
    }

    public String printableSoundingData() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("START******" + this.getDataTime().getLegendString()
                + "*******/n");
        for (SoundingLayer l : layerData) {
            buffer.append(l + "\n");
        }
        buffer.append("END  ******" + this.getDataTime().getLegendString()
                + "*******/n");
        return buffer.toString();
    }

    public SurfaceObsLocation getSpatialInfo() {
        return spatialInfo;
    }

    public void setSpatialInfo(SurfaceObsLocation spatialInfo) {
        this.spatialInfo = spatialInfo;
    }

    /**
     * Iterate the sounding to recompute the max and min data. This method must
     * be used when edits have been performed.
     */
    private void updateMaxMinTemp() {
        SoundingLayer maxTempLayer = null;
        SoundingLayer minTempLayer = null;

        double minTemp = Double.MAX_VALUE;
        double maxTemp = -Double.MAX_VALUE;

        for (SoundingLayer layer : layerData) {
            double t = layer.getTemperature();
            if (isValid(t)) {
                if (t > maxTemp) {
                    maxTemp = t;
                    maxTempLayer = layer;
                }
                if (t < minTemp) {
                    minTemp = t;
                    minTempLayer = layer;
                }
            }
        }
        maxTemperatureLayer = maxTempLayer;
        minTemperatureLayer = minTempLayer;
    }

    public int size() {
        return layerData.size();
    }

    /**
     * Interpolate the desired value at specified pressure
     * 
     * @param pressure
     * @param dataType
     * @return the interpolated value
     */
    public float interpolateValue(float pressure, DATA_TYPE dataType) {

        // find the bracketing layers
        SoundingLayer layer, prevLayer;
        layer = prevLayer = null;
        float press = (float) Math.floor(pressure);
        for (SoundingLayer l : layerData) {
            if (l.getPressure() < MISSING && l.getValue(dataType) < MISSING) {
                if (Math.floor(l.getPressure()) <= press) {
                    layer = l;
                    break;
                }
                prevLayer = l;
            }
        }

        // if bracketing layers not found
        if (layer == null || prevLayer == null) {
            return MISSING;
        }

        // compute weighting factors
        double w1 = Math.log(prevLayer.getPressure() / layer.getPressure());
        double w2 = Math.log(prevLayer.getPressure() / pressure) / w1;
        w1 = Math.log(pressure / layer.getPressure()) / w1;

        // interpolate desired value
        double interpVal = w1 * prevLayer.getValue(dataType) + w2
                * layer.getValue(dataType);

        return (float) interpVal;
    }

    public void sortByPressure() {
        Collections.sort(layerData, SoundingLayer.getPressureComparator());
    }

    public float totalTotals() {
        float totals = -9999.0f;

        // TT = (T850 - T500) + (Td850 - T500)
        SoundingLayer lyr850 = null;
        SoundingLayer lyr500 = null;

        Float val = new Float(850.0f);
        if (layerMap.containsKey(val)) {
            lyr850 = layerMap.get(val);
        }
        val = new Float(500.0f);
        if (layerMap.containsKey(val)) {
            lyr500 = layerMap.get(val);
        }
        if ((lyr850 != null) && (lyr500 != null)) {
            // 500 temperature is required for cross and vertical totals.
            float t500 = lyr500.getTemperature();
            float t850 = lyr850.getTemperature();
            float td850 = lyr850.getTemperature();
            if (t500 > -9999.0f) {
                if (t850 > -9999.0f) {
                    if (td850 > -9999.0f) {
                        totals = (t850 - t500) + (td850 - t500);
                    }
                }
            }
        }
        return totals;
    }

    public float getWindSpeed700() {
        float ws = -9999.0f;

        // TT = (T850 - T500) + (Td850 - T500)
        SoundingLayer lyr700 = null;

        Float val = new Float(700.0f);
        if (layerMap.containsKey(val)) {
            lyr700 = layerMap.get(val);
        }
        if (lyr700 != null) {
            ws = lyr700.getWindSpeed();
        }
        return ws;
    }

    public float getWindUComp500() {
        float uComp = -9999.0f;

        SoundingLayer lyr500 = null;

        Float val = new Float(500.0f);
        if (layerMap.containsKey(val)) {
            lyr500 = layerMap.get(val);
        }
        if (lyr500 != null) {
            float ws = lyr500.getWindSpeed();
            float wd = lyr500.getWindDirection();
            if ((ws >= 0) && (wd > 0) && (wd < MISSING)) {
                uComp = -ws * (float) Math.sin(Math.toRadians(wd));

            }
        }
        return uComp;
    }

    /**
     * Find the height in meters of the freezing level. 1. Surface is the
     * freezing level.
     * 
     * 
     * 
     * 
     * @return
     */
    public float firstFreezingLevel() {

        float fzlLevel = 0;

        // Create a list of layers that have both temperature and
        // height data.
        List<SoundingLayer> data = new ArrayList<SoundingLayer>();
        for (SoundingLayer layer : layerData) {
            if ((layer.getGeoHeight() != MISSING)
                    && (layer.getTemperature() != MISSING)) {
                data.add(layer);
            }
        }
        if (data.size() > 0) {
            // Scenerio 1 - Freezing level at the surface.
            if (data.get(0).getTemperature() == FREEZING) {
                fzlLevel = getElevation().floatValue();
            } else {
                if (data.size() > 2) {
                    for (int i = 1; i < data.size(); i++) {
                        float ta = data.get(i).getTemperature();
                        float tb = data.get(i - 1).getTemperature();
                        float ha = data.get(i).getGeoHeight();
                        float hb = data.get(i - 1).getGeoHeight();

                        if ((ta < FREEZING) && (tb > FREEZING)) {
                            fzlLevel = interp(hb, tb, ha, ta);
                        } else if ((ta > FREEZING) && (tb < FREEZING)) {
                            fzlLevel = interp(hb, tb, ha, ta);
                        }
                    }
                }
            }
        }
        return fzlLevel;
    }

    /**
     * Interpolate the temperature between tlo and thi at heights hlo and hhi
     * respectively.
     * 
     * @param hlo
     * @param tlo
     * @param hhi
     * @param thi
     * @return
     */
    private static final float interp(float hlo, float tlo, float hhi, float thi) {
        return hlo + ((FREEZING - tlo) * (hhi - hlo) / (thi - tlo));
    }

    public static final void main(String[] args) {

        VerticalSounding sounding = new VerticalSounding();
        sounding.setElevation(350);
        sounding.setInitialLoad(true);

        sounding.addLayer(new SoundingLayer(1000.0f, 76f, -9999.0f, -9999.0f,
                -9999.0f, -9999.0f, -9999.0f));
        sounding.addLayer(new SoundingLayer(969.0f, 350f, 294.3f, 290.73f,
                6.18f, 80f, -9999.0f));
        sounding.addLayer(new SoundingLayer(925.0f, 757f, 294.1f, 292.7f,
                14.93f, 105f, -9999.0f));
        sounding.addLayer(new SoundingLayer(850.0f, 1490f, 293.7f, 293.6f,
                11.33f, 195f, -9999.0f));
        sounding.addLayer(new SoundingLayer(700.0f, 3155f, 285.1f, 277.1f,
                16.99f, 245f, -9999.0f));

        sounding.addLayer(new SoundingLayer(639.0f, 3910f, 278.3f, 276.3f,
                16.99f, 245f, -9999.0f));
        sounding.addLayer(new SoundingLayer(611.5f, 4267f, 275.3f, 273.7f,
                16.99f, 245f, -9999.0f));
        sounding.addLayer(new SoundingLayer(591.0f, 4543f, 273.0f, 271.6f,
                16.99f, 245f, -9999.0f));
        sounding.addLayer(new SoundingLayer(566.8f, 4877f, 270.4f, 265.9f,
                16.99f, 245f, -9999.0f));

        sounding.addLayer(new SoundingLayer(500.0f, 5860f, 265.6f, 254.6f,
                11.8f, 295f, -9999.0f));
        sounding.addLayer(new SoundingLayer(493.0f, 5970f, 265.4f, 252.4f, 38f,
                260f, -9999.0f));

        sounding.setInitialLoad(false);
        sounding.invalidate();

        for (SoundingLayer layer : sounding) {
            System.out.println(layer);
        }
        System.out.println("Total totals     = " + sounding.totalTotals());
        System.out.println("700mb Wind Speed = " + sounding.getWindSpeed700());
        System.out.println("500mb u comp     = " + sounding.getWindUComp500());
        System.out.println("Freezing level 1 = "
                + sounding.firstFreezingLevel());

        sounding = new VerticalSounding();
        sounding.setElevation(350);
        sounding.setInitialLoad(true);

        sounding.addLayer(new SoundingLayer(969.0f, 350f, 273.15f, MISSING,
                MISSING, MISSING, MISSING));
        sounding.addLayer(new SoundingLayer(925.0f, 757f, 275f, MISSING,
                MISSING, MISSING, MISSING));
        sounding.addLayer(new SoundingLayer(925.0f, 757f, 278f, MISSING,
                MISSING, MISSING, MISSING));

        sounding.setInitialLoad(false);
        sounding.invalidate();
        System.out.println("Freezing level 2 = "
                + sounding.firstFreezingLevel());

        sounding = new VerticalSounding();
        sounding.setElevation(350);
        sounding.setInitialLoad(true);

        sounding.addLayer(new SoundingLayer(969.0f, 350f, 263.15f, MISSING,
                MISSING, MISSING, MISSING));
        sounding.addLayer(new SoundingLayer(925.0f, 757f, 271f, MISSING,
                MISSING, MISSING, MISSING));
        sounding.addLayer(new SoundingLayer(925.0f, 1500f, 275f, MISSING,
                MISSING, MISSING, MISSING));

        sounding.setInitialLoad(false);
        sounding.invalidate();
        System.out.println("Freezing level 3 = "
                + sounding.firstFreezingLevel());

    }
}