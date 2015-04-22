package gov.noaa.nws.ncep.common.dataplugin.gempak.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class Station {
    @DynamicSerializeElement
    private String stationId;

    @DynamicSerializeElement
    private int wmoIndex;

    @DynamicSerializeElement
    private int elevation;

    @DynamicSerializeElement
    private String country;

    @DynamicSerializeElement
    private String state;

    @DynamicSerializeElement
    private double latitude;

    @DynamicSerializeElement
    private double longitude;

    public String getStationId() {
        return stationId;
    }

    public void setStationId(String stationId) {
        this.stationId = stationId;
    }

    public int getWmoIndex() {
        return wmoIndex;
    }

    public void setWmoIndex(int wmoIndex) {
        this.wmoIndex = wmoIndex;
    }

    public int getElevation() {
        return elevation;
    }

    public void setElevation(int elevation) {
        this.elevation = elevation;
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

}
