package com.raytheon.uf.edex.plugin.goesr.dmw.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.edex.netcdf.description.field.date.EpochOffsetDateValue;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;

/**
 * A single Derived Motion Winds (DMW) Product Description. Used by the
 * DMWDecoder to decode fields from a single NetCDF file into multiple
 * DMWRecords.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/13/2016   19051   mcomerford   Initial creation
 * </pre>
 *
 * @author matt.comerford
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ProductDescription {

    @XmlAttribute(required = true)
    private String name;

    @XmlElement
    private VariableDescription lat;

    @XmlElement
    private VariableDescription lon;

    @XmlElement
    private VariableDescription wspd;

    @XmlElement
    private VariableDescription wdir;

    @XmlElement
    private VariableDescription dqf;

    @XmlElement
    private VariableDescription filter = null;

    @XmlElement
    private VariableDescription channel;

    @XmlElement
    private EpochOffsetDateValue dataTime;

    @XmlElement
    private DelegateFieldDescription orbitalSlot = null;

    @XmlElement
    private DelegateFieldDescription percentGoodDQF = null;

    @XmlElement
    private DelegateFieldDescription scene;

    @XmlElement
    private int validDQF;

    @XmlElement(name = "debug")
    private boolean debugStatus = false;

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the lat
     */
    public VariableDescription getLat() {
        return lat;
    }

    /**
     * @param lat the lat to set
     */
    public void setLat(VariableDescription lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public VariableDescription getLon() {
        return lon;
    }

    /**
     * @param lon the lon to set
     */
    public void setLon(VariableDescription lon) {
        this.lon = lon;
    }

    /**
     * @return the wspd
     */
    public VariableDescription getWspd() {
        return wspd;
    }

    /**
     * @param wspd the wspd to set
     */
    public void setWspd(VariableDescription wspd) {
        this.wspd = wspd;
    }

    /**
     * @return the wdir
     */
    public VariableDescription getWdir() {
        return wdir;
    }

    /**
     * @param wdir the wdir to set
     */
    public void setWdir(VariableDescription wdir) {
        this.wdir = wdir;
    }

    /**
     * @return the dqf
     */
    public VariableDescription getDQF() {
        return dqf;
    }

    /**
     * @param dqf the dqf to set
     */
    public void setDQF(VariableDescription dqf) {
        this.dqf = dqf;
    }

    /**
     * @return the filter
     */
    public VariableDescription getFilter() {
        return filter;
    }

    /**
     * @param filter the filter to set
     */
    public void setFilter(VariableDescription filter) {
        this.filter = filter;
    }

    /**
     * @return the channel
     */
    public VariableDescription getChannel() {
        return channel;
    }

    /**
     * @param channel the channel to set
     */
    public void setChannel(VariableDescription channel) {
        this.channel = channel;
    }

    /**
     * @return the dataTime
     */
    public EpochOffsetDateValue getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime the dataTime to set
     */
    public void setDataTime(EpochOffsetDateValue dataTime) {
        this.dataTime = dataTime;
    }

    /**
     * @return the orbitalSlot
     */
    public DelegateFieldDescription getOrbitalSlot() {
        return orbitalSlot;
    }

    /**
     * @param orbitalSlot the orbitalSlot to set
     */
    public void setOrbitalSlot(DelegateFieldDescription orbitalSlot) {
        this.orbitalSlot = orbitalSlot;
    }

    /**
     * @return the percentGoodDQF
     */
    public DelegateFieldDescription getPercentGoodDQF() {
        return percentGoodDQF;
    }

    /**
     * @param percentGoodDQF the percentGoodDQF to set
     */
    public void setPercentGoodDQF(DelegateFieldDescription percentGoodDQF) {
        this.percentGoodDQF = percentGoodDQF;
    }

    /**
     * @return the scene
     */
    public DelegateFieldDescription getScene() {
        return scene;
    }

    /**
     * @param scene the scene to set
     */
    public void setScene(DelegateFieldDescription scene) {
        this.scene = scene;
    }

    /**
     * @return the validDQF
     */
    public int getValidDQF() {
        return validDQF;
    }

    /**
     * @param validDQF the validDQF to set
     */
    public void setValidDQF(int validDQF) {
        this.validDQF = validDQF;
    }

    /**
     * @return the debugStatus
     */
    public boolean isDebugStatus() {
        return debugStatus;
    }

    /**
     * @param debugStatus the debugStatus to set
     */
    public void setDebugStatus(boolean debugStatus) {
        this.debugStatus = debugStatus;
    }

}
