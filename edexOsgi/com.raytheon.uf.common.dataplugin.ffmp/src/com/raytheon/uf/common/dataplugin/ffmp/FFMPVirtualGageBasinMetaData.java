package com.raytheon.uf.common.dataplugin.ffmp;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Holds VGB FFMP basin Meta Data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01may10      3937       dhladky     Setup
 * 01mar13      DR13228    gzhang      Add basin state
 * Aug 08, 2015 4722        dhladky    Dynamic serialize imp not needed.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class FFMPVirtualGageBasinMetaData  {

    /** basin id(key) in GIS **/
    @DynamicSerializeElement
    public String lid;

    /** which pfaf (basin) it is in **/
    @DynamicSerializeElement
    public Long parentPfaf;

    /** basin description **/
    @DynamicSerializeElement
    public String name;

    /** basin county **/
    @DynamicSerializeElement
    public String county;
    
    /** basin state DR 13228 **/
    @DynamicSerializeElement
    public String state;

    /** basin county **/
    @DynamicSerializeElement
    public String cwa;

    /** VGB coordinate **/
    @DynamicSerializeElement
    public Coordinate coordinate = new Coordinate();
    
    /** unique id for the gage in a data source */
    @DynamicSerializeElement
    public Long lookupId;

    public FFMPVirtualGageBasinMetaData() {

    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public Coordinate getCoordinate() {
        return coordinate;
    }

    public void setCoordinate(Coordinate coordinate) {
        this.coordinate = coordinate;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCounty() {
        return county;
    }

    public void setCounty(String county) {
        this.county = county;
    }

    public String getState() {
        return state;// DR 13228
    }

    public void setState(String state) {
        this.state = state;// DR 13228
    }
    
    public Long getParentPfaf() {
        return parentPfaf;
    }

    public void setParentPfaf(Long parentPfaf) {
        this.parentPfaf = parentPfaf;
    }

    public String getCwa() {
        return cwa;
    }

    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    public Long getLookupId() {
        return lookupId;
    }

    public void setLookupId(Long lookupId) {
        this.lookupId = lookupId;
    }

}