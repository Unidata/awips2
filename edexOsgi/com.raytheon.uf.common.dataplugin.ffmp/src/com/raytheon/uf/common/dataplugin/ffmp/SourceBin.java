package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * FFMP source binning object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/22/10      6581       D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SourceBin implements ISerializableObject {
    
    /** sourceName and dataKey **/
    @DynamicSerializeElement
    @XmlElement
    public double[] lats;
    
    @DynamicSerializeElement
    @XmlElement
    public double[] lons;
    
    @DynamicSerializeElement
    @XmlElement
    public double[] areaPercent;
      
    public SourceBin() {
        
    }
    
    public SourceBin(Coordinate[] coors, double[] areaPercent) {
        lats = new double[coors.length];
        lons = new double[coors.length];
        this.areaPercent = areaPercent;
        for (int i = 0; i < coors.length;i++) {
            lons[i] = coors[i].x;
            lats[i] = coors[i].y;
        }
    }
    
    public SourceBin(ArrayList<SourceBinEntry> sbes) {
        lats = new double[sbes.size()];
        lons = new double[sbes.size()];
        areaPercent = new double[sbes.size()];
        for (int i = 0; i < sbes.size(); i++) {
            Coordinate coor = sbes.get(i).getCoor();
            lons[i] = coor.x;
            lats[i] = coor.y;
            areaPercent[i] = sbes.get(i).getArea();
        }
    }
    
    public double[] getLats() {
        return lats;
    }


    public void setLats(double[] lats) {
        this.lats = lats;
    }


    public double[] getLons() {
        return lons;
    }


    public void setLons(double[] lons) {
        this.lons = lons;
    }
    
    public Coordinate[] getCoordinates() {
        Coordinate[] coors = new Coordinate[lats.length];
        for(int i = 0;i < lats.length;i++) {
            coors[i] = new Coordinate(lons[i], lats[i]);
        }
        return coors;
    }

    /**
     * Gets the entries for the source bins
     * @return
     */
    public ArrayList<SourceBinEntry> getEntries() {

        ArrayList<SourceBinEntry> entries = new ArrayList<SourceBinEntry>(
                lats.length);
        for (int i = 0; i < lats.length; i++) {
            SourceBinEntry sbe = new SourceBinEntry();
            sbe.setCoor(new Coordinate(lons[i], lats[i]));
            sbe.setArea(areaPercent[i]);
            entries.add(sbe);
        }

        return entries;
    }
    
    public double[] getAreaPercent() {
        return areaPercent;
    }
    
    public double getPercent(int i) {
        return areaPercent[i];
    }

    public void setAreaPercent(double[] areaPercent) {
        this.areaPercent = areaPercent;
    }

}
    
