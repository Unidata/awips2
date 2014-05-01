package com.raytheon.uf.common.dataplugin.ffmp;
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

import java.util.ArrayList;

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
 * 01/27/13     1478        D. Hladky   Removed un needed XML annotations
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

@DynamicSerialize
public class SourceBin implements ISerializableObject {
    
    /** sourceName and dataKey **/
    @DynamicSerializeElement
    public double[] lats;
    
    @DynamicSerializeElement
    public double[] lons;
    
    @DynamicSerializeElement
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
    
