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
package com.raytheon.uf.common.dataplugin.fog.analysis;

import java.awt.Point;
import java.util.ArrayList;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.vividsolutions.jts.geom.Coordinate;

public class FogCell {

    public int left;
    public int right;
    public int top;
    public int bottom;
   
    public int startX = 0;
    public int startY = 0;
    public int endX = 0;
    public int endY = 0;
    public int nx = 0;
    public int ny = 0;
    
    /**  Threat Grid Coordinate of this CELL **/
    public Coordinate threatCoor = null;
    
    public ArrayList<Point> points = null;
        
    public FOG_THREAT cellThreat = null;
    
    public FogRecord.IMAGE_GROUP group = null;
    
    /**
     * serialization
     */
    public FogCell() {
        
    }

    public FogCell(Coordinate threatCoor, GridGeometry2D threatGrid,
            GridGeometry2D pixelGrid) throws TransformException, FactoryException {

        this.threatCoor = threatCoor;
        // find the corner points in pixel land
        // threat grid i and j
        ReferencedCoordinate rc = new ReferencedCoordinate(threatCoor);
        Coordinate leftc = rc.asGridCell(pixelGrid, PixelInCell.CELL_CORNER);
        ReferencedCoordinate rc2 = new ReferencedCoordinate(threatCoor);
        Coordinate center = rc2.asGridCell(pixelGrid, PixelInCell.CELL_CENTER);

        top = (int) leftc.y;
        bottom = (int) (leftc.y + ((leftc.y - center.y) * 2));
        left = (int) leftc.x;
        right = (int) (leftc.x + ((center.x - leftc.x) * 2));
        ny = (bottom - top);
        nx = (right - left);
        startX = left;
        endX = right;
        startY = top;
        endY = bottom;
      
    }
    
    // ---------------------------------------------------------------------------
    // Name: Domain
    // Type: Public member function
    //
    // Description:
    // To get he minimum rectangle which encloses the fog cell area.
    // hereafter it is called DOMAIN.
    // Input Argument:
    // left: left bound of the DOMAIN.
    // right: right bound of the DOMAIN.
    // top: top bound of the DOMAIN.
    // bottom: bottom bound of the DOMAIN.
    // Output Argument:
    // Pass the references of the two points into the function.
    // History:
    // March 2004 ------ Qin Zeng(GDMB/MDL) created
    // Dec 22, 2009     D Hladky ported to JAVA for AWIPS II
    // --------------------------------------------------------------------------
    
    public FogCell(ArrayList<Point> points, FOG_THREAT cellThreat, FogRecord.IMAGE_GROUP group) {
        
        this.points = points;
        this.cellThreat = cellThreat;
        this.group = group;
    }
      
    public void setCellThreat(FOG_THREAT threat) {
        this.cellThreat = threat;
    }
      
    /**
     * Gets the consensus threat for this cell
     * @return
     */
    public FOG_THREAT getCellThreat() {
        return cellThreat;
    }
    
    /**
     * Gets the image grouping for this cell
     * @return
     */
    public FogRecord.IMAGE_GROUP getImageGroup() {
        return group;
    }
    
    /**
     * Get all of the pixels included in this cell
     * @return
     */
    public ArrayList<Point> getPoints() {
        return points;
    }
    
    /**
     * Gets you the area in "Pixels" so to speak
     * @return
     */
    public float getArea() {
        return nx*ny;
    }
    
    /**
     * Gets the pixel relative value for this x/y into the VIS/IR arrays
     * @param x
     * @param y
     * @return
     */
    public Coordinate getPixelRelativeCoordinate(int i)  {
        Coordinate pixelCoor = getPixelCoordinate(i);
        return new Coordinate(startX+pixelCoor.x, startY+pixelCoor.y);
    }
    
    /**
     * Find the pixel coordinate relative to the cell
     * @param total
     * @return
     */
    public Coordinate getPixelCoordinate(int total) {
        return new Coordinate(total/(total/nx), total/(total/ny));
    }
    
    /**
     * clear this cell to default
     */
    public void clear() {
        cellThreat = FOG_THREAT.GRAY;
    }
    
}
