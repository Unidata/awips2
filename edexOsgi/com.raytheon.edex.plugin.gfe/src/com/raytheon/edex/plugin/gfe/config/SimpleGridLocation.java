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
package com.raytheon.edex.plugin.gfe.config;

import java.awt.Point;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Python/Java configuration class for GridLocation
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/13/08     #1030      randerso    Initial port
 * 08/09/2013   #1571      randerso    Changed to use ProjectionType enum
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SimpleGridLocation {

    // GridLocation
    public Point gridSize;

    public Coordinate domainOrigin;

    public Coordinate domainExtent;

    // ProjectionData
    public String projectionID;

    public ProjectionData.ProjectionType projectionType;

    public Coordinate latLonLL;

    public Coordinate latLonUR;

    public Coordinate latLonOrigin;

    public float stdParallelOne;

    public float stdParallelTwo;

    public Point gridPointLL;

    public Point gridPointUR;

    public float latIntersect;

    public float lonCenter;

    public float lonOrigin;

    public SimpleGridLocation() {
        stdParallelOne = stdParallelTwo = 0.0f;
        latIntersect = lonCenter = lonOrigin = 0.0f;
        projectionType = ProjectionData.ProjectionType.NONE;
    }

    public SimpleGridLocation(Point gridSize, Coordinate origin,
            Coordinate extent, String projID, ProjectionType projType,
            Coordinate llll, Coordinate llur, Coordinate llo, float sp1,
            float sp2, Point gpll, Point gpur, float li, float lc, float lo) {
        this.gridSize = gridSize;
        this.domainOrigin = origin;
        this.domainExtent = extent;
        this.projectionID = projID;
        this.projectionType = projType;
        this.latLonLL = llll;
        this.latLonUR = llur;
        this.latLonOrigin = llo;
        this.stdParallelOne = sp1;
        this.stdParallelTwo = sp2;
        this.gridPointLL = gpll;
        this.gridPointUR = gpur;
        this.latIntersect = li;
        this.lonCenter = lc;
        this.lonOrigin = lo;
    }

    public void setGridSize(int x, int y) {
        gridSize = new Point(x, y);
    }
}
