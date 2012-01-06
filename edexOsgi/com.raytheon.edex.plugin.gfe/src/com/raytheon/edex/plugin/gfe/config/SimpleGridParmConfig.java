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

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Python/Java configuration class for GridParmInfo and ParmStorageInfo
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/11/08     #1030      randerso    Initial port	
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SimpleGridParmConfig {
    String parmName;

    // general GridParmInfo
    String gridType; // "Scalar", "Vector", "Weather"

    String units;

    String descriptiveName;

    float minAllowedValue;

    float maxAllowedValue;

    int precision;

    boolean timeIndependentParm;

    boolean rateParm;

    // GridLocation
    Point gridSize;

    Coordinate domainOrigin;

    Coordinate domainExtent;

    // TimeConstraints
    int startConstraint;

    int repeatConstraint;

    int durationConstraint;

    public SimpleGridParmConfig() {
        minAllowedValue = maxAllowedValue = 0.0f;
        precision = 0;
        timeIndependentParm = false;
        startConstraint = repeatConstraint = durationConstraint = 0;
        rateParm = false;
    }

    public SimpleGridParmConfig(String name, String type, String units,
            String description, float max, float min, int precision,
            boolean timeIndependentParm, Point dim, Coordinate origin,
            Coordinate extent, int start, int repeat, int duration,
            boolean rateParm) {
        this.parmName = name;
        this.gridType = type;
        this.units = units;
        this.descriptiveName = description;
        this.maxAllowedValue = max;
        this.minAllowedValue = min;
        this.precision = precision;
        this.timeIndependentParm = timeIndependentParm;
        this.gridSize = dim;
        this.domainOrigin = origin;
        this.domainExtent = extent;
        this.startConstraint = start;
        this.repeatConstraint = repeat;
        this.durationConstraint = duration;
        this.rateParm = rateParm;
    }
}
