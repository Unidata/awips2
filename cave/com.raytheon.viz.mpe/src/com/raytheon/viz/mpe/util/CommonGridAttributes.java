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
package com.raytheon.viz.mpe.util;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Contains definitions for use by netCDF and grib files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2011            snaples     Initial creation
 * Mar 5, 2013  15884      wkwock      gridPointLL and gridPointUR should be integer
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class CommonGridAttributes {

    String siteID;

    String descriptiveName;

    String units;

    String projectionType;

    String gridType;

    int len_grty, len_prty, len_units;

    Coordinate latLonLL = new Coordinate();

    Coordinate latLonUR = new Coordinate();

    Coordinate domainOrigin = new Coordinate();

    Coordinate domainExtent = new Coordinate();

    int[] gridSize = new int[2];

    int[] gridPointLL = new int[2];

    int[] gridPointUR = new int[2];

    /**
     * Time in seconds
     */
    long[][] validTimes = new long[6][2];

}
