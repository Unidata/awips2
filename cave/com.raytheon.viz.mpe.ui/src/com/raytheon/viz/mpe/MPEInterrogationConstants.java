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
package com.raytheon.viz.mpe;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Interrogation constants for MPE resources to use in returned {@link Map} from
 * {@link AbstractVizResource#interrogate(ReferencedCoordinate)}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEInterrogationConstants {

    public static final String INTERROGATE_BASIN = "Basin";

    public static final String INTERROGATE_COUNTY = "County";

    public static final String INTERROGATE_LAT_LON = "LatLon";

    public static final String INTERROGATE_GRID_CELL = "GridCell";

    public static final String INTERROGATE_VALUE = "Value";

    public static final String INTERROGATE_VALUE_LABEL = "ValueLabel";

    public static final String INTERROGATE_UNIT = "Unit";

}
