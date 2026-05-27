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
package com.raytheon.uf.common.grib;

import java.util.Set;

import com.raytheon.uf.common.gridcoverage.GridCoverage;

/**
 * Interface for looking up the names of grid coverages that are defined in the
 * grid coverage xml based off an existing coverage. Most coverages have only a
 * single name but this class handles multiple names for equivalent coverage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public interface GribCoverageNameLookup {

    /**
     * This method provides a way to get the names from the definition files for
     * looking up a grib model. It will return all the names of any coverages
     * defined in the grid definition files that are spatially equivalent to the
     * passed in coverage. This is useful when there are multiple grid
     * definition files with the same spatial attributes but different names or
     * for cases where the name in the definition file does not match what is
     * currently in the db.
     */
    public Set<String> getGribCoverageNames(GridCoverage coverage);
}
