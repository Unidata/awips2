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
package com.raytheon.uf.common.mpe.gribit2;

import com.raytheon.uf.common.mpe.gribit2.grid.IGridDefinition;

/**
 * Indicates that a {@link IGridDefinition} could not be found.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class GridDefinitionNotFoundException extends Exception {

    private static final long serialVersionUID = -6412677328308588786L;

    private static final String ERR_MSG_FMT = "Failed to find a grid definition for GRIB grid number: %s.";

    public GridDefinitionNotFoundException(final int gribNum) {
        super(String.format(ERR_MSG_FMT, gribNum));
    }
}