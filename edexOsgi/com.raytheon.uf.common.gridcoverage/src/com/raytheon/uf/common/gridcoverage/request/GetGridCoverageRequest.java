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
package com.raytheon.uf.common.gridcoverage.request;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to lookup a grid coverage that is spatially equivelant to the
 * provided coverage, also provides option for creating the coverage in the
 * database if it does not already exist.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
public class GetGridCoverageRequest implements IServerRequest {

    @DynamicSerializeElement
    private GridCoverage coverage;

    @DynamicSerializeElement
    private boolean create = false;

    public GetGridCoverageRequest() {
    }

    public GetGridCoverageRequest(GridCoverage coverage) {
        this.coverage = coverage;
        this.create = false;
    }

    public GetGridCoverageRequest(GridCoverage coverage, boolean create) {
        this.coverage = coverage;
        this.create = create;
    }

    public GridCoverage getCoverage() {
        return coverage;
    }

    public void setCoverage(GridCoverage coverage) {
        this.coverage = coverage;
    }

    public boolean getCreate() {
        return create;
    }

    public void setCreate(boolean create) {
        this.create = create;
    }

}
