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
package com.raytheon.uf.common.dataplugin.gfe.request;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Get Topo Data request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2013      #2331 randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class GetTopoDataRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private GridLocation gloc;

    /**
     * Default constructor
     */
    public GetTopoDataRequest() {
    }

    /**
     * Constructor
     * 
     * @param gloc
     *            GridLocation for topo data
     */
    public GetTopoDataRequest(GridLocation gloc) {
        this.gloc = gloc;
    }

    /**
     * @return the gloc
     */
    public GridLocation getGloc() {
        return gloc;
    }

    /**
     * @param gloc
     *            the gloc to set
     */
    public void setGloc(GridLocation gloc) {
        this.gloc = gloc;
    }
}
