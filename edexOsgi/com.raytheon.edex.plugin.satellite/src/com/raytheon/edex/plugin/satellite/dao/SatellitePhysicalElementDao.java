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
package com.raytheon.edex.plugin.satellite.dao;

import com.raytheon.edex.plugin.satellite.gini.SatellitePhysicalElement;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * A satellite creating entity
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SatellitePhysicalElementDao extends CoreDao {

    /**
     * Constructs a new SatellitePhysicalElementDao
     */
    public SatellitePhysicalElementDao() {
        super(DaoConfig.forClass(SatellitePhysicalElement.class));
    }

    /**
     * Retrieves a SatellitePhysicalElementDao by the given id
     * 
     * @param elementId
     *            The physical element id
     * @return The physical element
     */
    public SatellitePhysicalElement queryById(int elementId) {
        return (SatellitePhysicalElement) super.queryById(elementId);
    }
    
    public String getPhysicalElementName(int elementId){
        return queryById(elementId).getElementName();
    }
}
