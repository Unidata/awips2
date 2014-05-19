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

package com.raytheon.uf.edex.plugin.text.dao;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Criteria;

import com.raytheon.uf.common.dataplugin.text.db.WatchWarn;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * The dao implementation associated with the TextDao classes used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/04/07     400         garmendariz Initial Check in    
 * Oct 1, 2008  1538        jkorman     Added additional functionality.
 * Aug 9, 2010  3944        cjeanbap    Added method, queryAllWatchWarn. 
 * May 20, 2014 2536        bclement    moved from edex.textdb to edex.plugin.text
 * </pre>
 * 
 * @author garmendariz
 * @version 1
 */

public class WatchWarnDao extends CoreDao {

    /**
     * 
     */
    public WatchWarnDao() {
        super(DaoConfig.forClass("fxa", WatchWarn.class));
    }

    /**
     * Add an entry to the WatchWarn table.
     * 
     * @param watchWarn
     */
    public boolean addEntry(WatchWarn watchWarn) {
        this.persist(watchWarn);
        return true;
    }

    /**
     * Add an entry to the WatchWarn table.
     * 
     * @param watchWarn
     */
    public boolean deleteEntry(WatchWarn watchWarn) {
        this.delete(watchWarn);
        return true;
    }

    /**
     * Get a list of all scripts associated with a given product identifier.
     * 
     * @param productId
     *            A product identifier to query for.
     * @return List of scripts retrieved. Returns an empty list if no values
     *         were found.
     */
    public List<String> queryWatchWarn(String productId) {

        List<String> retList = new ArrayList<String>();

        List<?> values = null;
        try {
            values = queryBySingleCriteria("productid", productId);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if (values != null) {
            for (Object o : values) {
                retList.add(((WatchWarn) o).getScript());
            }
        }

        return retList;
    }
    
    @SuppressWarnings("unchecked")
    public List<WatchWarn> queryAllWatchWarn() {
        
        Criteria criteria = getSession().createCriteria(WatchWarn.class);   
        
        return criteria.list();
    }
}
