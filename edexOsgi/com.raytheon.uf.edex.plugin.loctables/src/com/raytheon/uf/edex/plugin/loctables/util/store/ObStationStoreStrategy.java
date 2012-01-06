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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.db.dao.spatial.ObStationDao;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class ObStationStoreStrategy extends AbstractStoreStrategy {

    private static final String SAVE_STATUS = "Saved new common_obs_spatial for gid=[%s]";
    
    private static final String UPDATE_STATUS = "Updated common_obs_spatial for gid=[%s]";
    
    private static final String ERROR_STATUS = "Error updating common_obs_spatial for gid=[%s] ";
    
    private Log logger = LogFactory.getLog(getClass());

    private ObStationDao dao = null;

    private boolean failed = true;
    
    public ObStationStoreStrategy() {
        dao = new ObStationDao();
        failed = false;
    }

    /**
     * 
     * @param row
     * @return Was the store successful?
     * @see com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy#store(com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow)
     */
    @Override
    public boolean store(ObStationRow row) {
        boolean success = false;
        String msg = null;
        int status = 0;
        if(dao != null) {
            if((!failed)&&(row != null)) {
                ObStation station = null;
                try {
                    station = dao.queryByGid(row.getGid());
                    if(station == null) {
                        // Entry doesn't exist, so create a new station entry
                        // and save it.
                        station = row.toObStation();
                        dao.saveOrUpdate(station);
                        msg = String.format(SAVE_STATUS, row.getGid()); 
                        logger.info(msg);

                        success = true;
                    } else {
                        // Entry exists, see if we need to change it.
                        if(ObStationRow.requiresUpdate(station,row.toObStation())) {
                            // station has been updated with change info from row
                            dao.saveOrUpdate(station);
                            msg = String.format(UPDATE_STATUS, row.getGid());
                            logger.info(msg);
                            success = true;
                        } else {
                            success = true;
                            msg = "No update required for gid=["+ row.getGid() + "]";
                        }
                    }
                } catch(Exception e) {
                    msg = String.format(ERROR_STATUS, row.getGid()); 
                    logger.info(msg,e);
                    status = -1;
                }
            }
        } else {
            msg = "ERROR:ObStationStoreStrategy.dao is null";
            status = -1;
        }
        if(msg != null) {
            postStatus(status,msg);
        }
        return success;
    }
    
    /**
     * Close has no behavior in the strategy.
     * @see java.io.Closeable#close()
     */
    @Override
    public void close() throws IOException {
        if(dao != null) {
            dao = null;
        }
    }

}
