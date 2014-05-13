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

package com.raytheon.edex.textdb.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StateMatch;
import com.raytheon.uf.common.wmo.AFOSProductId;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/04/07     400        garmendariz Initial Check in    
 * Oct 1, 2008  1538       jkorman     Added additional functionality.
 * 08/31/10     2103       cjeanbap    Updated criteria field of primary key.
 *
 * </pre>
 * 
 * @author garmendariz
 * @version 1
 */

public class StateMatchDao extends CoreDao {

    public StateMatchDao() {
        super(DaoConfig.forClass("fxa", StateMatch.class));
    }

    /**
     * Add a mapping for state -> (CCC and XXX)
     * 
     * @param stateInfo
     * @return
     */
    public boolean addState(StateMatch stateInfo) {
        this.persist(stateInfo);
        return true;
    }

    /**
     * Remove a mapping for state -> (CCC and XXX)
     * 
     * @param stateInfo
     * @return
     */
    public boolean removeState(StateMatch stateInfo) {
        this.delete(stateInfo);
        return true;
    }

    /**
     * Get a list of all entries for a specific state.
     * 
     * @param stateInfo
     *            A state to lookup.
     * @return List of StateMatch entries for the specified state. If none were
     *         found, an empty list is returned.
     */
    public List<StateMatch> queryState(String state) {
        List<StateMatch> stateList = new ArrayList<StateMatch>();

        List<?> values = null;
        try {
            values = queryBySingleCriteria("pk.state", state);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if (values != null) {
            for (Object o : values) {
                stateList.add(((StateMatch) o));
            }
        }

        return stateList;
    }

    /**
     * Create a list of AFOS commands from a state/nnn query.
     * 
     * @param state
     *            Two character state abbrevation.
     * @param nnn
     *            The category to use for the commands.
     * @return A list of product identifiers using the ccc and xxx from the
     *         state query, along with the supplied nnn.
     */
    public List<AFOSProductId> makeAFOSCommands(String state, String nnn) {
        List<AFOSProductId> commands = new ArrayList<AFOSProductId>();

        List<StateMatch> stateEntries = queryState(state);
        for (StateMatch stateEntry : stateEntries) {
            commands.add(new AFOSProductId(stateEntry.getPk().getCcc(), nnn,
                    stateEntry.getPk().getXxx()));
        }
        return commands;
    }
}
