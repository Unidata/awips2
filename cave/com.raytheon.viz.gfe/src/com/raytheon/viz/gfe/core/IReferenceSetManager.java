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
package com.raytheon.viz.gfe.core;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.reference.GroupID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.viz.gfe.core.msgs.IEditAreaGroupInvChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetInvChangedListener;

/**
 * Public interface for ReferenceSetManager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 2, 2008				randerso	Initial creation
 * 02/14/2013        #1506  mnash       Move away from using QueryScript on the UI thread
 * 02/26/2013        #1708  randerso    Remove evaluateRefSet from public interface
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public interface IReferenceSetManager {

    public static enum RefSetMode {
        REPLACE("="), UNION("|"), INTERSECT("&&"), SUBTRACT("-"), USE_CURRENT(
                "?");

        private String symbol;

        private RefSetMode(String symbol) {
            this.symbol = symbol;
        }

        /**
         * @return the symbol
         */
        public String getSymbol() {
            return symbol;
        }
    };

    public static enum QuickSetMode {
        RESTORE, SAVE
    }

    /**
     * Returns the activeRefSet.
     * 
     * @return the activeRefSet
     */
    public abstract ReferenceData getActiveRefSet();

    /**
     * Sets the active ReferenceSet and sends ReferenceSetChangedMsg
     * 
     */
    public abstract void setActiveRefSet(final ReferenceData refData);

    /**
     * @return a list of reference IDs that are available
     */
    public abstract List<ReferenceID> getAvailableSets();

    /**
     * Returns an empty reference set.
     * 
     * @return the empty reference set
     */
    public abstract ReferenceData emptyRefSet();

    /**
     * Returns an full reference set.
     * 
     * @return a full reference set
     */
    public abstract ReferenceData fullRefSet();

    /**
     * Gets and returns the specified reference set. This does not affect the
     * active reference set. Note that the actual ReferenceID may change when it
     * is retrieved from the user due to the protect and access level flags.
     * 
     * Checks the cache for the data. If there, simply returns it. If not, asks
     * the IFPServer for the specified set. Converts the ReferenceData from
     * LatLon to AWIPS.
     * 
     * Reference Data stored in the cache is converted to AWIPS coordinates.
     * 
     * @param refSetID
     * @return the reference set
     */
    public abstract ReferenceData loadRefSet(final ReferenceID refSetID);

    /**
     * Saves the active reference set under the given refID in the database
     * server. Returns true if successful, false otherwise.
     * 
     * Resets the id of the active edit area. Calls the saveRefSet() function.
     * If it works, returns true.
     * 
     * @param refID
     * @return true if successful
     */
    public abstract boolean saveActiveRefSet(final ReferenceID refID);

    /**
     * Saves the specified ReferenceData set under the specified ReferenceID.
     * 
     * Checks the net for connectivity and then calls the save function.
     * 
     * @param orefData
     * @return true if successful
     */
    public abstract boolean saveRefSet(final ReferenceData orefData);

    /**
     * Deletes the named reference set from the database server. Returns true if
     * successful, false otherwise.
     * 
     * Checks the net for connectivity and then calls the delete function. If it
     * works, returns true.
     * 
     * @param refID
     * @param withVerification
     * @return true if successful
     */
    public abstract boolean deleteRefSet(final ReferenceID refID,
            boolean withVerification);

    /**
     * Undo for Reference Sets. Uses _prevRefSet
     * 
     * Calls setActiveRefSet with _prevRefSet
     * 
     */
    public abstract void undoRefSet();

    /**
     * Returns a grid of values 0.0 . 1.0 based on how far away the grid point
     * is from the edge. Any point farther than taperFactor are assigned 1.0.
     * 
     * @param refData
     * @param taperFactor
     * @return the taperGrid
     */
    public abstract Grid2DFloat taperGrid(final ReferenceData refData,
            int taperFactor);

    /**
     * Returns a grid of values 0.0 . 1.0 based on how far away from the edge
     * from the specified direction.
     * 
     * @param refData
     * @param direction
     * @return the directionTaperGrid
     */
    public abstract Grid2DFloat directionTaperGrid(final ReferenceData refData,
            final String direction);

    /**
     * Returns a set of gridpoints that match the given site identifiers. if
     * IncludeOwnSite is set, then the site identified in this running GFE will
     * be included in the set of gridpoints (if it also is given in the list).
     * 
     * Go through each site, get reference data, then convert to bits, then "or"
     * the bits from each mask.
     * 
     * This uses the ISC_xxx edit areas.
     * 
     * @param sites
     * @param includeOwnSite
     * @return the site's grid points
     */
    public abstract Grid2DBit siteGridpoints(final List<String> sites,
            boolean includeOwnSite);

    /**
     * Returns a set of gridpoints for my site.
     * 
     * Uses the ISC_xxx edit area.
     * 
     * @return my site's grid points
     */
    public abstract Grid2DBit mySiteGridpoints();

    @Override
    public abstract String toString();

    /**
     * @return the mode
     */
    public abstract RefSetMode getMode();

    /**
     * Get list of edit area groups
     * 
     * @return the group inventory
     */
    public abstract List<String> getGroupInventory();

    /**
     * Get list of edit areas in the specified group
     * 
     * @param groupName
     * @return the list of edit areas in the group
     */
    public abstract List<String> getGroupData(String groupName);

    /**
     * Get the list of know groupIds
     * 
     * @return the group Ids
     */
    public abstract List<GroupID> getGroupIds();

    /**
     * @param mode
     *            the mode to set
     */
    public abstract void setMode(RefSetMode mode);

    /**
     * @param refData
     * @param mode
     */
    public abstract void incomingRefSet(ReferenceData refData, RefSetMode mode);

    /**
     * 
     */
    public abstract void clearRefSet();

    /**
     * 
     */
    public abstract void toggleRefSet();

    public abstract void addReferenceSetInvChangedListener(
            IReferenceSetInvChangedListener listener);

    public abstract void removeReferenceSetInvChangedListener(
            IReferenceSetInvChangedListener listener);

    public abstract void addReferenceSetChangedListener(
            IReferenceSetChangedListener listener);

    public abstract void removeReferenceSetChangedListener(
            IReferenceSetChangedListener listener);

    public abstract void addReferenceSetIDChangedListener(
            IReferenceSetIDChangedListener listener);

    public abstract void removeReferenceSetIDChangedListener(
            IReferenceSetIDChangedListener listener);

    public abstract void addEditAreaGroupInvChangedListener(
            IEditAreaGroupInvChangedListener listener);

    public abstract void removeEditAreaGroupInvChangedListener(
            IEditAreaGroupInvChangedListener listener);

    /**
     * 
     */
    public abstract void toggleQuickSetMode();

    public abstract QuickSetMode getQuickSetMode();

    /**
     * @param slot
     */
    public abstract void handleQuickSet(int slot);

    /**
     * @param button
     */
    public abstract ReferenceData getQuickSet(int button);

    public void saveGroup(String groupName, List<String> areaNames);

    public void deleteGroup(String groupName);

    /**
     * Dispose of this instance
     */
    public void dispose();

    /**
     * Retrieves a list of ReferenceData corresponding to the referenceIDs
     * 
     * @param need
     *            the referenceIDs
     * @return a List of ReferenceData
     */
    public List<ReferenceData> getReferenceData(List<ReferenceID> need);

    /**
     * Add a query to the history stack
     * 
     * @param s
     */
    public void pushHistoryStack(String s);

    /**
     * 
     * @return the history stack
     */
    public List<String> getHistoryStack();

    /**
     * Returns true if the query will recurse
     * 
     * @param name
     *            query name
     * @param query
     *            query string
     * @return true is query will recurse
     */
    public boolean willRecurse(String name, String query);
}