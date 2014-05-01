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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.msgs.IAvailableSourcesChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.ILockTableChangedListener;
import com.raytheon.viz.gfe.core.msgs.INewModelAvailableListener;
import com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISystemTimeRangeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModuleJobPool;

/**
 * Placeholder for ParmManager interface
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/28/2008              chammack    Initial creation of skeleton.
 * 06/25/2012    #766      dgilling    Added getVCModulePool().
 * 08/20/2012    #1082     randerso    Moved calcStepTimes to AbstractParmManager for
 *                                     use in PngWriter
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IParmManager extends IParmInventoryChangedListener,
        ILockTableChangedListener, IParmIDChangedListener {

    public static enum TRMode {
        LT, ZULU, DBID, LST
    };

    /**
     * Called to dispose the parm manager
     */
    public void dispose();

    /**
     * This function creates a new parm - of type database or virtual
     * calculated.
     * 
     * @param pid
     * @param mutableParm
     * @param displayable
     * @return
     */
    public Parm addParm(ParmID pid, boolean mutableParm, boolean displayable);

    /**
     * This function creates a new virtual parm with the specified
     * characteristics. If the parm already exists, a null will be returned and
     * no action takes place.
     * 
     * @param pid
     * @param gpi
     * @param data
     * @param mutableParm
     * @param displayable
     * @return
     */
    public Parm createVirtualParm(ParmID pid, GridParmInfo gpi,
            IGridSlice[] data, boolean mutableParm, boolean displayable);

    /**
     * Deletes a known parm(s). Will not delete a parm that is modified.
     * 
     * @param parms
     *            parms to delete
     */
    public void deleteParm(Parm... parms);

    /**
     * Deletes all parms that are marked temporary.
     * 
     */
    public void deleteTemporaryParms();

    /**
     * Returns a list of all parms
     * 
     * @return the list of all parms
     */
    public Parm[] getAllParms();

    /**
     * Returns the list of currently displayed databases. Displayed databases
     * are defined by displayed parms.
     * 
     * @return the list of displayed databases
     */
    public List<DatabaseID> getDisplayedDbs();

    /**
     * Returns the list of currently undisplayed databases. Undisplayed
     * databases are defined by undisplayed parms.
     * 
     * @return the list of undisplayed databases
     */
    public List<DatabaseID> getUndisplayedDbs();

    /**
     * Returns a list of available databases.
     * 
     * @return the available databases, which is the sum of server+VParm, but no
     *         duplicates.
     */
    public List<DatabaseID> getAvailableDbs();

    /**
     * Returns a list of available parameters based on the database id. If an
     * invalid or unknown database id is provided, an empty list is returned.
     * 
     * @param dbID
     *            the database id
     * @return the parm ids available
     */
    public ParmID[] getAvailableParms(DatabaseID dbID);

    /**
     * Returns a list of all available parameters in all databases
     * 
     * @return
     */
    public ParmID[] getAllAvailableParms();

    /**
     * Returns a list of displayed parms
     * 
     * @return the list of displayed parms
     */
    public Parm[] getDisplayedParms();

    /**
     * NOTE: In the legacy system this was getModified() but the underlying
     * implementation was not checking for modifications, it was checking for
     * locks. It has been renamed to better match the implementation.
     * 
     * @return an array of params that are currently locked
     */
    public Parm[] getLockedParms();

    /**
     * Returns a list of parms that have been modified.
     * 
     * @return the modified parms
     */
    public Parm[] getModifiedParms();

    /**
     * Returns the mutable database
     * 
     * @return the mutable database
     */
    public DatabaseID getMutableDatabase();

    /**
     * Returns the original mutable database
     * 
     * @return the original mutable database
     */
    public DatabaseID getOrigMutableDatabase();

    /**
     * Return the parm for a parm id
     * 
     * @param parmID
     *            the parm id
     * @return the parm
     */
    public Parm getParm(ParmID parmID);

    /**
     * Return a list of Parms for a list of ParmIDs with nulls in place of parms
     * that are not loaded.
     * 
     * @param parmIDs
     * @return
     */
    public Parm[] getParms(ParmID[] parmIDs);

    /**
     * Gets the ParmID from each Parm.
     * 
     * @param parms
     *            this list of Parms
     * @return the list of ParmIDs
     */
    public ParmID[] getParmIDs(Parm[] parms);

    /**
     * This function returns a parm pointer given an expression name for a parm.
     * If exprName is "variableElement", the variableParm is returned If
     * enableTopo is on and Topo is not available, it is enabled and the parm
     * pointer is returned. All available databases are searched for all
     * available parms. The expression name is checked first for an exact match
     * with the model time, then for an exact match without the model time. If
     * no match, then this routine returns null.
     * 
     * @param exprName
     *            the expression name
     * @param enableTopo
     *            whether to enable topo
     * @param variableParm
     *            the variable parm
     * @return
     */
    public Parm getParmInExpr(final String exprName, boolean enableTopo,
            Parm variableParm);

    /**
     * Calls getParmInExpr with the active parm as the last argument
     * 
     * @param exprName
     *            the expression name
     * @param enableTopo
     *            whether to enable topo
     * @return
     */
    public Parm getParmInExpr(final String exprName, boolean enableTopo);

    /**
     * Returns a list of regular parms that are selected.
     * 
     * @return a list of all selected parms
     */
    public Parm[] getSelectedParms();

    /**
     * Returns the system time range (a time range that contains all grids of
     * all active parms)
     * 
     * @return the system time range
     */
    public TimeRange getSystemTimeRange();

    /**
     * Returns a list of undisplayed parms
     * 
     * @return the list of undisplayed parms
     */
    public Parm[] getUndisplayedParms();

    /**
     * Returns a unique parm id that cannot conflict with existing parm ids nor
     * possible parm ids from the database. Intended for use with transient
     * parameters. The nameHint is used to modify the parmname.
     * 
     * @param pid
     * @param nameHint
     * @param categoryHint
     * @return
     */
    public ParmID getUniqueParmID(final ParmID pid, final String nameHint,
            final String categoryHint);

    /**
     * Returns true if the given parm is in the system. "In the system" means
     * that it is a valid parm, but may or may not be displayed at present.
     * 
     * @param parmId
     *            the parm id to look for
     * @return whether the parm is in the system
     */
    public boolean isParmInDatabase(ParmID parmId);

    public boolean saveParm(Parm parm);

    public boolean saveParm(Parm parm, TimeRange[] timeRanges);

    /**
     * This function adds or removes parms such that the displayed list matches
     * the specified parm list. This function prevents the unloading of a
     * parameter that is modified. The list is a series of ParmIDs.
     * 
     * @param parmList
     *            the list of parm ids
     */
    public void setDisplayedParms(ParmID[] parmList);

    /**
     * Controls the displayable characteristic of the parm. A displayable parm
     * cannot be made non-displayable if it is modified.
     * 
     * @param parm
     *            the parm to set
     * @param displayable
     *            whether the parm is displayable or not
     */
    public void setParmDisplayable(Parm parm, boolean displayable);

    /**
     * Register an available sources changed listener with the parm manager
     * 
     * Notifies the recipient of when the available sources list has changed
     * 
     * @param listener
     *            the available sources change listener
     */
    public void addAvailableSourcesChangedListener(
            IAvailableSourcesChangedListener listener);

    /**
     * Unregister an available sources changed listener from the parm manager
     * 
     * @param listener
     *            the available sources change listener
     */
    public void removeAvailableSourcesChangedListener(
            IAvailableSourcesChangedListener listener);

    /**
     * Register a displayed parm change listener with the parm manager
     * 
     * Notifies the recipient of when the displayed parm list has changed
     * 
     * @param listener
     *            the displayed parm change listener
     */
    public void addDisplayedParmListChangedListener(
            IDisplayedParmListChangedListener listener);

    /**
     * Unregister a displayed parm change listener from the parm manager
     * 
     * @param listener
     *            the displayed parm change listener
     */
    public void removeDisplayedParmListChangedListener(
            IDisplayedParmListChangedListener listener);

    /**
     * Register a new model available listener with the parm manager
     * 
     * Notifies the recipient of when a new model has arrived.
     * 
     * @param listener
     *            the new model available listener
     */
    public void addNewModelAvailableListener(INewModelAvailableListener listener);

    /**
     * Unregister a new model available listener from the parm manager
     * 
     * @param listener
     *            the new model available listener
     */
    public void removeNewModelAvailableListener(
            INewModelAvailableListener listener);

    /**
     * Register a ParmID change listener with the parm manager
     * 
     * Notifies the recipient of when a Parm has had its ParmID changed.
     * 
     * @param listener
     *            the ParmID change listener
     */
    public void addParmIDChangedListener(IParmIDChangedListener listener);

    /**
     * Unregister a ParmID change listener from the parm manager
     * 
     * @param listener
     *            the ParmID change listener
     */
    public void removeParmIDChangedListener(IParmIDChangedListener listener);

    /**
     * Register a parm change listener with the parm manager
     * 
     * Notifies the recipient of when the parm list has changed
     * 
     * @param listener
     *            the parm change listener
     */
    public void addParmListChangedListener(IParmListChangedListener listener);

    /**
     * Unregister a parm change listener from the parm manager
     * 
     * @param listener
     *            the parm change listener
     */
    public void removeParmListChangedListener(IParmListChangedListener listener);

    /**
     * Unregister a sytem time range change listener with the parm manager
     * 
     * @param listener
     *            the system time range change listener
     */
    public void addSystemTimeRangeChangedListener(
            ISystemTimeRangeChangedListener listener);

    /**
     * Register a sytem time range change listener with the parm manager
     * 
     * Notifies the recipient of when the system time range has changed
     * 
     * @param listener
     *            the system time range change listener
     */
    public void removeSystemTimeRangeChangedListener(
            ISystemTimeRangeChangedListener listener);

    /**
     * Returns the grid location which is a composite of all of the mutable
     * parameters, or if no mutable parameters, a typical grid location.
     * 
     * @return
     */
    public GridLocation compositeGridLocation();

    /**
     * Deallocate all grids that have not been used in a specified number of
     * seconds
     * 
     * @param seconds
     *            number of seconds
     */
    public void deallocateUnusedGrids(int seconds);

    /**
     * Command to enable or disable the topography parm.
     * 
     * Makes assumption that topography is a vparm.
     * 
     * @param wanted
     * @param forceVisibility
     * @throws GFEOperationFailedException
     */
    public void enableDisableTopoParm(boolean wanted, boolean forceVisibility);

    /**
     * Returns a DatabaseID that represents the dbName string and the relative
     * version number. For singleton databases, the version number is ignored.
     * If no database exists, then a default DatabaseID() is returned to the
     * user. The format of the dbName field is modelName, such as "NAM12", if
     * the database has no optional type, or optType_modelName, such as
     * "D2D_NAM12", if the database has an optional type.
     * 
     * @param databaseName
     *            the db name
     * @param version
     *            the version of the db
     * @return
     */
    public DatabaseID findDatabase(String databaseName, int version);

    /**
     * Returns the corresponding ISC parmID based on the incoming parmID. Does
     * not load the parm <br>
     * 
     * Attempts to match the incoming parmname and level. Goes through each of
     * the isc databases for the match. The first one is taken.
     * 
     * @param pid
     *            The parm id to load the corresponding ISC parmID for
     * @return The ISC parmID
     */
    public ParmID getISCParmID(ParmID pid);

    /**
     * Returns the isc Databases
     * 
     * @return
     */
    public List<DatabaseID> getIscDatabases();

    /**
     * Returns true if we are in isc mode, false if in normal mode.
     * 
     * @return true if we are in isc mode, false if in normal mode.
     */
    public boolean iscMode();

    /**
     * Returns the product database as a DatabaseID.
     * 
     * @return the product database ID
     */
    public DatabaseID getProductDB();

    /**
     * Replace displayed parms that represent old model data with new model
     * data. It is taken as given the model identifier passed to this routine is
     * the newest version of this model.
     * 
     * @param modelIdentifier
     *            The identifier of the new model.
     */
    public void updateModel(DatabaseID modelIdentifier);

    /**
     * Purges the parmIDCacheServer of all database IDs for the given site
     * 
     * @param site
     *            The site to purge
     */
    public void purgeDbCacheForSite(String site);

    /**
     * @param pid
     * @param mutableParm
     * @param displayable
     * @return
     */
    public Parm createParm(ParmID pid, boolean mutableParm, boolean displayable);

    /**
     * @param parmName
     * @return
     */
    public ParmID fromExpression(String parmName);

    public JobPool getNotificationPool();

    public VCModuleJobPool getVCModulePool();
}
