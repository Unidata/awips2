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
package com.raytheon.viz.gfe.core.parm;

import org.apache.commons.lang.Validate;
import org.eclipse.core.runtime.ListenerList;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.viz.gfe.core.msgs.GridDataChangedMsg;
import com.raytheon.viz.gfe.core.msgs.IColorTableModifiedListener;
import com.raytheon.viz.gfe.core.msgs.ICombineModeChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.ILockTableChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParameterSelectionChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.IPickupValueChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISelectionTimeRangeChangedListener;
import com.raytheon.viz.gfe.core.msgs.IVectorModeChangedListener;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * 
 * Contains all of the listeners for parms
 * 
 * This is an attempt to reduce the size of Parm by putting all parm-related
 * messaging in one place. There is one instance of this class per Parm.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 13, 2008             chammack    Initial creation
 * Sep 01, 2009  #2788      randerso    Changed listener lists to sets to prevent
 *                                      multiple registration
 * Feb 23, 2012  #346       dgilling    Implement clearParmListeners.
 * Mar 01, 2012  #346       dgilling    Use identity-based ListenerLists.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ParmListeners {

    private final ListenerList gridChangedListeners;

    private final ListenerList parmInventoryChangedListeners;

    private final ListenerList parmIDChangedListeners;

    private final ListenerList selectionTimeRangeChangedListeners;

    private final ListenerList parameterSelectionChangedListeners;

    private final ListenerList combineModeChangedListeners;

    private final ListenerList vectorModeChangedListeners;

    private final ListenerList pickupValueChangedListeners;

    private final ListenerList colorTableModifiedListeners;

    private final ListenerList lockTableChangedListeners;

    private final JobPool notificationPool;

    protected ParmListeners(JobPool pool) {
        this.gridChangedListeners = new ListenerList(ListenerList.IDENTITY);
        this.parmInventoryChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.parmIDChangedListeners = new ListenerList(ListenerList.IDENTITY);
        this.selectionTimeRangeChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.parameterSelectionChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.combineModeChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.vectorModeChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.pickupValueChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.colorTableModifiedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.lockTableChangedListeners = new ListenerList(ListenerList.IDENTITY);
        this.notificationPool = pool;
    }

    protected void clearParmListeners() {
        this.gridChangedListeners.clear();
        this.parmInventoryChangedListeners.clear();
        this.parmIDChangedListeners.clear();
        this.selectionTimeRangeChangedListeners.clear();
        this.parameterSelectionChangedListeners.clear();
        this.combineModeChangedListeners.clear();
        this.vectorModeChangedListeners.clear();
        this.pickupValueChangedListeners.clear();
        this.colorTableModifiedListeners.clear();
        this.lockTableChangedListeners.clear();
    }

    public void fireGridChangedListener(final ParmID parmID,
            final TimeRange validTime) {
        for (Object listener : this.gridChangedListeners.getListeners()) {
            final IGridDataChangedListener casted = (IGridDataChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.gridDataChanged(parmID, validTime);
                }
            };

            notificationPool.schedule(notTask);
        }
        new GridDataChangedMsg(parmID, validTime).send();
    }

    protected void fireParmInventoryChangedListener(final Parm parm,
            final TimeRange validTime) {
        for (Object listener : this.parmInventoryChangedListeners
                .getListeners()) {
            final IParmInventoryChangedListener casted = (IParmInventoryChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.parmInventoryChanged(parm, validTime);
                }
            };

            notificationPool.schedule(notTask);
        }
    }

    protected void fireParmIDChangedListener(final Parm parm,
            final ParmID newParmID) {
        for (Object listener : this.parmIDChangedListeners.getListeners()) {
            final IParmIDChangedListener casted = (IParmIDChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.parmIDChanged(parm, newParmID);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    public void fireSelectionTimeRangeChanged(final Parm parm,
            final TimeRange selectionTimeRange) {
        for (Object listener : this.selectionTimeRangeChangedListeners
                .getListeners()) {
            final ISelectionTimeRangeChangedListener casted = (ISelectionTimeRangeChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.selectionTimeRangeChanged(parm, selectionTimeRange);
                }
            };

            notificationPool.schedule(notTask);
        }
    }

    public void fireParameterSelectionChangedListener(final Parm parm,
            final boolean selected) {
        for (Object listener : this.parameterSelectionChangedListeners
                .getListeners()) {
            final IParameterSelectionChangedListener casted = (IParameterSelectionChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.parameterSelectionChanged(parm, selected);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    public void fireCombineModeChangedListener(final Parm parm,
            final CombineMode mode) {
        for (Object listener : this.combineModeChangedListeners.getListeners()) {
            final ICombineModeChangedListener casted = (ICombineModeChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.combineModeChanged(parm, mode);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    public void fireVectorModeChangedListener(final Parm parm,
            final VectorMode mode) {
        for (Object listener : this.vectorModeChangedListeners.getListeners()) {
            final IVectorModeChangedListener casted = (IVectorModeChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.vectorModeChanged(parm, mode);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    public void firePickupValueChangedListener(final Parm parm,
            final WxValue pickupValue) {
        for (Object listener : this.pickupValueChangedListeners.getListeners()) {
            final IPickupValueChangedListener casted = (IPickupValueChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.pickupValueChanged(parm, pickupValue);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    public void fireColorTableModified(final Parm parm) {
        for (Object listener : this.colorTableModifiedListeners.getListeners()) {
            final IColorTableModifiedListener casted = (IColorTableModifiedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.colorTableModified(parm);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    public void fireLockTableChanged(final Parm parm, final LockTable lockTable) {
        for (Object listener : this.lockTableChangedListeners.getListeners()) {
            final ILockTableChangedListener casted = (ILockTableChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.lockTableChanged(parm, lockTable);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    /**
     * Add grid changed listener
     * 
     * @param listener
     */
    public void addGridChangedListener(IGridDataChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.gridChangedListeners.add(listener);
    }

    /**
     * Remove grid changed listener
     * 
     * @param listener
     */
    public void removeGridChangedListener(IGridDataChangedListener listener) {
        this.gridChangedListeners.remove(listener);
    }

    /**
     * Add parm inventory changed listener
     * 
     * @param listener
     */
    public void addParmInventoryChangedListener(
            IParmInventoryChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.parmInventoryChangedListeners.add(listener);
    }

    /**
     * Remove parm inventory changed listener
     * 
     * @param listener
     */
    public void removeParmInventoryChangedListener(
            IParmInventoryChangedListener listener) {
        this.parmInventoryChangedListeners.remove(listener);
    }

    /**
     * Add parm id changed listener
     * 
     * @param listener
     */
    public void addParmIDChangedListener(IParmIDChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.parmIDChangedListeners.add(listener);
    }

    /**
     * Remove parm id changed listener
     * 
     * @param listener
     */
    public void removeParmIDChangedListener(IParmIDChangedListener listener) {
        this.parmIDChangedListeners.remove(listener);
    }

    /**
     * Add selection time range changed listener
     * 
     * @param listener
     */
    public void addSelectionTimeRangeChangedListener(
            ISelectionTimeRangeChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.selectionTimeRangeChangedListeners.add(listener);
    }

    /**
     * Remove selection time range changed listener
     * 
     * @param listener
     */
    public void removeSelectionTimeRangeChangedListener(
            ISelectionTimeRangeChangedListener listener) {
        this.selectionTimeRangeChangedListeners.remove(listener);
    }

    /**
     * Add parameter selection changed listener
     * 
     * @param listener
     */
    public void addParameterSelectionChangedListener(
            IParameterSelectionChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.parameterSelectionChangedListeners.add(listener);
    }

    /**
     * Remove parameter selection changed listener
     * 
     * @param listener
     */
    public void removeParameterSelectionChangedListener(
            IParameterSelectionChangedListener listener) {
        this.parameterSelectionChangedListeners.remove(listener);
    }

    /**
     * Add combine mode changed listener
     * 
     * @param listener
     */
    public void addCombineModeChangedListener(
            ICombineModeChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.combineModeChangedListeners.add(listener);
    }

    /**
     * Remove combine mode changed listener
     * 
     * @param listener
     */
    public void removeCombineModeChangedListener(
            ICombineModeChangedListener listener) {
        this.combineModeChangedListeners.remove(listener);
    }

    /**
     * Add vector mode changed listener
     * 
     * @param listener
     */
    public void addVectorModeChangedListener(IVectorModeChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.vectorModeChangedListeners.add(listener);
    }

    /**
     * Remove vector mode changed listener
     * 
     * @param listener
     */
    public void removeVectorModeChangedListener(
            IVectorModeChangedListener listener) {
        this.vectorModeChangedListeners.remove(listener);
    }

    /**
     * Add pickup value changed listener
     * 
     * @param listener
     */
    public void addPickupValueChangedListener(
            IPickupValueChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.pickupValueChangedListeners.add(listener);
    }

    /**
     * Remove pickup value changed listener
     * 
     * @param listener
     */
    public void removePickupValueChangedListener(
            IPickupValueChangedListener listener) {
        this.pickupValueChangedListeners.remove(listener);
    }

    /**
     * Add color table modified listener
     * 
     * @param listener
     */
    public void addColorTableModifiedListener(
            IColorTableModifiedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.colorTableModifiedListeners.add(listener);
    }

    /**
     * Remove color table modified listener
     * 
     * @param listener
     */
    public void removeColorTableModifiedListener(
            IColorTableModifiedListener listener) {
        this.colorTableModifiedListeners.remove(listener);
    }

    /**
     * Add lock table changed listener
     * 
     * @param listener
     */
    public void addLockTableChangedListener(ILockTableChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        this.lockTableChangedListeners.add(listener);
    }

    /**
     * Remove lock table changed listener
     * 
     * @param listener
     */
    public void removeLockTableChangedListener(
            ILockTableChangedListener listener) {
        this.lockTableChangedListeners.remove(listener);
    }

}
