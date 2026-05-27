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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.ArrayList;
import java.util.List;

/**
 * ProcedureComm
 * 
 * Provides communication support between history list and procedure dialog
 * 
 * NOTE: Bundles are transferred using strings to guarantee deep copies.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 13, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ProcedureComm {

    private static ProcedureComm instance;

    public List<ICopyOutListener> copyOutListeners;

    public List<ICopyOutStateChangeListener> copyOutStateListeners;

    public static class BundlePair {
        public String name;

        public String xml;
    }

    public static synchronized ProcedureComm getInstance() {
        if (instance == null) {
            instance = new ProcedureComm();
        }
        return instance;
    }

    /**
     * Constructor
     */
    private ProcedureComm() {
        copyOutListeners = new ArrayList<ICopyOutListener>();
        copyOutStateListeners = new ArrayList<ICopyOutStateChangeListener>();
    }

    /**
     * Add a copy out listener
     * 
     * @param listener
     *            the listener to add
     */
    public void addCopyOutListener(ICopyOutListener listener) {
        this.copyOutListeners.add(listener);
        refreshCopyOutState();
    }

    /**
     * Remove a copy out listener
     * 
     * @param listener
     *            the listener to remove
     */
    public void removeCopyOutListener(ICopyOutListener listener) {
        this.copyOutListeners.remove(listener);
        refreshCopyOutState();
    }

    /**
     * Add a state listener
     * 
     * @param listener
     *            the state listener
     */
    public void addCopyOutStateListener(ICopyOutStateChangeListener listener) {
        this.copyOutStateListeners.add(listener);
    }

    /**
     * Remove a state listener
     * 
     * @param listener
     *            the state listener
     */
    public void removeCopyOutStateListener(ICopyOutStateChangeListener listener) {
        this.copyOutStateListeners.remove(listener);
    }

    /**
     * Call copy out
     * 
     * @param b
     *            the bundle to copy out
     * @param src
     *            the object that called copy out
     */
    public void copyOut(BundlePair b, Object src) {
        for (ICopyOutListener l : this.copyOutListeners) {
            l.copyOut(b, src);
        }
    }

    public void refreshCopyOutState() {
        for (ICopyOutStateChangeListener l : this.copyOutStateListeners) {
            l.copyOutStateChange();
        }
    }

    public int getCopyListenerCount() {
        return this.copyOutListeners.size();

    }

    public static interface ICopyOutListener {
        /**
         * Defines a copy out listener
         * 
         * @param b
         *            the bundle to copy out
         * @param src
         *            the class that generated the message
         */
        public void copyOut(BundlePair b, Object src);
    }

    public static interface ICopyOutStateChangeListener {
        /**
         * Defines a copy out state change
         */
        public void copyOutStateChange();
    }

}
