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
package com.raytheon.uf.viz.datadelivery.subscription.subset.presenter;

import java.util.List;

import com.raytheon.viz.ui.presenter.IPresenterView;

/**
 * Implements view specific functions that the {@link DataTimingSubsetPresenter}
 * will call. For instance, the presenter will retrieve a list of available
 * dates for a specific dataset and call {@link #setDataSetDates(List)}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Changed as result of renamed objects.
 * Sep 24, 2012 1209       djohnson     isValid() moved to presenter.
 * 
 * </pre>
 *
 * @author djohnson
 * @version 1.0
 */

public interface IDataTimingSubsetView extends IPresenterView {
    /**
     * Check whether the date cycle is dirty.
     *
     * @return true if the date cycle is dirty
     */
    boolean isDirty();

    /**
     * Set whether the date cycle is dirty.
     *
     * @param b
     *            the boolean value
     */
    void setDirty(boolean b);
}
