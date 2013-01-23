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

import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;

/**
 * {@link IDataTimingSubsetView} that works with {@link GriddedDataSet}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Removed cycles.
 * Jan 10, 2012 1444       mpduff       Add updateSelectedForecastHours method.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IGriddedDataTimingSubsetView extends IDataTimingSubsetView {

    /**
     * Get the selected forecast hours
     * 
     * @return the selected forecast hours
     */
    String[] getSelectedFcstHours();

    /**
     * @param fcstHours
     */
    void setSelectedForecastHours(List<String> fcstHours);

    /**
     * @param fcstHours
     */
    void setAvailableForecastHours(List<String> fcstHours);

    /**
     * Update the selected forecast hours.
     * 
     * @param fcstHours
     */
    void updateSelectedForecastHours(List<String> fcstHours);
}
