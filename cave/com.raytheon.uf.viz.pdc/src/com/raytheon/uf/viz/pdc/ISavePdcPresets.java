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
package com.raytheon.uf.viz.pdc;

import java.text.ParseException;
import java.util.List;

import com.raytheon.viz.hydrocommon.pdc.data.PointDataPreset;

/**
 * Save Point Data Control Presets Interface.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 04, 2018   7379     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public interface ISavePdcPresets {

    /**
     * Set the time fields in PDCOptionData base on the current Value/Time
     * selections.
     *
     * @throws ParseException
     */
    void setTimeFields() throws ParseException;

    /**
     * Get the {@link PointDataPreset} data objects
     * 
     * @return List of PointDataPreset objects
     */
    List<PointDataPreset> getPresetData();

    /**
     * Populate the PDCOptionData object to select the appropriate values in the
     * Point Data Control dialog.
     *
     * @param id
     *            The preset id
     */
    void populatePresetData(String id);
}
