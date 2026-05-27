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
package com.raytheon.viz.awipstools;

import org.locationtech.jts.geom.LineString;

/**
 * Interface for listening for baseline changes. Register with ToolsDataManager
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 13, 2021  8451     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public interface IBaselineChangedListener {

    /**
     * Called when a baseline changes
     *
     * @param name
     *            name of changed baseline
     * @param baseline
     *            new baseline LineString
     */
    void baselineChanged(String name, LineString baseline);

}
