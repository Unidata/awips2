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
package com.raytheon.uf.viz.grid.radar;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;

/**
 * A LevelNode for the pressure at the height of a radar tilt. The data will be
 * centered on a specific radar site.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Sep 08, 2021  8652     njensen   Initial creation
 * Oct 14, 2024  2037939  mapeters  Extend TiltTemporalGridDataLevelNode
 *                                  to calculate from true elevation angle
 *
 * </pre>
 *
 */
public class RcpGridDataLevelNode extends TiltTemporalGridDataLevelNode {

    public RcpGridDataLevelNode(String modelName, Level primaryAngleLevel) {
        super(modelName, primaryAngleLevel);
    }

    @Override
    protected AbstractRequestableData buildData(TimeAndSpace tas) {
        return new RcpRequestableData(modelName, level, tas);
    }
}