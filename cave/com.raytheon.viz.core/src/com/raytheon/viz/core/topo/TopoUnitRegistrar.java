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
package com.raytheon.viz.core.topo;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.UnitFormat;

import com.raytheon.viz.core.units.IUnitRegistrar;

/**
 * Registers units for topo
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TopoUnitRegistrar implements IUnitRegistrar {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.units.IUnitRegistrar#register()
     */
    @Override
    public void register() {
        UnitFormat.getUCUMInstance().label(SI.KILO(NonSI.FOOT), "kft");
    }

}
