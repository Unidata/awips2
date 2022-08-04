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
package com.raytheon.uf.viz.preciprate;

import com.raytheon.viz.core.units.IUnitRegistrar;

import tec.uom.se.format.SimpleUnitFormat;
import tec.uom.se.unit.Units;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PrecipUnits implements IUnitRegistrar {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.units.IUnitRegistrar#register()
     */
    @Override
    public void register() {
        SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).alias(Units.HOUR, "hr");
    }

}
