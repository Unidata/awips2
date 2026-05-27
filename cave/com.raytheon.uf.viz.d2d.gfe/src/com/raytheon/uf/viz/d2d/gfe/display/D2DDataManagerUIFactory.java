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
package com.raytheon.uf.viz.d2d.gfe.display;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerFactory;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.internal.GFESpatialDisplayManager;

/**
 *
 * UI-based DataManager factory used by D2D
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2019  71896      tjensen     Initial creation
 *
 * </pre>
 *
 * @author tjensen
 */
public class D2DDataManagerUIFactory extends DataManagerFactory {

    private static D2DDataManagerUIFactory instance = new D2DDataManagerUIFactory();

    public static D2DDataManagerUIFactory getInstance() {
        return instance;
    }

    @Override
    protected ISpatialDisplayManager createSpatialDisplayManager(DataManager dm,
            Object discriminator) {
        return new GFESpatialDisplayManager(dm);
    }

}
