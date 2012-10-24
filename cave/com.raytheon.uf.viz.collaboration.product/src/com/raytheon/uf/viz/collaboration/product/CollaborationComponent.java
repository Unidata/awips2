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
package com.raytheon.uf.viz.collaboration.product;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.application.component.IStandaloneComponent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationComponent implements IStandaloneComponent {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.application.component.IStandaloneComponent#startComponent
     * (java.lang.String)
     */
    @Override
    public Object startComponent(String componentName) throws Exception {
        PathManagerFactory.setAdapter(new CollaborationLocalizationAdapter());
        // TODO:
        System.out
                .println("TODO: Initialize collaboration dialog that creates similar environment as the view in eclipse.");
        return null;
    }

}
