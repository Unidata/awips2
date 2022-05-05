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
package com.raytheon.uf.common.dataplugin.radar.level3.generic;

import java.io.DataInputStream;
import java.io.IOException;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@DynamicSerialize
public class GridComponent extends GenericDataComponent {

    @Override
    public void parseData(DataInputStream in) throws IOException {
        // TODO Auto-generated method stub

    }

}
