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
package com.raytheon.uf.edex.plugin.ffmp.common;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * FFTIAccum
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2011            dhladky     Initial creation
 * 02/01/13     1569        D. Hladky  Added constants, changed serialization, time limit
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@DynamicSerialize
public class FFTIAccum extends FFTIData {

    @DynamicSerializeElement
    private Double accumulation = 0.0;
   
    public Double getAccumulation() {
        return accumulation;
    }

    public void setAccumulation(Double accumulation) {

        this.accumulation = accumulation;
    }

}
