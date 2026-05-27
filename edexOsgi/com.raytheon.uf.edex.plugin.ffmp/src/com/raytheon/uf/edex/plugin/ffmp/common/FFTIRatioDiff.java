package com.raytheon.uf.edex.plugin.ffmp.common;

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

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * FFTIRatioDiff
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2011            dhladky     Initial creation
 * 02/01/13    1569        D. Hladky   Added constants, serialization changes
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class FFTIRatioDiff extends FFTIData {

    @DynamicSerializeElement
    private List<Float> qpes;

    @DynamicSerializeElement
    private List<Float> guids;

    public FFTIRatioDiff() {

    }

    public FFTIRatioDiff(List<Float> qpes, List<Float> guids,
            Double gap) {
        setQpes(qpes);
        setGuids(guids);
        setGap(gap);
    }

    public List<Float> getQpes() {
        return qpes;
    }

    public void setQpes(List<Float> qpes) {
        this.qpes = qpes;
    }

    public List<Float> getGuids() {
        return guids;
    }

    public void setGuids(List<Float> guids) {
        this.guids = guids;
    }

}
