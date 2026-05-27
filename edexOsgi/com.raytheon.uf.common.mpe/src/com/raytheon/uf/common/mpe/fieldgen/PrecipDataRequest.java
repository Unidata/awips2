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
package com.raytheon.uf.common.mpe.fieldgen;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Used to request mpe precipitation data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */
@DynamicSerialize
public class PrecipDataRequest implements IServerRequest {

    @DynamicSerializeElement
    private PrecipField field;

    @DynamicSerializeElement
    private PrecipDataKey precipDataKey;

    public PrecipDataRequest() {
    }

    public PrecipDataRequest(final PrecipDataKey precipDataKey) {
        this.precipDataKey = precipDataKey;
    }

    public PrecipField getField() {
        return field;
    }

    public void setField(PrecipField field) {
        this.field = field;
    }

    public PrecipDataKey getPrecipDataKey() {
        return precipDataKey;
    }

    public void setPrecipDataKey(PrecipDataKey precipDataKey) {
        this.precipDataKey = precipDataKey;
    }
}