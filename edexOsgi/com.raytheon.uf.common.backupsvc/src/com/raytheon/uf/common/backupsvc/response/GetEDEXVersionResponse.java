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
package com.raytheon.uf.common.backupsvc.response;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response to GetEDEXVersionRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2016 5937       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

@DynamicSerialize
public class GetEDEXVersionResponse {

    /** String returned in response when EDEX version is not known */
    public static final String UNDEFINED = "Undefined";

    @DynamicSerializeElement
    private String edexVersion;

    /** Host that is sending this response. */
    @DynamicSerializeElement
    private String respondingHost;

    public String getEdexVersion() {
        return edexVersion;
    }

    public void setEdexVersion(String edexVersion) {
        this.edexVersion = edexVersion;
    }

    public String getRespondingHost() {
        return respondingHost;
    }

    public void setRespondingHost(String respondingHost) {
        this.respondingHost = respondingHost;
    }

    @Override
    public String toString() {
        return "GetEDEXVersionResponse [edexVersion=" + edexVersion
                + ", respondingHost=" + respondingHost + "]";
    }

}
