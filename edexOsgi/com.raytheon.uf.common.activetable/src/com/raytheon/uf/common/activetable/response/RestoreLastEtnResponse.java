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
package com.raytheon.uf.common.activetable.response;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Response type for <code>RestoreLastEtnRequest</code>. Contains the last ETN
 * used and a list of hosts that could not be contacted during the process to
 * determine the next ETN and to restore the last used ETN.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 02, 2018 20557      ryu         Initial creation
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1.0
 */
@DynamicSerialize
public final class RestoreLastEtnResponse {

    @DynamicSerializeElement
    private int lastEtn;

    @DynamicSerializeElement
    private String phensig;

    @DynamicSerializeElement
    private Map<String, Integer> resultsByHost;

    @DynamicSerializeElement
    private List<String> errorMessages;

    public RestoreLastEtnResponse() {
        // for Dynamic Serialize use only.
    }

    public RestoreLastEtnResponse(int nextEtn, String phensig) {
        this(nextEtn, phensig, new HashMap<String, Integer>(),
                new ArrayList<String>());
        this.resultsByHost.put("localhost", this.lastEtn);
    }

    public RestoreLastEtnResponse(int lastEtn, String phensig,
            Map<String, Integer> resultsByHost, List<String> errorMessages) {
        this.lastEtn = lastEtn;
        this.phensig = phensig;
        this.resultsByHost = resultsByHost;
        this.errorMessages = errorMessages;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("GetNextEtnResponse [nextEtn=");
        builder.append(lastEtn);
        builder.append(", phensig=");
        builder.append(phensig);
        builder.append(", resultsByHost=");
        builder.append(resultsByHost);
        builder.append(", errorMessages=");
        builder.append(errorMessages);
        builder.append("]");
        return builder.toString();
    }

    public boolean isOkay() {
        return errorMessages.isEmpty();
    }

    public String getError() {
        return !isOkay() ? StringUtil.join(errorMessages, '\n') : "";
    }

    public void addErrorMessage(String message) {
        if (CollectionUtil.isNullOrEmpty(errorMessages)) {
            errorMessages = new ArrayList<>();
        }
        errorMessages.add(message);
    }

    public int getLastEtn() {
        return lastEtn;
    }

    public void setLastEtn(int nextEtn) {
        this.lastEtn = nextEtn;
    }

    public String getPhensig() {
        return phensig;
    }

    public void setPhensig(String phensig) {
        this.phensig = phensig;
    }

    public Map<String, Integer> getResultsByHost() {
        return resultsByHost;
    }

    public void setResultsByHost(Map<String, Integer> resultsByHost) {
        this.resultsByHost = resultsByHost;
    }

    public List<String> getErrorMessages() {
        return errorMessages;
    }

    public void setErrorMessages(List<String> errorMessages) {
        this.errorMessages = errorMessages;
    }
}
