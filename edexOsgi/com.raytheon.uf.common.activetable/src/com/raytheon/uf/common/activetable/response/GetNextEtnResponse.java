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
 * Response type for <code>GetNextEtnRequest</code>. Contains the next ETN to
 * use and a list of hosts that could not be contacted during the process to
 * determine the next ETN.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2013  #1843     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
@DynamicSerialize
public final class GetNextEtnResponse {

    @DynamicSerializeElement
    private int nextEtn;

    @DynamicSerializeElement
    private String phensig;

    @DynamicSerializeElement
    private Map<String, Integer> resultsByHost;

    @DynamicSerializeElement
    private List<String> errorMessages;

    public GetNextEtnResponse() {
        // for Dynamic Serialize use only.
    }

    public GetNextEtnResponse(int nextEtn, String phensig) {
        this(nextEtn, phensig, new HashMap<String, Integer>(),
                new ArrayList<String>());
        this.resultsByHost.put("localhost", this.nextEtn);
    }

    public GetNextEtnResponse(int nextEtn, String phensig,
            Map<String, Integer> resultsByHost, List<String> errorMessages) {
        this.nextEtn = nextEtn;
        this.phensig = phensig;
        this.resultsByHost = resultsByHost;
        this.errorMessages = errorMessages;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("GetNextEtnResponse [nextEtn=");
        builder.append(nextEtn);
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
            errorMessages = new ArrayList<String>();
        }
        errorMessages.add(message);
    }

    public int getNextEtn() {
        return nextEtn;
    }

    public void setNextEtn(int nextEtn) {
        this.nextEtn = nextEtn;
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
