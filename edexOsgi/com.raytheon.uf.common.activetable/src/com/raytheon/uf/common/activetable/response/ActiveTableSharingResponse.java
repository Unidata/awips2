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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response for the following active table sharing request types:
 * <ul>
 * <li>MergeActiveTableRequest
 * <li>RetrieveRemoteActiveTableRequest
 * <li>SendActiveTableRequest
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class ActiveTableSharingResponse implements ISerializableObject {

    @DynamicSerializeElement
    private boolean taskSuccess;

    @DynamicSerializeElement
    private String errorMessage;

    /**
     * Build an ActiveTableSharingResponse.
     * 
     * @param taskSuccess
     *            Whether the task tied to the request succeeded.
     * @param errorMessage
     *            Any status/error messages that could be returned relevant to
     *            the task.
     */
    public ActiveTableSharingResponse(boolean taskSuccess, String errorMessage) {
        this.taskSuccess = taskSuccess;
        this.errorMessage = errorMessage;
    }

    public boolean isTaskSuccess() {
        return taskSuccess;
    }

    public void setTaskSuccess(boolean taskSuccess) {
        this.taskSuccess = taskSuccess;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }
}
