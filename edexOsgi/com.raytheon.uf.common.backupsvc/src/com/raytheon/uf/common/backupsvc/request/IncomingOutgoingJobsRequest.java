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
package com.raytheon.uf.common.backupsvc.request;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 *
 * Parent class for {@IncomingBackupJobsRequest} and
 * {@OutgoingBackupJobsRequest}. This class is used to retrieve incoming and
 * outgoing backup job requests used in the Backup Service Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2021  91325     Lisa Singh      Initial creation
 * May 24, 2021  84473     Amanuel Challa  Added Outgoing jobs Id
 * Jul 01, 2021  93517     Amanuel Challa  Added filterQueryStr for filtering query
 * </pre>
 *
 * @author lsingh
 */
@DynamicSerialize
public class IncomingOutgoingJobsRequest implements IServerRequest {

    /**
     * The type of request being sent
     */
    @DynamicSerializeElement
    private RequestType type;

    /**
     * The CAVE site, eg OAX
     */
    @DynamicSerializeElement
    private String site;

    /**
     * A List of job IDs
     */
    @DynamicSerializeElement
    private List<Long> jobIds;

    /**
     * A SQL Query string used to filter jobs
     */
    @DynamicSerializeElement
    private String filterQueryStr;

    /** Operations for this request */
    public enum RequestType {
        /**
         * User wants to set selected backup service jobs as Accepted. Requires
         * the "data" parameter.
         **/
        ACCEPTED,

        /**
         * User wants to set the select backup service jobs as Rejected.
         * Requires the "data" parameter.
         **/
        REJECTED,

        /**
         * User wants to Wait on the selected backup services jobs. Requires the
         * "data" parameter.
         **/
        WAIT,

        /**
         * User wants to Send the selected backup services jobs. Requires the
         * "data" parameter.
         **/
        SEND,

        /**
         * User wants to Delete the selected backup services jobs. Requires the
         * "data" parameter.
         **/
        DELETE,

        /**
         * Retrieves a list of incoming/outgoing backup services jobs. The data
         * parameter can be null or not. If data is null, then all jobs for a
         * given site are returned. If data is not null, then the specified jobs
         * are returned.
         **/
        READ;

        @Override
        public String toString() {
            String name = name().toLowerCase();
            return Character.toUpperCase(name.charAt(0)) + name.substring(1);
        }
    }

    public IncomingOutgoingJobsRequest() {
    }

    public IncomingOutgoingJobsRequest(RequestType type, String site,
            List<Long> jobIDs2) {
        this.type = type;
        this.site = site;
        this.jobIds = jobIDs2;
    }

    public IncomingOutgoingJobsRequest(RequestType type, String site) {
        this.type = type;
        this.site = site;
    }

    public IncomingOutgoingJobsRequest(RequestType type, String site,
            String filterQueryStr) {
        this.type = type;
        this.site = site;
        this.filterQueryStr = filterQueryStr;
    }

    public RequestType getType() {
        return type;
    }

    public void setType(RequestType type) {
        this.type = type;
    }

    public void setJobIds(List<Long> jobIds) {
        this.jobIds = jobIds;
    }

    public List<Long> getJobIds() {
        return jobIds;
    }

    public String getSite() {
        return site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getFilterQueryStr() {
        return filterQueryStr;
    }

    public void setFilterQueryStr(String filterQueryStr) {
        this.filterQueryStr = filterQueryStr;
    }

}
