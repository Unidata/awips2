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
package com.raytheon.uf.edex.registry.acp.xacml.objects;

import java.util.ArrayList;
import java.util.List;

import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.StatusCodeType;
import org.opensaml.xacml.policy.EffectType;
import org.opensaml.xacml.policy.ObligationType;
import org.opensaml.xacml.policy.ObligationsType;

/**
 * 
 * General use class for encapsulating match states for matching various
 * elements of a policy to a request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Match {

    /** Static deny match */
    public static final Match DENY = new Match(DECISION.Deny);

    /** Static permit match */
    public static final Match PERMIT = new Match(DECISION.Permit);

    /** Static not applicable match */
    public static final Match NOT_APPLICABLE = new Match(DECISION.NotApplicable);

    /**
     * Enum holding the various match types
     */
    public static enum MATCH_TYPE {
        MATCH, NO_MATCH, INDETERMINATE
    }

    /** The result of a match request */
    private DECISION match;

    /** The message associated with the match response */
    private String message;

    /** The XACML response code associated with this match */
    private String statusCode = StatusCodeType.SC_OK;

    /** List of applicable obligations that need to be applied for this match */
    private List<ObligationType> obligations = new ArrayList<ObligationType>();

    /**
     * Creates a new match object
     * 
     * @param match
     *            The decision
     * @param statusCode
     *            The status code
     * @param message
     *            The message
     */
    public Match(DECISION match, String statusCode, String message) {
        this.match = match;
        this.message = message;
        this.statusCode = statusCode;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append("\n");
        buf.append("      MATCH: ").append(this.match).append("::")
                .append(this.statusCode).append("\n");
        return buf.toString();
    }

    /**
     * Creates a new match object
     * 
     * @param match
     *            The decision
     * @param statusCode
     *            The status code
     */
    public Match(DECISION match, String statusCode) {
        this(match, statusCode, "");
    }

    /**
     * Creates a new match object
     * 
     * @param match
     *            The decision
     */
    public Match(DECISION match) {
        this(match, StatusCodeType.SC_OK, "");
    }

    /**
     * Creates a new match object. Copy constructor
     * 
     * @param match
     *            The match object to copy
     */
    public Match(Match match) {
        this.match = match.getMatch();
        this.message = match.getMessage();
        this.statusCode = match.getStatusCode();
    }

    /**
     * Creates a new Indeterminate match with the provided status code
     * 
     * @param statusCode
     *            The status code
     * @return The Indeterminate match with the given status code
     */
    public static Match indeterminate(String statusCode) {
        return new Match(DECISION.Indeterminate, statusCode);
    }

    /**
     * Sets obligations that are applicable. Obligations are only applicable if
     * the decision matches the effect and the decision is Permit or Deny
     * 
     * @param obligationsObject
     *            The object holding the obligations
     */
    public void setApplicableObligations(ObligationsType obligationsObject) {
        if (obligationsObject != null) {
            for (ObligationType obligation : obligationsObject.getObligations()) {
                if ((obligation.getFulfillOn().equals(EffectType.Permit) && match
                        .equals(DECISION.Permit))
                        || (obligation.getFulfillOn().equals(EffectType.Deny) && match
                                .equals(DECISION.Deny))) {
                    this.obligations.add(obligation);
                }
            }
        }
    }

    /**
     * @return the match
     */
    public DECISION getMatch() {
        return match;
    }

    /**
     * @param match
     *            the match to set
     */
    public void setMatch(DECISION match) {
        this.match = match;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the statusCode
     */
    public String getStatusCode() {
        return statusCode;
    }

    /**
     * @param statusCode
     *            the statusCode to set
     */
    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    /**
     * @return the obligations
     */
    public List<ObligationType> getObligations() {
        return obligations;
    }

    /**
     * @param obligations
     *            the obligations to set
     */
    public void setObligations(List<ObligationType> obligations) {
        this.obligations = obligations;
    }

}
