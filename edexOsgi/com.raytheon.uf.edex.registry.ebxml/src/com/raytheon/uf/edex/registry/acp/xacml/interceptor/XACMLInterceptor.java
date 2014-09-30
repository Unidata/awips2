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
package com.raytheon.uf.edex.registry.acp.xacml.interceptor;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.FilterObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;

import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.security.AccessDeniedException;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageContentsList;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.security.LoginSecurityContext;
import org.apache.cxf.security.SecurityContext;
import org.apache.ws.security.saml.ext.OpenSAMLUtil;
import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.DecisionType;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.ctx.ResultType;
import org.opensaml.xacml.policy.ObligationType;
import org.opensaml.xacml.policy.ObligationsType;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.acp.xacml.XACMLPolicyAdministrator;
import com.raytheon.uf.edex.registry.acp.xacml.XACMLPolicyDecisionPoint;
import com.raytheon.uf.edex.registry.acp.xacml.engine.obligation.XACMLObligationEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * 
 * Policy enforcement point (PEP) - The system entity that performs access
 * control, by making decision requests and enforcing authorization decisions.
 * This term is defined in a joint effort by the IETF Policy Framework Working
 * Group and the Distributed Management Task Force (DMTF)/Common Information
 * Model (CIM) in [RFC3198]. This term corresponds to "Access Enforcement
 * Function" (AEF) in [ISO10181-3].
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/09/2014    724          bphillip    Initial Coding
 * 8/08/2014    1720         bphillip    Implemented support for custom XACML access policies
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
public class XACMLInterceptor extends AbstractPhaseInterceptor<Message> {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(XACMLInterceptor.class);

    /** The slot containing the id of the custom XACML policy */
    private static final String POLICY_SLOT_NAME = "urn:oasis:names:tc:ebxml-regrep:rim:RegistryObject:accessControlPolicy";

    /** The id of the default XACML policy set */
    private static final String DEFAULT_POLICY = "urn:oasis:names:tc:xacml:2.0:data-delivery:default-policySet";

    /** XACML Policy Administrator object which manages XACML policies */
    private XACMLPolicyAdministrator xacmlPolicyAdmin;

    /** The XACML Policy Decision point */
    private XACMLPolicyDecisionPoint pdp;

    /** Registry object data access object used for registry objects */
    private RegistryObjectDao registryObjectDao;

    /**
     * Builder object that builds XACML authorization requests from the registry
     * requests
     */
    private RegistryXACMLRequestBuilder requestBuilder;

    public XACMLInterceptor() {
        super(Phase.PRE_INVOKE);
    }

    /**
     * Constructs a new XACMLInterceptor
     * 
     * @param xacmlPolicyAdmin
     *            The policy admin
     * @param pdp
     *            The policy decision point
     * @param registryObjectDao
     *            The registry object data access object
     */
    public XACMLInterceptor(XACMLPolicyAdministrator xacmlPolicyAdmin,
            XACMLPolicyDecisionPoint pdp, RegistryObjectDao registryObjectDao) {
        this(Phase.PRE_INVOKE, xacmlPolicyAdmin, pdp, registryObjectDao);
    }

    /**
     * Constructs a new XACMLInterceptor
     * 
     * @param phase
     *            The phase in the CXF Interceptor chain that this interceptor
     *            gets executed
     * @param xacmlPolicyAdmin
     *            The policy admin
     * @param pdp
     *            The policy decision point
     * @param registryObjectDao
     *            The registry object data access object
     */
    public XACMLInterceptor(String phase,
            XACMLPolicyAdministrator xacmlPolicyAdmin,
            XACMLPolicyDecisionPoint pdp, RegistryObjectDao registryObjectDao) {
        super(phase);
        OpenSAMLUtil.initSamlEngine();
        this.xacmlPolicyAdmin = xacmlPolicyAdmin;
        this.pdp = pdp;
        this.registryObjectDao = registryObjectDao;
        requestBuilder = new RegistryXACMLRequestBuilder();
    }

    @Override
    public void handleMessage(Message message) throws Fault {
        SecurityContext sc = message.get(SecurityContext.class);

        boolean isSoapCall = (message != null && message
                .get(Message.WSDL_OPERATION) != null);

        if (sc instanceof LoginSecurityContext) {
            LoginSecurityContext loginSecurityContext = (LoginSecurityContext) sc;
            Principal principal = sc.getUserPrincipal();
            Set<Principal> principalRoles = loginSecurityContext.getUserRoles();
            List<String> roles = new ArrayList<String>();
            if (principalRoles != null) {
                for (Principal p : principalRoles) {
                    if (p != principal) {
                        roles.add(p.getName());
                    }
                }
            }

            List<RequestType> requestList = null;
            List<RegistryObjectType> resources = null;
            try {
                resources = getResources(message, isSoapCall);
                requestList = requestBuilder.createRequestList(isSoapCall,
                        principal, roles, message, resources);
            } catch (Exception e1) {
                throw new SecurityException("Error generating XACML requests!",
                        e1);
            }
            try {
                boolean accessPermitted = true;
                for (int i = 0; i < requestList.size() && accessPermitted; i++) {
                    ResponseType response = performRequest(requestList.get(i),
                            resources.get(i));
                    ResultType result = response.getResult();

                    // Handle any Obligations returned by the PDP
                    handleObligations(requestList.get(i), principal, message,
                            result);

                    if (result != null
                            && (result.getDecision().getDecision() == DecisionType.DECISION.Permit)
                            && (result.getResourceId() == null)) {
                        continue;
                    }
                    statusHandler.warn("XACML authorization not permitted:");
                    accessPermitted = false;
                }
                if (accessPermitted) {
                    return;
                }
            } catch (Exception e) {
                statusHandler
                        .error("An error occurred during XACML authorization. Defaulting to Unauthorized",
                                e);
                throw new AccessDeniedException("Unauthorized");
            }
        } else {
            statusHandler
                    .error("The SecurityContext was not an instance of LoginSecurityContext. No authorization "
                            + "is possible as a result");
        }

        throw new AccessDeniedException("Unauthorized");

    }

    /**
     * Performs the XACML authorization request
     * 
     * @param request
     *            The authorization request
     * @param resource
     *            The resource contained in the request
     * @return The XACML authorization response
     * @throws EbxmlRegistryException
     *             If errors occur retrieving the XACML policy object
     */
    private ResponseType performRequest(RequestType request,
            RegistryObjectType resource) throws EbxmlRegistryException {
        ResponseType response = null;
        XACMLObject policy = null;
        String policyName = resource.getSlotValue(POLICY_SLOT_NAME);
        if (policyName == null) {
            policy = xacmlPolicyAdmin.getPolicyObject(DEFAULT_POLICY);
        } else {
            policy = xacmlPolicyAdmin.getPolicyObject(policyName);
            if (policy == null) {
                statusHandler
                        .warn("Policy ["
                                + policyName
                                + "] does not exist.  Using default access control policy!");
                policy = xacmlPolicyAdmin.getPolicyObject(DEFAULT_POLICY);
            }
        }

        response = pdp.evaluate(policy, request);
        return response;
    }

    /**
     * Extracts the ids of the resources from the message
     * 
     * @param message
     *            The message to get the resource ids from
     * @param isSoapCall
     *            True if this is a SOAP call
     * @return The list of resources ids extracted from the message
     * @throws EbxmlRegistryException
     *             If an invalid message is submitted
     */
    private List<RegistryObjectType> getResources(Message message,
            boolean isSoapCall) throws EbxmlRegistryException {
        List<RegistryObjectType> registryObjects = new ArrayList<RegistryObjectType>();
        List<String> ids = new ArrayList<String>();
        ObjectRefListType refList = null;
        RegistryObjectListType objList = null;

        if (isSoapCall) {
            RegistryRequestType request = null;
            MessageContentsList content = (MessageContentsList) ((SoapMessage) message)
                    .getContent(List.class);

            if (!content.isEmpty()) {
                Object obj = content.get(0);
                if (obj instanceof RegistryRequestType) {
                    request = (RegistryRequestType) obj;
                }
            }
            if (request == null) {
                throw new EbxmlRegistryException(
                        "Could not determine request type!");
            }

            if (request instanceof CatalogObjectsRequest) {
                refList = ((CatalogObjectsRequest) request).getObjectRefList();
                objList = ((CatalogObjectsRequest) request)
                        .getOriginalObjects();
            } else if (request instanceof FilterObjectsRequest) {
                objList = ((FilterObjectsRequest) request).getOriginalObjects();
            } else if (request instanceof QueryRequest) {
                if (message.getExchange().getOutMessage() != null) {
                    QueryResponse queryResponse = (QueryResponse) ((SoapMessage) message
                            .getExchange().getOutMessage()).getContent(
                            List.class).get(0);
                    refList = queryResponse.getObjectRefList();
                    objList = queryResponse.getRegistryObjectList();
                }
            } else if (request instanceof RemoveObjectsRequest) {
                refList = ((RemoveObjectsRequest) request).getObjectRefList();
            } else if (request instanceof SubmitObjectsRequest) {
                objList = ((SubmitObjectsRequest) request)
                        .getRegistryObjectList();
            } else if (request instanceof UpdateObjectsRequest) {
                refList = ((UpdateObjectsRequest) request).getObjectRefList();
            } else if (request instanceof ValidateObjectsRequest) {
                refList = ((ValidateObjectsRequest) request).getObjectRefList();
                objList = ((ValidateObjectsRequest) request)
                        .getOriginalObjects();
            } else {
                throw new IllegalArgumentException("Unsupported request type: "
                        + request.getClass());
            }
            if (refList != null) {
                for (ObjectRefType ref : refList.getObjectRef()) {
                    ids.add(ref.getId());
                }
            }
            if (objList != null) {
                registryObjects.addAll(objList.getRegistryObject());
            }
            if (!ids.isEmpty()) {
                registryObjects.addAll(registryObjectDao.getById(ids));
            }
        }
        return registryObjects;
    }

    /**
     * Handle any Obligations returned by the PDP
     * 
     * @throws XACMLException
     */
    protected void handleObligations(RequestType request, Principal principal,
            Message message, ResultType result) throws XACMLException {
        ObligationsType obligationObject = result.getObligations();
        if (obligationObject != null
                && !obligationObject.getObligations().isEmpty()) {
            statusHandler.info("Evaluating "
                    + obligationObject.getObligations().size()
                    + " obligations!");
            for (ObligationType obligation : result.getObligations()
                    .getObligations()) {
                XACMLObligationEvaluator.getInstance().evaluate(obligation,
                        request);
            }
        }
    }
}