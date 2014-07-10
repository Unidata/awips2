package com.raytheon.uf.edex.registry.acp.xacml;

import java.io.ByteArrayOutputStream;
import java.security.Principal;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import org.apache.cxf.message.Message;
import org.apache.cxf.rt.security.xacml.AbstractXACMLAuthorizingInterceptor;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.ctx.ResultType;
import org.opensaml.xacml.policy.ObligationType;
import org.opensaml.xacml.policy.ObligationsType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xml.XMLObject;
import org.opensaml.xml.io.Marshaller;
import org.opensaml.xml.io.MarshallerFactory;
import org.w3c.dom.Element;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.registry.acp.xacml.engine.obligation.XACMLObligationEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class XACMLInterceptor extends AbstractXACMLAuthorizingInterceptor {
    
    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(XACMLInterceptor.class);

    private XACMLPolicyAdministrator xacmlPolicyAdmin;

    private XACMLPolicyDecisionPoint pdp;

    public XACMLInterceptor(XACMLPolicyAdministrator xacmlPolicyAdmin,
            XACMLPolicyDecisionPoint pdp) throws MsgRegistryException {
        this.xacmlPolicyAdmin = xacmlPolicyAdmin;
        this.pdp = pdp;
    }

    @Override
    public ResponseType performRequest(RequestType request, Message message)
            throws Exception {

        if(statusHandler.isPriorityEnabled(Priority.DEBUG)){
            statusHandler.debug(outputRequest(request));
        }
        PolicySetType defaultPolicy = xacmlPolicyAdmin
                .getPolicySet("urn:oasis:names:tc:xacml:2.0:data-delivery:default-policySet");
        ResponseType response = pdp.evaluate(defaultPolicy, request);
        return response;
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

    private String outputRequest(XMLObject xmlObject) throws Exception {
        try {
            System.setProperty("javax.xml.parsers.DocumentBuilderFactory",
                    "org.apache.xerces.jaxp.DocumentBuilderFactoryImpl");

            MarshallerFactory marshallerFactory = org.opensaml.xml.Configuration
                    .getMarshallerFactory();
            Marshaller marshaller = marshallerFactory.getMarshaller(xmlObject);
            Element element = marshaller.marshall(xmlObject);

            ByteArrayOutputStream byteArrayOutputStrm = new ByteArrayOutputStream();
            DOMImplementationRegistry registry = DOMImplementationRegistry
                    .newInstance();
            DOMImplementationLS impl = (DOMImplementationLS) registry
                    .getDOMImplementation("LS");
            LSSerializer writer = impl.createLSSerializer();
            LSOutput output = impl.createLSOutput();
            output.setByteStream(byteArrayOutputStrm);
            writer.write(element, output);
            return byteArrayOutputStrm.toString();
        } catch (Exception e) {
            throw new Exception("Error Serializing the SAML Response", e);
        }
    }

}
