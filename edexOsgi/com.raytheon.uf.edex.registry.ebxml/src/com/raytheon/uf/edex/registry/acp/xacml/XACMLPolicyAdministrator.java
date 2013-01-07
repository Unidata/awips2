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
package com.raytheon.uf.edex.registry.acp.xacml;

import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.PolicyType;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLParser;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.util.RegistrySessionManager;
import com.raytheon.uf.edex.registry.ebxml.util.EDEXRegistryManager;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * The system entity that creates a policy or policy set
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
public class XACMLPolicyAdministrator {

    /** The status handler */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XACMLPolicyAdministrator.class);

    /** The singleton instance */
    private static final XACMLPolicyAdministrator instance = new XACMLPolicyAdministrator();

    /** The map of policies known to the system */
    private Map<String, PolicyType> policyMap = new HashMap<String, PolicyType>();

    /** The map of policy sets known to the system */
    private Map<String, PolicySetType> policySetMap = new HashMap<String, PolicySetType>();

    /**
     * Gets the singleton instance of the XACMLPolicyAdministrator
     * 
     * @return The singleton instance of the XACMLPolicyAdministrator
     */
    public static XACMLPolicyAdministrator getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private XACMLPolicyAdministrator() {
        try {
            RegistrySessionManager.openSession();
            loadAccessControlPolicies();
        } catch (MsgRegistryException e) {
            e.printStackTrace();
        } finally {
            RegistrySessionManager.closeSession();
        }
    }

    /**
     * Gets a policy object based on the id. The returned value may be a Policy
     * Set or a Policy
     * 
     * @param id
     *            The ID of the policy object to retrieve
     * @return The policy or policy set with the given ID
     * @throws EbxmlRegistryException
     */
    public XACMLObject getPolicyObject(String id) throws EbxmlRegistryException {
        XACMLObject policyObject = (XACMLObject) XACMLParser
                .getInstance()
                .unmarshallXacmlObjectFromText(
                        new String(
                                ((ExtrinsicObjectType) new RegistryObjectTypeDao(
                                        ExtrinsicObjectType.class).getByLid(id))
                                        .getRepositoryItem(), Charset
                                        .forName("UTF-8")));
        if ((policyObject instanceof PolicyType)
                || (policyObject instanceof PolicySetType)) {
            return policyObject;
        } else {
            // This case should not happen, but is provided as a redundant check
            throw new EbxmlRegistryException(
                    "Incorrect type returned while getting policy object!");
        }
    }

    /**
     * Gets the policy object for the given policy ID
     * 
     * @param policyId
     *            The policy ID
     * @return The policy with the given policy ID
     * @throws EbxmlRegistryException
     *             If the policy cannot be retrieved from the registry
     */
    public PolicyType getPolicy(String policyId) throws EbxmlRegistryException {
        PolicyType policy = policyMap.get(policyId);
        if (policy == null) {
            ExtrinsicObjectType policyObj = new RegistryObjectTypeDao(
                    ExtrinsicObjectType.class).getByLid(policyId);
            policy = (PolicyType) XACMLParser.getInstance()
                    .unmarshallXacmlObjectFromText(
                            new String(policyObj.getRepositoryItem(), Charset
                                    .forName("UTF-8")));
        }
        return policy;
    }

    /**
     * Gets the policy object for the given policy set ID
     * 
     * @param policySetId
     *            The policy set ID
     * @return The Policy Set with the given policy set ID
     * @throws EbxmlRegistryException
     *             If the policy set cannot be retrieved from the registry
     */
    public PolicySetType getPolicySet(String policySetId)
            throws EbxmlRegistryException {
        PolicySetType policySet = policySetMap.get(policySetId);
        if (policySet == null) {
            ExtrinsicObjectType policyObj = new RegistryObjectTypeDao(
                    ExtrinsicObjectType.class).getByLid(policySetId);
            policySet = (PolicySetType) XACMLParser.getInstance()
                    .unmarshallXacmlObjectFromText(
                            new String(policyObj.getRepositoryItem(), Charset
                                    .forName("UTF-8")));
        }
        return policySet;
    }

    /**
     * Loads the access control policies from the file system folder
     * 
     * @throws MsgRegistryException
     *             If errors occur while reading and storing the policies and
     *             policy sets
     */
    public void loadAccessControlPolicies() throws MsgRegistryException {
        @SuppressWarnings("rawtypes")
        LifecycleManager lcm = ((EDEXRegistryManager) EDEXUtil
                .getESBComponent("edexRegistryManager")).getLifeCycleManager();
        LocalizationFile[] files = PathManagerFactory.getPathManager()
                .listStaticFiles("ebxml/acp", new String[] { ".xml" }, true,
                        true);
        File[] fileList = new File[files.length];
        for (int i = 0; i < fileList.length; i++) {
            fileList[i] = files[i].getFile();
        }

        SubmitObjectsRequest submitRequest = new SubmitObjectsRequest();
        submitRequest.setComment("Submission of access control policy");
        submitRequest.setCheckReferences(false);
        submitRequest.setMode(Mode.CREATE_OR_REPLACE);
        submitRequest.setId("ACP submission");
        List<RegistryObjectType> regObjs = new ArrayList<RegistryObjectType>();
        for (int i = 0; i < fileList.length; i++) {
            statusHandler.info("Reading access control policy from file: "
                    + fileList[i].getName());

            try {
                Object policyObject = XACMLParser.getInstance()
                        .unmarshalXacmlObject(fileList[i]);
                String lid = null;
                String description = null;
                String objectType = null;
                String name = "Access Control Policy";
                if (policyObject instanceof PolicySetType) {
                    PolicySetType policySet = (PolicySetType) policyObject;
                    lid = policySet.getPolicySetId();
                    description = policySet.getDescription().getValue();
                    objectType = RegistryObjectTypes.POLICY_SET;
                    name += " Set";
                    policySetMap.put(policySet.getPolicySetId(), policySet);
                } else if (policyObject instanceof PolicyType) {
                    PolicyType policy = (PolicyType) policyObject;
                    lid = policy.getPolicyId();
                    description = policy.getDescription().getValue();
                    objectType = RegistryObjectTypes.POLICY;
                    policyMap.put(policy.getPolicyId(), policy);
                } else {
                    statusHandler
                            .error("File unrelated to access control found in acp directory. Ignoring...");
                    continue;
                }

                byte[] serializedPolicy = XACMLParser.getInstance()
                        .readFileContents(fileList[i])
                        .getBytes(Charset.forName("UTF-8"));
                ExtrinsicObjectType regObj = new ExtrinsicObjectType();
                regObj.setId(lid);
                regObj.setLid(lid);
                regObj.setDescription(RegistryUtil
                        .getInternationalString(description));
                regObj.setName(RegistryUtil.getInternationalString(name));
                regObj.setObjectType(objectType);
                regObj.setStatus(StatusTypes.APPROVED);
                regObj.setOwner(RegistryUtil.DEFAULT_OWNER);
                regObj.setRepositoryItem(serializedPolicy);
                regObj.setMimeType("text/xml; charset=UTF-8");
                regObj.setVersionInfo(EbxmlObjectUtil.newVersionObject());
                regObjs.add(regObj);
            } catch (Exception e) {
                statusHandler.fatal(
                        "Access Control Policies not properly initialized!", e);
            }
        }
        submitRequest.setRegistryObjectList(EbxmlObjectUtil
                .createRegistryObjectList(regObjs));
        lcm.submitObjects(submitRequest);
    }
}
