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
package com.raytheon.uf.common.registry.constants;

/**
 * 
 * Registry canonical query types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class CanonicalQueryTypes {

    public static final String GET_OBJECT_BY_ID = "urn:oasis:names:tc:ebxml-regrep:query:GetObjectById";

    public static final String GET_OBJECTS_BY_LID = "urn:oasis:names:tc:ebxml-regrep:query:GetObjectsByLid";

    public static final String GET_AUDIT_TRAIL_BY_LID = "urn:oasis:names:tc:ebxml-regrep:query:GetAuditTrailByLid";

    public static final String GET_AUDIT_TRAIL_BY_TIME_INTERVAL = "urn:oasis:names:tc:ebxml-regrep:query:GetAuditTrailByTimeInterval";

    public static final String GET_AUDIT_TRAIL_BY_ID = "urn:oasis:names:tc:ebxml-regrep:query:GetAuditTrailById";

    public static final String GET_CLASSIFICATION_SCHEMES_BY_ID = "urn:oasis:names:tc:ebxml-regrep:query:GetClassificationSchemesById";

    public static final String EXPORT_OBJECT = "urn:oasis:names:tc:ebxml-regrep:query:ExportObject";

    public static final String GET_CHILDREN_BY_PARENT_ID = "urn:oasis:names:tc:ebxml-regrep:query:GetChildrenByParentId";

    public static final String GET_REGISTRY_PACKAGES_BY_MEMBER_ID = "urn:oasis:names:tc:ebxml-regrep:query:GetRegistryPackagesByMemberId";

    public static final String FIND_ALL_MY_OBJECTS = "urn:oasis:names:tc:ebxml-regrep:query:FindAllMyObjects";

    public static final String GARBAGE_COLLECTOR = "urn:oasis:names:tc:ebxml-regrep:query:GarbageCollector";

    public static final String BASIC_QUERY = "urn:oasis:names:tc:ebxml-regrep:query:BasicQuery";

    public static final String EXTRINSIC_OBJECT_QUERY = "urn:oasis:names:tc:ebxml-regrep:query:ExtrinsicObjectQuery";

    public static final String FIND_ASSOCIATIONS = "urn:oasis:names:tc:ebxml-regrep:query:FindAssociations";

    public static final String FIND_ASSOCIATED_OBJECTS = "urn:oasis:names:tc:ebxml-regrep:query:FindAssociatedObjects";

    public static final String ADHOC_QUERY = "urn:oasis:names:tc:ebxml-regrep:query:AdhocQuery";

    public static final String KEYWORD_SEARCH = "urn:oasis:names:tc:ebxml-regrep:query:KeywordSearch";

    public static final String REGISTRY_PACKAGE_SELECTOR = "urn:oasis:names:tc:ebxml-regrep:query:RegistryPackageSelector";

    public static final String CLASSIFICATION_SCHEME_SELECTOR = "urn:oasis:names:tc:ebxml-regrep:query:ClassificationSchemeSelector";

    public static final String GET_NOTIFICATION = "urn:oasis:names:tc:ebxml-regrep:query:GetNotification";

}
