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
package com.raytheon.uf.edex.ebxml.query;

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2011            bphillip     Initial creation
 *
 * </pre>
 *
 * @author bphillip
 * @version 1.0	
 */

public final class QueryParameters {

    public static final List<String> QUERY_PARAM_NAMES;
    
    /** "name" query parameter key. */
    public static final String NAME_QUERY_PARAM = "name";
    /** "description" query parameter key. */
    public static final String DESC_QUERY_PARAM = "description";
    /** "objectType" query parameter key. */
    public static final String OBJ_TYPE_QUERY_PARAM = "objectType";
    /** "status" query parameter key. */
    public static final String STATUS_QUERY_PARAM = "status";
    /** "versionName" query parameter key. */
    public static final String VERSION_NAME_QUERY_PARAM = "versionName";
    /** "userVersionName" query parameter key. */
    public static final String USER_VERSION_NAME_QUERY_PARAM = "userVersionName";
    
    /** "classificationNode" query parameter key. */
    public static final String CLASS_NODE_QUERY_PARAM = "classificationNode";
    /** "classifiedObject" query parameter key. */
    public static final String CLASS_OBJECT_QUERY_PARAM = "classifiedObject";
    /** "classificationScheme" query parameter key. */
    public static final String CLASS_SCHEME_QUERY_PARAM = "classificationScheme";
    /** "nodeRepresentation" query parameter key. */
    public static final String CLASS_NODE_REP_QUERY_PARAM = "nodeRepresentation";
    
    /** "lid" query parameter key. */
    public static final String LID_QUERY_PARAM = "lid";
    
    /** "owner" query parameter key. */
    public static final String OWNER_QUERY_PARAM = "owner";
    
    
    static{
        QUERY_PARAM_NAMES = new ArrayList<String>();
        QUERY_PARAM_NAMES.add(NAME_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(DESC_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(VERSION_NAME_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(USER_VERSION_NAME_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(CLASS_NODE_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(CLASS_OBJECT_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(CLASS_SCHEME_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(CLASS_NODE_REP_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(LID_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(OBJ_TYPE_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(OWNER_QUERY_PARAM);
        QUERY_PARAM_NAMES.add(STATUS_QUERY_PARAM);
    }
    
    
    /** "matchOnAnyParameter" query parameter key. */
    public static final String MATCH_ANY_QUERY_PARAM = "matchOnAnyParameter";
    /** "queryExpression" query parameter key. */
    public static final String QUERY_EXPR_PARAM = "queryExpression";
    /** "queryLanguage" query parameter key. */
    public static final String QUERY_LANG_PARAM = "queryLanguage";
      
}
