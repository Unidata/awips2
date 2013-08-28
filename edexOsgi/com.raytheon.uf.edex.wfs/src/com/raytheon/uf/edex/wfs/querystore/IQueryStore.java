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
package com.raytheon.uf.edex.wfs.querystore;

import java.util.List;
import java.util.Map;

import net.opengis.wfs.v_2_0_0.QueryType;
import net.opengis.wfs.v_2_0_0.StoredQueryDescriptionType;

import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * interface for parameterized query storage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2012            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      renamed for standards
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IQueryStore {

    /**
     * @param query
     * @throws Exception
     */
    public void store(StoredQueryDescriptionType query) throws OgcException;

    /**
     * @param id
     * @return null if no stored query matches id
     * @throws Exception
     */
    public StoredQueryDescriptionType retrieve(String id) throws OgcException;

    /**
     * Remove stored query. Does nothing if no query matches id
     * 
     * @param id
     * @throws Exception
     */
    public void remove(String id) throws OgcException;

    /**
     * List all IDs of stored queries
     * 
     * @return empty list if no queries are found
     * @throws Exception
     */
    public List<String> list() throws OgcException;

    /**
     * Retrieve and resolve stored query
     * 
     * @param id
     * @param jaxbParameters
     *            map of parameter names to values
     * @return
     * @throws Exception
     */
    public List<QueryType> resolve(String id, Map<String, String> parameters)
            throws OgcException;

    /**
     * @return internal resolver for store
     */
    public StoredQueryResolver getResolver();

}
