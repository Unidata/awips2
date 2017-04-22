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
package com.raytheon.edex.uengine.tasks.query;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.hibernate.StatelessSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.message.CatalogAttribute;
import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Retrieves the MAX(refTime) for each datauri in a comma delimited list.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/08/2008              randerso    Initial creation
 * 08/20/2009   2586       rjpeter     Optimized query and ensured single bad uri will not break entire list.
 * 12/10/2015   5166       kbisanz     Update logging to use SLF4J
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class LatestTimeQuery extends ScriptTask {
    private static final String QUERY_PRE_TABLE = "SELECT MAX(REFTIME) FROM AWIPS.";

    private static final String QUERY_POST_TABLE = " WHERE DATAURI LIKE ?";

    private static final Pattern urlDelimiter = Pattern.compile(",");

    private static final Pattern tableDelimiter = Pattern.compile("/");

    private Logger logger = LoggerFactory.getLogger(getClass());

    private String uriList;

    /**
     * @param uriList
     *            comma separated list of data URIs
     */
    public LatestTimeQuery(String uriList) {
        this.uriList = uriList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        String[] uris = urlDelimiter.split(uriList);
        Map<String, List<String>> tableMap = new HashMap<String, List<String>>(
                75);

        for (String uri : uris) {
            String[] tokens = tableDelimiter.split(uri, 3);
            String table = tokens[(tokens.length > 1 ? 1 : 0)];
            List<String> datauris = tableMap.get(table);

            if (datauris == null) {
                datauris = new ArrayList<String>(500);
                tableMap.put(table, datauris);
            }
            datauris.add(uri);
        }

        ResponseMessageCatalog rmc = new ResponseMessageCatalog();
        StatelessSession session = null;
        Connection conn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        List<CatalogItem> catalogItems = new ArrayList<CatalogItem>(uris.length);
        StringBuilder sqlQuery = new StringBuilder(75);

        try {
            session = openSession();
            conn = session.connection();
            for (Entry<String, List<String>> entry : tableMap.entrySet()) {
                String table = entry.getKey();
                try {
                    sqlQuery.setLength(0);
                    sqlQuery.append(QUERY_PRE_TABLE);
                    sqlQuery.append(table);
                    sqlQuery.append(QUERY_POST_TABLE);
                    ps = conn.prepareStatement(sqlQuery.toString());

                    for (String datauri : entry.getValue()) {
                        try {
                            ps.setString(1, datauri);
                            rs = ps.executeQuery();

                            if (rs.next()) {
                                Object obj = rs.getObject(1);

                                if (obj != null) {
                                    String time = String.valueOf(obj);
                                    CatalogItem ci = new CatalogItem();
                                    ci.setKey(datauri);
                                    CatalogAttribute attribute = new CatalogAttribute(
                                            "dataTime", time);
                                    ci.setAttributes(new CatalogAttribute[] { attribute });
                                    catalogItems.add(ci);
                                }
                            }
                        } catch (SQLException e) {
                            // have to restart transaction
                            logger.error("dataTime retrieval failed for uri ["
                                    + datauri + "]", e);

                            // due to hibernate voiding current transaction on
                            // error
                            conn.clearWarnings();
                            conn.commit();
                        }
                    }
                    conn.commit();
                } catch (SQLException e) {
                    // have to restart transaction
                    StringBuilder errorMsg = new StringBuilder(
                            "dataTime retrieval failed for table [");
                    errorMsg.append(table);
                    errorMsg.append("] datauris [");
                    for (String datauri : entry.getValue()) {
                        errorMsg.append(datauri);
                        errorMsg.append(", ");
                    }
                    errorMsg.delete(errorMsg.length() - 2, errorMsg.length());
                    logger.error(errorMsg.toString(), e);

                    // due to hibernate voiding current transaction on error
                    conn.clearWarnings();
                    conn.commit();
                } finally {
                    closePreparedStatement(ps);
                }
            }
        } catch (Exception e) {
            // have to restart transaction
            logger.error(
                    "Error occurred establishing connection to retrieve data times",
                    e);
        } finally {
            closeConnection(conn);
            closeSession(session);
        }

        CatalogItem[] items = catalogItems.toArray(new CatalogItem[0]);
        rmc.setItems(items);
        rmc.setDataURI("");
        rmc.setFileType("");
        rmc.setValues(new String[0]);
        rmc.setValidTime(new Date());
        return rmc;
    }

    private StatelessSession openSession() {
        CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
        return dao.getSessionFactory().openStatelessSession();
    }

    private void closeSession(StatelessSession s) {
        if (s != null) {
            s.close();
        }
    }

    private Connection openConnection(StatelessSession s) throws SQLException {
        Connection rval = null;

        if (s != null) {
            rval = s.connection();
        }

        return rval;
    }

    private void closeConnection(Connection c) {
        if (c != null) {
            try {
                c.close();
            } catch (SQLException e) {
                // ignore
            }
        }
    }

    private void closePreparedStatement(PreparedStatement ps) {
        if (ps != null) {
            try {
                ps.close();
            } catch (SQLException e) {
                // ignore
            }
        }
    }
}
