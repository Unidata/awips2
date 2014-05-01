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
package com.raytheon.edex.monitors;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//import com.raytheon.edex.db.dao.CoreDao;
//import com.raytheon.edex.db.dao.pool.DaoConfig;
//import com.raytheon.edex.db.dao.pool.DaoPool;
import com.raytheon.edex.services.MonitorSrv;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Performs a database monitoring task on a remote database. This monitor
 * is intended to allow an instance of EDEX to monitor database connections
 * on another server. The number of connections is reported to the system
 * log. The format of the log message is
 * <P>
 *             Database: {connection URL}, connections = {count}
 * <P>
 * This class is intended to be injected into an {@link MonitorSrv} instance by
 * Mule. Because of that, all constructor arguments are of type String. 
 * <P>
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  12May2008    1113       MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class EdexDBRemoteMonitor extends AEdexDBMonitor {
    private static final String DB_DRIVER = "org.postgresql.Driver"; 
    private String connectionURL = "jdbc:postgresql://localhost:5432/metadata";
    private String user = "awips:awips";

    static {
        try {
            Class.forName(DB_DRIVER);
        } catch (ClassNotFoundException e) {
            System.out.println(new EdexException("Unable to load PostgresSQL driver",e));
        }
    }
    /**
     * Constructor. Creates an empty object.
     */
    public EdexDBRemoteMonitor() {
        super();
    }
    /**
     * Constructor. Initialized the SQL query to the argument.
     * 
     * @param data SQL string used to monitor the database.
     */
    public EdexDBRemoteMonitor(String query) {
        super(query);
    }
    /**
     * 
     * @param url
     * @param user
     * @param query
     */
    public EdexDBRemoteMonitor(String url, String user, String query) {
        super();
        this.connectionURL = url;
        this.user = user;
        this.query = query;
    }
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.IEdexMonitor#execute()
     */
    @Override
    public void execute() {
        Connection conn = null;
        try {
            conn = getConnection(connectionURL, user);
            if (conn == null) {
                return;
            }
            List<Object> results = performQuery(conn, query);
            printResult(results);
        } finally {
            closeConnection(conn);
        }
    }
    private void printResult(List<Object> results) {
        if (results.size() != 1) {
            logger.warn("obtained invalid result from status query.");
        }
        logger.info(String.format(REPORT_FORMAT,connectionURL, results.get(0).toString()));
    }
    /**
     * 
     * @param conn
     * @param query
     * @return
     */
    private List<Object> performQuery(Connection conn, String query) {
        List<Object> retVal = new ArrayList<Object>();
        Statement s = null;
        ResultSet r = null;
        try {
            s = conn.createStatement();
            r = s.executeQuery(query);
            while (r.next()) {
                retVal.add(r.getObject(1));
            }
        } catch (Exception e) {
            logger.warn("Encountered problems executing status query",e);
        } finally {
            try {
                if (r != null) {
                    r.close();
                }
                if (s != null) {
                    s.close();
                }
            } catch (SQLException e) {
                logger.warn("Unable to close resultset/statement for status query", e);
            }
        }
        return retVal;
    }
    /**
     * 
     * @param url
     * @param user
     * @return
     */
    private Connection getConnection(String url, String user) {
        Connection conn = null;
        try {
            Pattern p = Pattern.compile("(.+)[:=@](.+)");
            Matcher m = p.matcher(user);
            String name = "";
            String passwd = "";
            if (m.matches()) {
                name = m.group(1);
                passwd = m.group(2);
            }
            conn = DriverManager.getConnection(url, name, passwd);
        } catch (SQLException e) {
            logger.warn("Unable to create connection to database, url=" + url + ", user=" + user, e);
        }
        return conn;
    }
    /**
     * 
     * @param conn
     */
    private void closeConnection(Connection conn) {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                // can't do much, just log and return
                logger.warn("Unable to close DB Connection",e);
            }
        }
    }
}
