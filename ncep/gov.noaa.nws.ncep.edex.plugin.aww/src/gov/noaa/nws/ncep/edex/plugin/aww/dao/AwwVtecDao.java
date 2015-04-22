package gov.noaa.nws.ncep.edex.plugin.aww.dao;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import org.hibernate.Session;
import org.springframework.orm.hibernate4.SessionFactoryUtils;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

public class AwwVtecDao extends CoreDao {

    public AwwVtecDao() {
        this(DaoConfig.forClass(AwwVtec.class));
    }

    /**
     * @param config
     */
    public AwwVtecDao(DaoConfig config) {
        super(config);
    }


    /**
     * Returns QueryResult object containing AwwVtec iformation in the database. The
     * detail information depends on individual SQL query
     * 
     * @return the list of subscriptions
     */
    public synchronized QueryResult getQueryResultByNativeSQLQuery(String nativeSQLQuery) {
    	QueryResult queryResult = null; 
    	try {
    		queryResult = executeSelectNativeSqlQuery(nativeSQLQuery); 
    	} catch(ClassCastException cce) {
    		//do nothing now, if a logger is configured, we will add log message here
    		//If the ClassCastException is thrown, it means there are no results returned
    	} catch(DataAccessLayerException dalc) {
    		//do nothing now, if a logger is configured, we will add log message here
    	}
        return queryResult;
    }

    /**
     * Executes a native SQL statement. This method completely bypasses
     * Hibernate and uses JDBC directly
     * 
     * @param sql
     *            The sql string
     * @return A QueryResultObject 
     * @throws DataAccessLayerException
     *             If the statement fails
     */
    public QueryResult executeSelectNativeSqlQuery(String selectSQLQuery)
            throws DataAccessLayerException {
        Session session = null;
        Connection conn = null;
        Statement stmt = null;
        SQLException exception = null;
        QueryResult results = null;

        try {
            session = getSession(true);
            conn = SessionFactoryUtils.getDataSource(getSessionFactory()).getConnection();
            stmt = conn.createStatement();

        } catch (SQLException e) {
            throw new DataAccessLayerException(
                    "Method: executeSelectNativeSqlQuery in AwwVtecDao class, Unable to create JDBC statement", e);
        }
        try {
            ResultSet resultSet = stmt.executeQuery(selectSQLQuery); 
            results = mapResultSet(resultSet);

        } catch (SQLException e1) {
            exception = e1;
            logger.error("Error executing script="+selectSQLQuery+",  ", e1);
        }

        try {
            stmt.close();
        } catch (SQLException e1) {
            exception = e1;
            logger.error("Method: executeSelectNativeSqlQuery in AwwVtecDao class, Unable to close JDBC statement!", e1);
        }

        try {
            if (!conn.isClosed()) {
                conn.close();
            }
        } catch (SQLException e) {
            exception = e;
            logger.error("Method: executeSelectNativeSqlQuery in AwwVtecDao class, Cannot close database connection!!", e);
        }
        if (session.isOpen()) {
            session.close();
        }
        if (exception != null) {
            throw new DataAccessLayerException(
                    "Method: executeSelectNativeSqlQuery in AwwVtecDao class, SQLException is wrapped and rethrown in DataAccessLayerException", exception);
        }
        return results;
    }

    /**
     * Helper method for mapping JDBC result sets
     * this method is private in coreDao, it is copied here
     * 
     * @param rs
     *            The raw ResultSet object
     * @return The remapped results
     * @throws SQLException
     *             If mapping fails
     */
    private QueryResult mapResultSet(ResultSet rs) throws SQLException {
        QueryResult results = new QueryResult();

        ResultSetMetaData metadata = rs.getMetaData();

        int columnCount = metadata.getColumnCount();
        for (int i = 0; i < columnCount; i++) {
            results.addColumnName(metadata.getColumnLabel(i + 1), i);
        }

        List<QueryResultRow> rows = new ArrayList<QueryResultRow>();
        while (rs.next()) {
            Object[] columnValues = new Object[columnCount];
            for (int i = 1; i <= columnCount; i++) {
                columnValues[i - 1] = rs.getObject(i);

            }
            rows.add(new QueryResultRow(columnValues));
        }
        results.setRows(rows.toArray(new QueryResultRow[] {}));
        return results;
    }


}
