/*****************************************************************************************
 * COPYRIGHT (c), 2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.common.dbQuery;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;

/**
 * Executes an arbitrary hql or sql query. Also contains functionality to insert
 * or update an object in a database
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * 10/15/2008   1615       bphillip     Initial Creation
 * 12/11/2008   1777       bphillip     Added insert/update functionality
 * 04/01/2010              m.laryukhin  Adapted to perform queries from command line standalone programs
 * 
 * </pre>
 * 
 * @author bphillip, mlaryukhin
 * @version 1.0
 */
public class NcDirectDbQuery {

	/** The constraints for the script creator */
	private Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();

	/** The sql Query */
	private String query;

	/** The database name */
	private String database;

	/** The language the query is written in */
	private QueryLanguage queryLanguage;

	private static String httpServer;

	/**
	 * Executes a database query redirecting the call either to
	 * com.raytheon.uf.viz.core.catalog.DirectDbQuery or to local
	 * gov.noaa.nws.ncep.ui.pgen.db.Connector depending on the caller. If the
	 * method is called from a standalone non-eclipse plugin-based program, then
	 * some classes cannot be loaded and local version of Connector is needed.
	 * 
	 * @param query
	 *            The query
	 * @param database
	 *            The database name
	 * @param language
	 *            The query language
	 * @return The results
	 * @throws VizException
	 *             If the query fails
	 */
	public static List<Object[]> executeQuery(String query, String database, QueryLanguage language)
			throws VizException {
		try {
			// check whether the class VizApp can be loaded
			String server = VizApp.getHttpServer();
			if (server == null)
				throw new ExceptionInInitializerError("Server is not specified");
			
			return DirectDbQuery.executeQuery(query, database, language);

		} catch (ExceptionInInitializerError e) {
			return call(query, database, language);
		} catch (NoClassDefFoundError e) {
			return call(query, database, language);
		}
	}

	private static List<Object[]> call(String query, String database,
			QueryLanguage language) throws VizException {
		// either VizApp is not loaded (standalone app, command line) 
		// or "server" is null (standalone app started as eclipse-plugin) 
		if (NcDirectDbQuery.getHttpServer() == null || NcDirectDbQuery.getHttpServer().isEmpty())
			throw new VizException("No server specified in NcDirectDbQuery");
		return new NcDirectDbQuery(query, database, language).performQuery();
	}

	/**
	 * Constructs a new DirectDbQuery
	 * 
	 * @param query
	 *            The query
	 * @param database
	 *            The database
	 * @param language
	 *            The query language
	 */
	private NcDirectDbQuery(String query, String database, QueryLanguage language) {
		constraints.put("pluginName", new RequestConstraint("satellite"));
		this.query = query;
		this.database = database;
		queryLanguage = language;
		constraints.put("query", new RequestConstraint(query));
		constraints.put("database", new RequestConstraint(database));
	}
	
    /**
     * Executes a database query. The results are returned in a QueryResult
     * object
     * 
     * @param query
     *            The query
     * @param database
     *            The database name
     * @param language
     *            The query language
     * @return The results
     * @throws VizException
     *             If the query fails
     */
    public static QueryResult executeMappedQuery(String query, String database,
            QueryLanguage language) throws VizException {

		try {
			// check whether the class VizApp can be loaded
			String server = VizApp.getHttpServer();
			if (server == null)
				throw new ExceptionInInitializerError("Server is not specified");

			return DirectDbQuery.executeMappedQuery(query, database, language);

		} catch (ExceptionInInitializerError e) {
			if (NcDirectDbQuery.getHttpServer() == null || NcDirectDbQuery.getHttpServer().isEmpty())
				throw new VizException("No server specified in NcDirectDbQuery");
			return new NcDirectDbQuery(query, database, language).performMappedQuery();
		} catch (NoClassDefFoundError e) {
			if (NcDirectDbQuery.getHttpServer() == null || NcDirectDbQuery.getHttpServer().isEmpty())
				throw new VizException("No server specified in NcDirectDbQuery");
			return new NcDirectDbQuery(query, database, language).performMappedQuery();
		}
    }

    /**
     * Performs the mapped query
     * 
     * @return The results
     * @throws VizException
     *             If the query fails
     */
    private QueryResult performMappedQuery() throws VizException {
		if (database == null) {
			throw new VizException("Database not specified for query");
		}
		if (query == null || query.isEmpty()) {
			throw new VizException("Cannot execute null or empty query");
		}

		if (queryLanguage == null || queryLanguage != QueryLanguage.SQL) {
			throw new VizException("Only SQL query language is supported in DirectDbQueryPgen");
		}

		String queryText = "import SqlQuery \n" + "request = SqlQuery.SqlQuery(\"" + query
				+ "\",\"" + database + "\") \n" + "return request.execute()";

		Message message = NcConnector.getInstance()
				.connectMessage(queryText, null, 60000, httpServer);

		AbstractResponseMessage[] absresponses = message.getBody().getResponses();
		QueryResult queryResult = null;
		AbstractResponseMessage response = absresponses[0];

		if (response instanceof ResponseMessageGeneric) {
			queryResult = (QueryResult) ((ResponseMessageGeneric) response).getContents();

		} else if (response instanceof ResponseMessageError) {
			ResponseMessageError rme = (ResponseMessageError) response;
			VizServerSideException innerException = new VizServerSideException(rme.toString());
			throw new VizServerSideException(rme.getErrorMsg(), innerException);
		}
		return queryResult;
	}
	
    /**
     * Performs the query
     * 
     * @return The results
     * @throws VizException
     *             query error
     */
    private List<Object[]> performQuery() throws VizException {
        QueryResult result = performMappedQuery();
        List<Object[]> unmappedResults = new ArrayList<Object[]>();

        for (QueryResultRow row : result.getRows()) {
            unmappedResults.add(row.getColumnValues());
        }
        return unmappedResults;
    }

	/**
	 * Getter for the http server.
	 * 
	 * @return
	 */
	public static String getHttpServer() {
		return NcDirectDbQuery.httpServer;
	}

	/**
	 * Setter for http server.
	 * 
	 * @param httpServer
	 */
	public static void setHttpServer(String httpServer) {
		NcDirectDbQuery.httpServer = httpServer;
	}
}
