/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@oce.orst.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//
/////////////////////////////////////////////////////////////////////////////



/* $Id: dodsSQLServlet.java,v 1.11.2.6 2004/08/26 21:47:49 ndp Exp $ */

package dods.servers.sql;

import java.io.*;
//import java.lang.reflect.*;
import java.sql.*;
//import java.text.*;
//import java.util.*;
import java.util.zip.DeflaterOutputStream;
import javax.servlet.*;
import javax.servlet.http.*;

import dods.util.*;
import dods.dap.*;
//import dods.dap.Server.ServerDDS;
import dods.dap.Server.FunctionLibrary;
import dods.dap.parser.ParseException;
import dods.servlet.DODSServlet;
import dods.servlet.ReqState;
import dods.servlet.GuardedDataset;

/***************************************************************************
 * This extension of DODSServlet adds JDBC connection functionality to the
 * servlet design.
 * Because of the unusual nature of DBMS systems with respect to other types
 * of data archives, the usual procedures for retrieving data in a DODS server
 * needed to be changed. This class, <code>dodsSQLServlet</code>, encapsulates
 * the logic for interacting with a DBMS. This includes getting the client's
 * DODS request, changing it to an SQL query, querying the DBMS (using JDBC)
 * and then collecting the reply and sending the returned data to the client.
 * <p>
 * The information for making the JDBC connection should be located in the
 * <code>iniFile</code> object that is inflated by the <code>doGet()</code>
 * method of the parent class <code>DODSServlet</code>. The section in the
 * iniFile should look like:
 * <pre>
 * [JDBC]
 * Driver            =    the.name.of.the.jdbc.driver
 * ConnectionURL     =    jdbc:somevendor://targetmachine:port
 * username          =    guest
 * password          =
 * MaxResponseLength =    300
 * </pre>
 *
 * @see #doGet(HttpServletRequest, HttpServletResponse) doGet()
 * @author Nathan David Potter
 */

public abstract class dodsSQLServlet extends DODSServlet {

    private static final boolean _Debug = false;

    private Connection currentConnection = null;
    private Statement  currentStatement  = null;

    /***************************************************************************
     * This function must be implemented locally for each DODS server. It should
     * do the following:
     *	<ul>
     *	<li> Make a new ServerFactory (aka BaseTypeFactory) for the dataset requested.
     *	<li> Instantiate a sqlDDS using the ServerFactory and populate it (this
     *		 could be accomplished by just opening a (cached?) DDS in a file and parsing it)
     *	<li> Return this freshly minted ServerDDS object (to the servlet code where it is used.)
     *	</ul>
     *
     * @param rs The ReqState object containing the particulars of this client request..
     *
     * @see dods.dap.Server.ServerDDS
     * @see dods.servers.sql.sqlServerFactory sqlServerFactory
     * @see dods.servers.test.test_ServerFactory test_ServerFactory
     */
    protected abstract GuardedSQLDataset getSQLDataset(ReqState rs)
            throws DODSException, IOException, ParseException;


    /***************************************************************************
     * Supress this method (just pass through it) we need a special DDS here,
     * and thus this method gets replaced by getSQLDDS().
     *
     * @param rs The ReqState object containing the particulars of this client request..
     *
     * @see dods.dap.Server.ServerDDS
     * @see dods.servers.sql.sqlServerFactory sqlServerFactory
     * @see dods.servers.test.test_ServerFactory test_ServerFactory
     */
    protected GuardedDataset getDataset(ReqState rs) throws DODSException, IOException, ParseException {
        return (getSQLDataset(rs));
    }


    /***************************************************************************
     * This function must be implemented locally for each DODS server. It should
     * return a String cointaining the DODS Server Version...
     */
    public abstract String getServerVersion();


    /***************************************************************************
     * Intitializes the servlet. Init (at this time) basically sets up
     * the object dods.util.Debug from the debuggery flags in the
     * servlet InitParameters. The Debug object can be referenced (with
     * impunity) from any of the dods code...
     *
     */
    public void init() throws ServletException {


        super.init();


        // Set some default drivers
        String jdbcDriver = "openlink.jdbc2.Driver";

        String tmp = getInitParameter("JDBCdriver");
        if (tmp != null)
            jdbcDriver = tmp;


        try {
            // Load the JDBC Driver
            Class.forName(jdbcDriver);

        } catch (ClassNotFoundException e) {
            throw new ServletException("\n Cannot Load JDBC Driver Class: " + e.getMessage() +
                    "\n Is the driver name spelled correctly?" +
                    "\n Is the .class file or the jar file" +
                    "\n containing the driver on the CLASSPATH ??\n\n");
        }


        // Keep us updated
        System.out.println("Using JDBC Driver:    " + jdbcDriver);


    }


    /***************************************************************************
     * Handler for the client's data request. Requires the getSQLDDS()
     * method implemented by each server localization effort.
     *
     * <p>Once the DDS has been parsed, the projection is determine by proccesing
     * the constraint expression. The SQLDDS is then asked to convert it's
     * projection and selection information into an SQL query. This query is then
     * submitted to the DBMS. The result set is then read into the SQLDDS. In the
     * the process, the data is returned to the client. Neat, eh?
     *
     * @param request The client's <code> HttpServletRequest</code> request
     * object.
     * @param response The server's <code> HttpServletResponse</code> response
     * object.
     * @param rs The ReqState object containing the particulars of this client request..
     * This is used (if it's not just empty) subset the data in the dataset.
     */
    public void doGetDODS(HttpServletRequest request,
                          HttpServletResponse response,
                          ReqState rs)
            throws IOException, ServletException {


        if (Debug.isSet("showResponse"))
            System.out.println("DRDS   " +
                    "Sending DODS Data For: " + rs.getDataSet() +
                    "    CE: '" + rs.getConstraintExpression() + "'");

        response.setContentType("application/octet-stream");
        response.setHeader("XDODS-Server", getServerVersion());
        response.setHeader("Content-Description", "dods_data");


        ServletOutputStream sOut = response.getOutputStream();
        OutputStream bOut = null, eOut= null;


        if (rs.getAcceptsCompressed()) {
            response.setHeader("Content-Encoding", "deflate");
            bOut = new DeflaterOutputStream(sOut);
        } else {
            // Commented out because of a bug in the DODS C++ stuff...
            //response.setHeader("Content-Encoding", "plain");
            bOut = new BufferedOutputStream(sOut);
        }


        GuardedSQLDataset sqlDS = null;
        try {


            sqlDS = getSQLDataset(rs);


            // Utilize the getSQLDDS() method to get a parsed and populated sqlDDS
            // for this request.
            sqlDDS myDDS = sqlDS.getSQLDDS();

            // Utilize the getDAS() method to get a parsed and populated DAS
            // for this request.
            DAS myDAS = sqlDS.getDAS();


            if (_Debug) {
                System.out.println("Constrained DDS before constraint parsing:");
                myDDS.printConstrained(System.out);
            }

            // Instantiate the ClauseFactory using the correct FunctionLibrary
            FunctionLibrary flib = new FunctionLibrary();
            flib.setPrefix("dods.servers.sql.SSF");
            SqlClauseFac scf = new SqlClauseFac(flib);


            // Instantiate the CEEvaluator using thew DDS and the
            // ClauseFactory, then parse the constraint expression
            sqlCEEval ce = new sqlCEEval(myDDS, scf);
            ce.parseConstraint(rs.getConstraintExpression());

            if (_Debug) {
                System.out.println("Constrained DDS after constraint parsing:");
                myDDS.printConstrained(System.out);
            }


            // The UseDatasetName InitParameter tells getSQLQuery() to use the dataset name as a
            // prefix for all of the variables and tables requested from the DB.

            boolean useDSName = false;
            String tmp = getInitParameter("UseDatasetName");
            if (tmp != null) {
                if (tmp.equals("") || tmp.equalsIgnoreCase("true"))
                    useDSName = true;
            }
            System.out.println("UseDataSetName is: " + useDSName);

            String query = ce.getSQLQuery(myDAS, useDSName);

            if (Debug.isSet("showRequest")) System.out.println("Query String: \"" + query + "\"");

            connect2DB();

            ResultSet resSet = currentStatement.executeQuery(query);

            if (_Debug) System.out.println("Got the ResultSet.");

            if (resSet.next()/*  && rs.first() */) {


                //processResult(rs,System.out);


                //rs.first();
                //if(rs.isFirst())
                //    System.out.println("Currently at First row...");
                //else
                //    System.out.println("Not At First Row");


                // JDBCMaxResponseLength InitParameter is used to limit the number of rows returned
                // to the client from the DODS server.
                int maxRows = 10;
                tmp = rs.getInitParameter("JDBCMaxResponseLength");
                if (tmp != null)
                    maxRows = Integer.decode(tmp).intValue();

                sqlResponse res = new sqlResponse(resSet, maxRows);

                if (_Debug) System.out.println("Attempting to send data...");

                // Send the constrained DDS back to the client
                PrintWriter pw = new PrintWriter(new OutputStreamWriter(bOut));
                myDDS.printConstrained(pw);

                if (Debug.isSet("showResponse")) {
                    System.out.println("SQL query response recieved.\nSending Data...");
                }

                // Send the Data delimiter back to the client
                //pw.println("Data:"); // JCARON CHANGED
                pw.flush();
                bOut.write("\nData:\n".getBytes()); // JCARON CHANGED
                bOut.flush();

                // Send the binary data back to the client
                DataOutputStream sink = new DataOutputStream(bOut);
                ce.send(myDDS.getName(), sink, res);
                sink.flush();

                // Finish up sending the compressed stuff, but don't
                // close the stream (who knows what the Servlet may expect!)
                if (rs.getAcceptsCompressed())
                    ((DeflaterOutputStream) bOut).finish();

                if (Debug.isSet("showResponse")) {
                    System.out.println("Done!");
                }


            } else {

                eOut = new BufferedOutputStream(sOut);

                response.setHeader("Content-Description", "dods_error");

                // This should probably be set to "plain" but this works, the
                // C++ slients don't barf as they would if I sent "plain" AND
                // the C++ don't expect compressed data if I do this...
                response.setHeader("Content-Encoding", "");

                DODSException de = new DODSException("Your Query Produced No Matching Results.");

                de.print(eOut);
                de.print(System.out);
            }


        } catch (DODSException de) {


            eOut = new BufferedOutputStream(sOut);

            response.setHeader("Content-Description", "dods_error");

            // This should probably be set to "plain" but this works, the
            // C++ slients don't barf as they would if I sent "plain" AND
            // the C++ don't expect compressed data if I do this...
            response.setHeader("Content-Encoding", "");

            de.print(eOut);
            de.print(System.out);


        } catch (SQLException sqle) {


            eOut = new BufferedOutputStream(sOut);

            response.setHeader("Content-Description", "dods_error");

            // This should probably be set to "plain" but this works, the
            // C++ slients don't barf as they would if I sent "plain" AND
            // the C++ don't expect compressed data if I do this...
            response.setHeader("Content-Encoding", "");

            DODSException de = new DODSException(sqle.toString());

            de.print(eOut);
            de.print(System.out);


        } catch (ParseException pe) {



            eOut = new BufferedOutputStream(sOut);

            response.setHeader("Content-Description", "dods_error");

            // This should probably be set to "plain" but this works, the
            // C++ clients don't barf as they would if I sent "plain" AND
            // the C++ don't expect compressed data if I do this...
            response.setHeader("Content-Encoding", "");

            // Strip any double quotes out of the parser error message.
            // These get stuck in auto-magically by the javacc generated parser
            // code and they break our error parser (bummer!)
            String msg = pe.getMessage().replace('\"', '\'');

            DODSException de2 = new DODSException(DODSException.CANNOT_READ_FILE, msg);


            de2.print(eOut);
            de2.print(System.out);
        }
        finally {
            try { // Make sure to release DB connection!
                currentStatement.close();
                currentConnection.close();
                if (Debug.isSet("JDBC")) System.out.println("Closed Database connection.");
            } catch (SQLException e) {
                DODSException de = new DODSException(e.toString());
                de.print(eOut);
                de.print(System.out);
            }
        }


        response.setStatus(HttpServletResponse.SC_OK);


    }
    /***************************************************************************/


    //#*******************************************************************************
    private void connect2DB() throws SQLException {



        // Set some default drivers
        String jdbcDriver = "";
        String connectionURL = "jdbc:openlink://cupcake.oce.orst.edu/DSN=oplEOSDB";
        String uname = "";
        String password = "";
        String tmp = null;


        tmp = getInitParameter("JDBCdriver");
        if (tmp != null)
            jdbcDriver = tmp;

        tmp = getInitParameter("JDBCconnectionURL");
        if (tmp != null)
            connectionURL = tmp;

        tmp = getInitParameter("JDBCusername");
        if (tmp != null)
            uname = tmp;

        tmp = getInitParameter("JDBCpassword");
        if (tmp != null)
            password = tmp;


        // Keep us updated
        if (Debug.isSet("JDBC")) System.out.println("Using JDBC Driver:    " + jdbcDriver);
        if (Debug.isSet("JDBC")) System.out.println("Using Connection URL: " + connectionURL);

        // Make the connection
        currentConnection  = DriverManager.getConnection(connectionURL, uname, password);
        currentStatement = currentConnection.createStatement();
        if (Debug.isSet("JDBC")) System.out.println("Opened Database connection.");


    }
    //#*******************************************************************************


    //#*******************************************************************************
    public void processResult(ResultSet result, PrintStream dOut) throws SQLException, IOException {

        ResultSetMetaData meta;
        int count;

        meta = result.getMetaData();
        count = meta.getColumnCount();

        printColumnNames(meta, dOut);


        int limit = 0;
        //if(result.first()){
        while (limit++ < 10) {
            for (int c = 1; c <= count; c++) {
                String tmp = result.getString(c);
                if (tmp == null) {

                    dOut.println("null ");
                    System.out.print("null ");
                }


                dOut.print(tmp + "   ");
                System.out.print(tmp + "   ");
            }
            dOut.println("");
            System.out.println("");
            result.next();
        }
        dOut.println("limit: " + limit);
        //}
    }
    //#*******************************************************************************



    //#*******************************************************************************
    /**
     *	Read the meta data stream and print up column names with type information
     *
     *
     */
    public void printColumnNames(ResultSetMetaData m, PrintStream dOut) throws SQLException {

        int count = m.getColumnCount();
        String metaStuff = "";

        // Make the array one bigger and fill the 0th element with a
        // dummy so the index matches the column index rom the ResultSet


        for (int c = 1; c <= count; c++) {

            String name = m.getColumnName(c);
            String type = m.getColumnTypeName(c);

            if (name == null)
                name = "NULL  ";
            else
                name += "(" + type + ")  ";

            metaStuff += name;

        }

        dOut.println(metaStuff);
        for (int i = 0; i < metaStuff.length(); i++)
            dOut.print("-");
        dOut.println("");

    }
    //#*******************************************************************************


}



