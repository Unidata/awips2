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

package dods.servers.sql;


/**
 * Test routine for the JDBC connection
 *
 * @version $Revision: 1.2.2.2 $
 * @author ndp
 */

import java.io.*;
import java.util.*;
import java.sql.*;

import gnu.getopt.Getopt;

import dods.util.*;

public class JDBCTest {

    public static String DDSFile, ConstraintExpression;
    public static String sqlQuery, iniFileName;

    private static boolean verboseOutput;
    private static PrintStream dOut;

    // Constructor
    public JDBCTest() {
        iniFileName = "DODS.ini";
        sqlQuery = "SELECT * FROM *";
        dOut = System.out;

    }


    public static void main(String[] args) throws Exception {

        JDBCTest jdbct = new JDBCTest();


        try {

            if (jdbct.parse_options(args)) {

                System.out.println("-------------------------------------------");
                System.out.println(".ini File: " + iniFileName);
                System.out.println("SQL Query: " + sqlQuery);


                System.out.println("Attempting to Connect to DBMS.");
                Statement stmnt = connect2DB();

                if (stmnt != null)
                    System.out.println("Connected to database.");

                System.out.println("Sending Query.");
                ResultSet rs = stmnt.executeQuery(sqlQuery);

                if (rs != null)
                    System.out.println("Got ResultSet From DBMS.");


                if (!processResult(rs)) {
                    System.out.println("\n\n\nYour Query Produced No Matches in The Database.\n\n");
                }
                stmnt.close();


                System.out.println("-------------------------------------------");
            }

        } catch (Throwable e) {
            System.out.println("\n\nERROR of Type: " + e.getClass().getName() + "\n");
            System.out.println("Message:\n" + e.getMessage() + "\n");
            System.out.println("Stack Trace: ");
            e.printStackTrace(System.out);
            System.out.println("\n\n");
        }

        System.exit(0);
    }


    //#*******************************************************************************
    public static Statement connect2DB()
            throws SQLException, FileNotFoundException, IOException {

        Connection conn = null;
        Statement stmt = null;


        // Set some default drivers
        String jdbcDriver = "oracle.jdbc.driver.OracleDriver";
        String connectionURL = "jdbc:oracle:thin:@whsun4.wh.whoi.edu:1526:nefsc2";
        String uname = "";
        String password = "";


        // Go Read the ini file if there is one...
        iniFile inf = new iniFile(iniFileName);

        if (inf.setSection("JDBC")) {
            String tmp = null;

            tmp = inf.getProperty("driver");
            if (tmp != null) jdbcDriver = tmp;

            tmp = inf.getProperty("connectionurl");
            if (tmp != null) connectionURL = tmp;

            tmp = inf.getProperty("username");
            if (tmp != null) uname = tmp;

            tmp = inf.getProperty("password");
            if (tmp != null) password = tmp;

        }

        // Keep us updated
        dOut.println("Using JDBC Driver:    " + jdbcDriver);
        dOut.println("Using Connection URL: " + connectionURL);
        dOut.println("Using user:           " + uname);
        dOut.println("Using Password:       " + password);

        // Add the JDBC driver to the system properties

        try {
            // Load the JDBC Driver
            Class.forName(jdbcDriver);

        } catch (ClassNotFoundException e) {
            throw new SQLException("\n Cannot Load JDBC Driver Class: " + e.getMessage() +
                    "\n Is the driver name spelled correctly?" +
                    "\n Is the .class file or the jar file" +
                    "\n containing the driver on the CLASSPATH ??\n\n");
        }

        // Make the connection
        System.out.println("Attempting to getConnection()");
        conn = DriverManager.getConnection(connectionURL, uname, password);
        System.out.println("Attempting to createStatement()");
        stmt = conn.createStatement();
        //stmt = conn.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);

        return (stmt);
    }
    //#*******************************************************************************



    //#*******************************************************************************
    public static boolean processResult(ResultSet result) throws SQLException, IOException {

        ResultSetMetaData meta;
        int count;

        meta = result.getMetaData();
        count = meta.getColumnCount();

        printColumnNames(meta);


        int limit = 0;

        boolean done = false;
        while (result.next() && !done) {
            for (int c = 1; c <= count; c++) {
                String tmp = result.getString(c);
                if (verboseOutput) {
                    if (tmp == null) {

                        dOut.println("null ");
                    }
                    dOut.print(tmp + "   ");
                }
            }
            if (verboseOutput) dOut.println("");
            limit++;
        }
        dOut.println("\nScanned " + limit + " rows...");

        if (limit == 0)
            return false;
        return (true);

    }
    //#*******************************************************************************





    //#*******************************************************************************
    /**
     *	Read the meta data stream and build up column names with type information
     *
     *
     */
    public static void printColumnNames(ResultSetMetaData m) throws SQLException {

        int count = m.getColumnCount();
        String metaStuff = "";


        for (int c = 1; c <= count; c++) {
            //String name1 =  m.getCatalogName(c);
            //String name2 =  m.getColumnClassName(c);
            //String name3 =  m.getColumnLabel(c);
            //String name4 =  m.getSchemaName(c);
            //String name5 =  m.getTableName(c);
            //String name6 =  m.getColumnName(c);
            //String name7 =  m.getColumnTypeName(c);
            //System.out.println("NAMES:");
            //System.out.println("getCatalogName():     "+name1);
            //System.out.println("getColumnClassName(): "+name2);
            //System.out.println("getColumnLabel():     "+name3);
            //System.out.println("getSchemaName():      "+name4);
            //System.out.println("getTableName():       "+name5);
            //System.out.println("getColumnName():      "+name6);
            //System.out.println("getColumnTypeName():  "+name7);

            String name = m.getColumnName(c);
            String type = m.getColumnTypeName(c);

            if (name == null)
                name = "NULL  ";
            else
                name += "(" + type + ")  ";

            metaStuff += name;

        }

        for (int i = 0; i < metaStuff.length(); i++)
            dOut.print("-");
        dOut.println("");
        dOut.println(metaStuff);
        for (int i = 0; i < metaStuff.length(); i++)
            dOut.print("-");
        dOut.println("");


    }
    //#*******************************************************************************




    //***************************************************************
    public static boolean parse_options(String[] args) {

        boolean retVal = true;

        Getopt g = new Getopt("JDBCTest", args, "q:i:v:h:");

        int c;
        String arg;
        while ((c = g.getopt()) != -1) {

            switch (c) {

                case 'q':
                    arg = g.getOptarg();
                    dOut.print("SQL Query: " + ((arg != null) ? arg : "null") + "\n");
                    sqlQuery = arg;
                    break;

                case 'v':
                    verboseOutput = true;
                    arg = g.getOptarg();
                    dOut.print("Verbose Output: ON\n");
                    break;

                case 'i':
                    arg = g.getOptarg();
                    dOut.print("iniFile: \"" + ((arg != null) ? arg : "null") + "\"\n");
                    if (arg != null)
                        iniFileName = arg;
                    break;

                    //
                case 'h':
                    arg = g.getOptarg();
                default:
                    System.out.println("*******************************************************************************************************************");
                    System.out.println("Usage:");
                    System.out.println("JDBCTest [-q \"SqlQuery\"] [-v] [-i iniFileName]");
                    System.out.println("Where:");
                    System.out.println("    -q \"SqlQuery\" = This option specifies the SQL query for the DBMS.");
                    System.out.println("                      SqlQuery must be a correctly formatted SQL Query for the target database.");
                    System.out.println("                      (It should be a SELECT statement.)");
                    System.out.println("                      And it must be contained in quotes (\").");
                    System.out.println("    -v = Turns on verbose mode. (This will show you all of the returned data");
                    System.out.println("         from the DBMS.  Default: off");
                    System.out.println("    -i iniFileName = The name of the .ini file containing all of the JDBC information.");
                    System.out.println("         The file MUST BE in your home directory.");
                    System.out.println("         Default: \"DODS.ini\"");
                    System.out.println("");
                    System.out.println("Examples:");
                    System.out.println("    java dods.servers.sql.JDBCTest -q \"Select distinct instrument_id from drifter\"");
                    System.out.println("    java dods.servers.sql.JDBCTest -v -q \"Select distinct instrument_id from drifter\"");
                    System.out.println("    java dods.servers.sql.JDBCTest -v -i \"my.ini -q\" \"Select distinct instrument_id from drifter\"");
                    System.out.println("*******************************************************************************************************************");
                    retVal = false;
                    break;
            }
        }

        return (retVal);
    }
    //***************************************************************


}




