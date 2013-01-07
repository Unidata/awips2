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
 * Test routine for the SD classes
 *
 * @version $Revision: 1.3.2.1 $
 * @author ndp
 */

import java.io.*;
import java.util.*;
import java.sql.*;

import gnu.getopt.Getopt;

import dods.dap.*;
import dods.dap.parser.*;
import dods.dap.Server.*;
import dods.util.*;

public class sqlTest {

    public static String DDSFile, ConstraintExpression;

    private static boolean Debug;
    private static PrintStream dOut;

    // Constructor
    public sqlTest() {
    }


    //***************************************************************
    // Dump the Server DDS contents to stdout.
    public static void print_SDDS(ServerDDS sdds, boolean constrained) {

        System.out.println("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
        System.out.println("sqlDDS:");
        Enumeration e = sdds.getVariables();

        while (e.hasMoreElements()) {
            Object o = e.nextElement();
            ServerMethods s = (ServerMethods) o;
            BaseType bt = (BaseType) o;

            System.out.println(bt.getTypeName() + " " + bt.getName() + ":");
            System.out.println("Constrained DDS:");

            bt.printDecl(System.out, "    ", true, constrained);


            System.out.println("Declaration and Value:");

            if (s.isRead()) {

                try {
                    bt.printVal(System.out, "    ", true);
                } catch (NullPointerException except) {
                    System.out.println(" Instance not Allocated.");
                }
            } else {
                System.out.println(" Item not yet initialized.");
            }

            System.out.print(" isProj: " + s.isProject());
            System.out.print("    isRead: " + s.isRead());
            System.out.println("    isSynth: " + s.isSynthesized());
            if (e.hasMoreElements())
                System.out.println("- - - - - - - - - - - - - - - - - -");
        }
        System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");

    }
    //***************************************************************



    //***************************************************************
    public static void parse_options(String[] args) {


        Getopt g = new Getopt("SDTest", args, "f:c:");

        int c;
        String arg;
        while ((c = g.getopt()) != -1) {
            switch (c) {
                case 'f':
                    arg = g.getOptarg();
                    if (Debug)
                        dOut.print("DDS File: " +
                                ((arg != null) ? arg : "null") + "\n");
                    DDSFile = arg;
                    break;
                case 'c':
                    arg = g.getOptarg();
                    if (Debug)
                        dOut.print("Constraint Expression: \"" +
                                ((arg != null) ? arg : "null") + "\"\n");

                    ConstraintExpression = arg;
                    break;
                    //
                case '?':
                    break; // getopt() already printed an error
                    //
                default:
                    if (Debug) dOut.print("getopt() returned " + c + "\n");
            }
        }
    }
    //***************************************************************


    public static sqlDDS getDDS() throws FileNotFoundException, DDSException, ParseException {

        File fin = new File(DDSFile);
        FileInputStream fp_in = new FileInputStream(fin);
        DataInputStream dds_source = new DataInputStream(fp_in);

        sqlServerFactory sfactory = new sqlServerFactory();
        sqlDDS myDDS = new sqlDDS("bogus", sfactory);

        if (Debug) dOut.println("Parsing DDS...");
        myDDS.parse(dds_source);

        //if(Debug) dOut.println("Printing DDS...");
        //myDDS.print(dOut);

        //print_SDDS(myDDS,false);

        return (myDDS);

    }


    public static void main(String[] args) throws Exception {

        sqlTest sdt = new sqlTest();

        dOut = System.out;
        Debug = true;


        try {

            File fout = new File("a.out");
            FileOutputStream fp_out = new FileOutputStream(fout);
            DataOutputStream sink = new DataOutputStream(fp_out);

            System.out.println("-------------------------------------------");

            System.out.println("Debugging Display: " + (Debug ? "ON" : "OFF"));
            parse_options(args);

            System.out.println("...........................................");


            sqlDDS myDDS = getDDS();

// Utilize the getDAS() method to get a parsed and populated DAS
// for this request.
            DAS myDAS = new DAS();


            myDDS.print(System.out);



//testNameBuilder(myDDS);


            if (Debug) dOut.println("Constructing CEEvaluator...");
            sqlCEEval ce = new sqlCEEval(myDDS);

            if (Debug) dOut.println("Parsing Constraint Expression: " + ConstraintExpression);
            ce.parseConstraint(ConstraintExpression);

            myDDS.printConstrained(dOut);


            String query = ce.getSQLQuery(myDAS, true);
            if (Debug) dOut.println("Query String: \"" + query + "\"");

            Statement stmnt = connect2DB();

            ResultSet rs = stmnt.executeQuery(query);


            if (rs.next() && rs.isFirst()) {

                processResult(rs);

                System.out.println("ResultSet.isFirst(): " + rs.isFirst());
                System.out.println("ResultSet.first(): " + rs.first());

                System.out.println("ResultSet.absolute(1): " + rs.absolute(1));

                sqlResponse res = new sqlResponse(rs);

                if (Debug) dOut.println("Attempting to send data...");
                ce.send(myDDS.getName(), sink, res);


                print_SDDS(myDDS, true);
                myDDS.printConstrained(dOut);

            } else {
                System.out.println("\n\n\nYour Query Produced No Matches in The Database.\n\n");
            }
            stmnt.close();

//*/

            System.out.println("-------------------------------------------");

            sink.close();

        } catch (DODSException e) {
            System.out.println("\n\nERROR of Type: " + e.getClass().getName() + "\n");
            System.out.println("Message:\n" + e.getMessage() + "\n");
            System.out.println("Stack Trace: ");
            e.printStackTrace(System.out);
            System.out.println("\n\n");
        }

        System.exit(0);
    }


    public static void testNameBuilder(sqlDDS dds) {

        Enumeration v = dds.getVariables();

        while (v.hasMoreElements()) {

            BaseType thisBT = (BaseType) v.nextElement();
            System.out.println("\nHighest Level. Starting On: " + thisBT.getName());
            nameDigger(thisBT, dds);
        }

    }

    private static int Level = 0;

    private static void nameDigger(BaseType thisBT, sqlDDS dds) {


        Level++;
        System.out.print("Item: \"" + thisBT.getName() + "\"\t\tFullName: \"" + thisBT.getLongName() + "\"");
        System.out.print("\t\tLevel: " + Level);

        String fullName = null;
        if (thisBT instanceof DConstructor) {

            System.out.print("(It's a DConstructor...)\n");
            Enumeration dce = ((DConstructor) thisBT).getVariables();

            boolean done = false;
            while (dce.hasMoreElements()) {

                BaseType dcBT = (BaseType) dce.nextElement();
                nameDigger(dcBT, dds);
            }
        } else if (thisBT instanceof DVector) {

            System.out.print("(It's a DVector...)\n");
            PrimitiveVector pv = ((DVector) thisBT).getPrimitiveVector();

            nameDigger(pv.getTemplate(), dds);


        }
        System.out.println("");
        Level--;
    }


    //#*******************************************************************************
    public static Statement connect2DB()
            throws SQLException, FileNotFoundException, IOException {

        Connection conn = null;
        Statement stmt = null;


        // Set some default drivers
        String jdbcDriver = "openlink.jdbc2.Driver";
        String connectionURL = "jdbc:openlink://cupcake.oce.orst.edu/DSN=oplEOSDB";
        String uname = "";
        String password = "";


        // Go Read the ini file if there is one...
        DODSiniFile inf = new DODSiniFile();
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
        if (Debug) dOut.println("Using JDBC Driver:    " + jdbcDriver);
        if (Debug) dOut.println("Using Connection URL: " + connectionURL);

        // Add the JDBC driver to the system properties
        Properties p = System.getProperties();
        p.put("jdbc.drivers", jdbcDriver);
        System.setProperties(p);

        // Make the connection
        conn = DriverManager.getConnection(connectionURL, uname, password);
        //stmt = conn.createStatement ();
        stmt = conn.createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);

        return (stmt);
    }
    //#*******************************************************************************



    //#*******************************************************************************
    public static void processResult(ResultSet result) throws SQLException, IOException {

        ResultSetMetaData meta;
        int count;

        meta = result.getMetaData();
        count = meta.getColumnCount();

        printColumnNames(meta);


        int limit = 0;
        result.beforeFirst();
        dOut.println("ResultSet.isFirst(): " + result.isFirst());
//	while (result.next()  &&  limit++<10) {
        boolean done = false;
        while (result.next() && !done) {
            // for (int c = 1; c <= count; c++){
            //String tmp  = result.getString(c);
            //if(tmp == null){

            //dOut.println("null ");
            //}


            //dOut.print(tmp+"   ");
            // }
            //dOut.println ("");
            dOut.print(".");
            limit++;
            done = result.isLast();
        }
        dOut.println("\nScanned " + limit + " rows...");

        result.beforeFirst();
        dOut.println("ResultSet.isFirst(): " + result.isFirst());
        if (limit > 1) return;
//	while (result.next()  &&  limit++<10) {
        done = false;
        while (result.next() && !done) {
            //for (int c = 1; c <= count; c++){
            //String tmp  = result.getString(c);
            //if(tmp == null){

            //dOut.println("null ");
            //}


            //dOut.print(tmp+"   ");
            // }
            //dOut.println ("");
            dOut.print(".");
            done = result.isLast();
        }
        dOut.println("\nScanned " + limit + " rows...");

        dOut.println("limit: " + limit);
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
            String name1 = m.getCatalogName(c);
            //String name2 =  m.getColumnClassName(c);
            String name3 = m.getColumnLabel(c);
            String name4 = m.getSchemaName(c);
            String name5 = m.getTableName(c);
            String name6 = m.getColumnName(c);
            String name7 = m.getColumnTypeName(c);
            System.out.println("NAMES:");
            System.out.println("getCatalogName():     " + name1);
            //System.out.println("getColumnClassName(): "+name2);
            System.out.println("getColumnLabel():     " + name3);
            System.out.println("getSchemaName():      " + name4);
            System.out.println("getTableName():       " + name5);
            System.out.println("getColumnName():      " + name6);
            System.out.println("getColumnTypeName():  " + name7);

            String name = name6;
            String type = m.getColumnTypeName(c);
            //String type = m.getColumnTypeName(c);

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



