/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@coas.oregonstate.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//
/////////////////////////////////////////////////////////////////////////////

/* $Id: sqlCEEval.java,v 1.10.2.2 2004/07/23 21:41:15 ndp Exp $
 *
 */

package dods.servers.sql;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

import dods.dap.BaseType;
import dods.dap.DAS;
import dods.dap.NoSuchVariableException;
import dods.dap.Server.BTFunctionClause;
import dods.dap.Server.BoolFunction;
import dods.dap.Server.BoolFunctionClause;
import dods.dap.Server.CEEvaluator;
import dods.dap.Server.Clause;
import dods.dap.Server.ClauseFactory;
import dods.dap.Server.InvalidOperatorException;
import dods.dap.Server.SDODSException;
import dods.dap.Server.ServerDDS;
import dods.dap.Server.ServerMethods;
import dods.dap.parser.ExprParserConstants;

/**
 * This class is used to parse and evaluate a constraint expression. When
 * constructed it must be passed a valid DDS along with the expression. This DDS
 * will be used as the environment (collection of variables and functions)
 * during the parse and evaluation of the constraint expression.
 * <p>
 * A server (servlet, CGI, ...) must first instantiate the DDS (possibly reading
 * it from a cache) and then create and instance of this class. Once created,
 * the constraint may be parsed and then evaluated. The class supports sending
 * data based on the results of CE evaluation. That is, the send() method of the
 * class combines both the evaluation of the constraint and the output of data
 * values so that the server can return data using a single method call.
 * 
 * This implementation is intended to be reading data from a JDBC connection to
 * a relational database. And as such the send method has been modified to to
 * handle bookkeeping issues, and a method added that generates and SQL SELECT
 * statement by interogating the DDS to determine the "Projection" and by
 * converting the "Selection" (the Clauses) into the constraint used in the
 * Select statement's WHERE section.
 * 
 * @version $Revision: 1.10.2.2 $
 * @author ndp
 * @see ServerDDS
 * @see ServerMethods
 * @see Clause
 */
public class sqlCEEval extends CEEvaluator implements ExprParserConstants {

    private static final boolean _Debug = true;

    /**
     * Construct a new <code>sqlCEEval</code> with <code>dds</code> as the DDS
     * object with which to resolve all variable and function names.
     * 
     * @param dds
     *            DDS object describing the dataset targeted by this constraint.
     */
    public sqlCEEval(ServerDDS dds) {
        super(dds);
    }

    /**
     * Construct a new <code>sqlCEEvaluator</code> with <code>dds</code> as the
     * DDS object with which to resolve all variable and function names, and
     * <code>clauseFactory</code> as a source of Clause objects .
     * 
     * @param clauseFactory
     *            The factory which will be used by the parser to construct the
     *            clause tree. This allows servers to pass in a factory which
     *            creates custom clause objects.
     * @param dds
     *            DDS object describing the dataset targeted by this constraint.
     */
    public sqlCEEval(ServerDDS dds, ClauseFactory clauseFactory) {

        super(dds, clauseFactory);

    }

    /**
     * This function sends the variables described in the constrained DDS to the
     * output described by <code>sink</code>. This function calls
     * <code>ServerIO::serialize()</code>. to achieve this data transmission.
     * This implementation is intended to be reading data from a JDBC connection
     * to a relational database.
     * <p>
     * Relational databases appear relatively "flat" to DODS. By this we mean
     * that DODS datasets can have very complex multilevel structure. Relational
     * databases appear to DODS as a dataset populated by one or more Sequences,
     * each one representing a table in the database. The table contents are
     * generally simple types, or arrays of bytes. Since relational databases
     * support cross table (and thus cross sequence) queries and since they
     * return the results of these queries in a single "table like" object, we
     * must take care to unpack this returned data into the appropriate members
     * of the DDS representation of the dataset. The read() method of the
     * Sequence type plays a key roll in this, along with the read() methods of
     * the simple types and the send() method of the CEEvaluator.
     * </p>
     * <p>
     * This send() method handles "rewinding" the row index in the ResultSet so
     * that as each Sequence (table) is processed, the ResultSet object is
     * starting at the first row of returned data, and the Sequence's read()
     * method can scan the columns it needs from all of the rows of returned
     * data.
     * 
     * @param dataset
     *            The name of the dataset to send.
     * @param sink
     *            A pointer to the output buffer for the data.
     * @param specialO
     *            Special Object to carry implementation specific content..
     * @see #parseConstraint(String) parseConstraint()
     * @see ServerMethods#serialize
     */
    public void send(String dataset, OutputStream sink, Object specialO)
            throws NoSuchVariableException, SDODSException, IOException {

        // ResultSet rs = ((sqlResponse) specialO).getResultSet();

        Enumeration e = getDDS().getVariables();
        while (e.hasMoreElements()) {

            /*
             * try { // Rewind row index for each Sequence (Table) rs.first(); }
             * catch(SQLException sqle){ throw new IOException(sqle.toString());
             * }
             */

            ServerMethods s = (ServerMethods) e.nextElement();

            if (_Debug)
                System.out.println("CEE --- Sending variable: "
                        + ((BaseType) s).getName());

            if (s.isProject())
                s.serialize(dataset, (DataOutputStream) sink, this, specialO);
        }
    }

    /**
     * Generates an SQL SELECT statement by interogating the DDS object to
     * determine the "Projection" and by converting the "Selection" (the
     * Clauses) into the constraint used in the SELECT statement's WHERE
     * section.
     * 
     * @return <code>String</code> containing the prepared SQL Select statement
     *         for use in the Databse query.
     */
    public String getSQLQuery(DAS das, boolean useDatasetName)
            throws InvalidOperatorException {

        sqlDDS dds = (sqlDDS) getDDS();

        String projectedVariables = "";
        String projectedTables = "";
        Vector projV = dds.getRequestedVars();
        Vector projT = dds.getRequestedTables();

        String prefix = "";
        if (useDatasetName)
            prefix = dds.getName() + ".";

        // Get the list of projected variables
        Enumeration e = projV.elements();
        while (e.hasMoreElements()) {

            projectedVariables += prefix
                    + ((BaseType) e.nextElement()).getLongName();
            if (e.hasMoreElements())
                projectedVariables += ", ";
        }

        // Get the list of projected tables/sequences.
        e = projT.elements();
        while (e.hasMoreElements()) {

            projectedTables += prefix
                    + ((BaseType) e.nextElement()).getLongName();
            if (e.hasMoreElements())
                projectedTables += ", ";
        }

        if (_Debug)
            System.out.println("projectedVariables: '" + projectedVariables
                    + "'");
        if (_Debug)
            System.out.println("projectedTables:    '" + projectedTables + "'");

        String query = "SELECT " + distinct() + projectedVariables + " FROM "
                + projectedTables;

        String constraint = convertClausesToSQL(useDatasetName);

        if (constraint != null)
            query += " WHERE " + constraint;

        return (query);

    }

    /**
     * Checks to see if the client used the unique() function. Because of the
     * way that SQL uses the DISTINCT keyword in the syntax for the SELECT
     * statement, it requires us to check for the presence of a call to unique()
     * prior to evaluating all of the other Clauses (Function or otherwise).
     * This function locates all of the Clauses that resolve to the unique()
     * function and if there are such Clauses then it returns the SQLCommand
     * that describes the unique() function in SQL land. In this case it is a
     * String containing the SQL keyword DISTINCT.
     * 
     * @return A String containg the DISTINCT keyword if unique() was invoked,
     *         or an empty String if unique() was not invoked
     */
    private String distinct() {

        String distinct = "";
        LinkedList uClauses = new LinkedList();

        Enumeration enumi = getClauses();
        while (enumi.hasMoreElements()) {
            Clause c = (Clause) enumi.nextElement();
            if (c instanceof BoolFunctionClause) {

                BoolFunctionClause bfc = (BoolFunctionClause) c;
                BoolFunction func = bfc.getFunction();
                List args = bfc.getChildren();

                if (func.getName().equals("unique")) {
                    if (func instanceof SqlBoolFunction) {
                        distinct = ((SqlBoolFunction) func).getSQLCommand(args);
                        uClauses.add(c);

                        if (_Debug)
                            System.out
                                    .println("THEY CALLED THE unique() FUNCTION!");
                    }

                }
            }
        }

        if (!uClauses.isEmpty()) {
            for (int i = 0; i < uClauses.size(); i++) {
                removeClause((Clause) uClauses.get(i));
            }
        }

        return distinct;

    }

    /**
     * Helper method for getSQLQuery(). Converts each Clause type into a string
     * of SQL used in the WHERE section of the SELECT statement.
     * 
     * @return a <code>String</code> containing the SQL representation of the
     *         <code>Clauses</code>. If no Clauses can be represented as SQL
     *         then this method returns <code>null</code>.
     * @return <code>String</code> containing the <code>Clauses</code> as an SQL
     *         fragment for use in the WHERE section of the SQL query.
     */

    protected String convertClausesToSQL(boolean useDatasetName)
            throws InvalidOperatorException {

        if (_Debug)
            System.out.println("Converting Clauses to SQL constraints...");

        String constraint = null;
        Enumeration enumi = getClauses();
        LinkedList toBePurged = new LinkedList();

        while (enumi.hasMoreElements()) {

            Clause c = (Clause) enumi.nextElement();

            String s = convertClauseToSQL(c, useDatasetName);

            if (_Debug)
                System.out.println("SQL: " + s);

            if (s != null) {
                if (constraint != null)
                    constraint += " AND " + "( " + s + " )";
                else
                    constraint = "(" + s + ")";

                toBePurged.add(c);
            }

        }

        for (int i = 0; i < toBePurged.size(); i++) {
            Clause c = (Clause) toBePurged.get(i);
            removeClause(c);
        }

        return constraint;
    }

    /**
     * Helper method for convertClausesToSQL(). Converts a Clause type into a
     * string of SQL used in the WHERE section of the SELECT statement.
     * 
     * @return a <code>String</code> containing the SQL representation of the
     *         <code>Clause</code>. If the <code>Clause</code> cannot be
     *         represented as SQL then this method returns <code>null</code>.
     * @return <code>String</code> containing the <code>Clause</code> as an SQL
     *         fragment for use in the WHERE section of the SQL query.
     */

    protected String convertClauseToSQL(Clause c, boolean useDatasetName)
            throws InvalidOperatorException {

        sqlDDS dds = (sqlDDS) getDDS();

        // String lop, op, rop;
        String s = "";

        // boolean isRegExp = false;

        if (c instanceof SqlRelOpClause) {

            SqlRelOpClause relop = (SqlRelOpClause) c;

            s = relop.getSqlRepresentation(useDatasetName, dds);

        } else if (c instanceof BTFunctionClause) {

            BTFunctionClause btfc = (BTFunctionClause) c;

            if (btfc.getFunction() instanceof SqlBTFunction) {

                List args = btfc.getChildren();
                s = ((SqlBTFunction) btfc).getSQLCommand(args);

            } else {
                s = null;
            }

            /*
             * System.out.println("\n\n\n\n\n");
             * System.out.println("I found a BTFunctionClause!!!!");
             * 
             * System.out.println("Function is a '"+
             * btfc.getFunction().getClass().getName()+"'");
             * 
             * List args = btfc.getChildren();
             * 
             * System.out.println("Function had "+args.size()+" argument(s).");
             * 
             * for(int i=0; i<args.size() ;i++){ try {
             * dods.util.Tools.probeObject(args.get(i)); //BaseType bt =
             * args[i].eval(getDDS(),new Object());
             * //System.out.println("arg["+i+"]: '"+bt.getName()+"'"); } catch
             * (Throwable e){ } }
             * 
             * System.out.println("\n\n\n\n\n");
             */

        } else if (c instanceof BoolFunctionClause) {

            BoolFunctionClause bfc = (BoolFunctionClause) c;
            BoolFunction func = bfc.getFunction();

            if (func instanceof SqlBoolFunction) {

                List args = bfc.getChildren();
                s = ((SqlBoolFunction) func).getSQLCommand(args);

            } else {
                s = null;
            }

            /*
             * 
             * System.out.println("\n\n\n\n\n");
             * System.out.println("I found a BoolFunctionClause!!!!");
             * 
             * System.out.println("Function is a '"+
             * bfc.getFunction().getClass().getName()+"'");
             * 
             * List args = bfc.getChildren();
             * 
             * System.out.println("Function had "+args.size()+" argument(s).");
             * 
             * for(int i=0; i<args.size() ;i++){ try { ValueClause val =
             * (ValueClause) args.get(i);
             * 
             * System.out.println("arg["+i+"]: '"+val+"'"); } catch (Throwable
             * e){ } }
             * 
             * System.out.println("\n\n\n\n\n");
             */

            // s = null;

        } else {

            s = null;
        }

        return s;

    }

    protected String getSQLVariables() {
        String s = null;

        return (s);
    }

    protected String getSQLFromClause() {
        String s = null;

        return (s);
    }

    protected String getSQLWhereClause() {
        String s = null;

        return (s);
    }

}
