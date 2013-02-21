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

package dods.servers.sql;

import java.util.*;
import java.io.*;
//import java.sql.*;

import dods.dap.*;
import dods.dap.Server.*;
import dods.dap.parser.*;

/** A specialized case of a <code>RelOpClause</code> that can
 * return an SQL representation of its self.
 * @see Operator
 * @see RelOpClause
 * @author ndp */
public class SqlRelOpClause extends RelOpClause implements TopLevelClause {


    private static final boolean _Debug = false;


    /** Creates a new SqlRelOpClause. If the lhs and all the elements of the rhs
     *  are constant, the SqlRelOpClause will be flagged as constant, and
     *  evaluated immediately.
     *
     * @param op The operator invoked by the clause
     * @param lhs The left-hand side of the comparison.
     * @param rhs A list of SubClauses representing the right-hand side of the
     * comparison.
     * @exception SDODSException Thrown if the clause is constant, but
     *            the attempt to evaluate it fails.
     */
    protected SqlRelOpClause(int op, SubClause lhs, List rhs)
            throws SDODSException {

        super(op, lhs, rhs);

    }

    /**
     * Supplies the (best effort) representation of this
     * <code>SqlRelOpClause</code> as an SQL syntax for inclusion
     * in the WHERE clause in an SQL SELECT query.
     *
     * @return String representation of this Clause as a snipet of SQL.
     */
    public String getSqlRepresentation(boolean useDatasetName, sqlDDS dds)
            throws InvalidOperatorException {

        String lop, op, rop, s = "";

        boolean isRegExp = false;


        List lhsChildren = lhs.getChildren();

        if (!lhsChildren.isEmpty())
            throw new InvalidOperatorException("Nested expressions are not supported! " +
                    "(LHS of RelOpCluase has children!)");

        BaseType lhsVal = lhs.getValue();


        if (lhs.isConstant()) {

            // Make a special print writer to catch the Value of the
            // constant in a String.
            StringWriter val = new StringWriter();
            lhsVal.printVal(new PrintWriter(val), "", false);
            lop = val.toString();

            if (_Debug) System.out.println("\nLOP is constant valued: `" + lop + "`");
            if (_Debug) System.out.print("LHS BaseType   getName(): \"" + lhsVal.getName() + "\"");
            if (_Debug) System.out.print(" getTypeName(): \"" + lhsVal.getTypeName() + "\"");
            if (_Debug) System.out.print(" printVal(): `");
            if (_Debug) lhsVal.printVal(System.out, "", false);
            if (_Debug) System.out.println("`");

        } else {


            lop = lhsVal.getLongName();

            if (_Debug) System.out.println("\nLOP from bt.getLongName(): " + lop);

            if (useDatasetName)
                lop = dds.getName() + "." + lop;


        }


        switch (getOperator()) {
            case ExprParserConstants.EQUAL:
                op = "=";
                break;
            case ExprParserConstants.NOT_EQUAL:
                op = "<>";
                break;
            case ExprParserConstants.GREATER:
                op = ">";
                break;
            case ExprParserConstants.GREATER_EQL:
                op = ">=";
                break;
            case ExprParserConstants.LESS:
                op = "<";
                break;
            case ExprParserConstants.LESS_EQL:
                op = "<=";
                break;
            case ExprParserConstants.REGEXP:
                op = " LIKE ";
                isRegExp = true;
                break;
            default:
                throw new InvalidOperatorException("Only Simple Comparison Operators Are Support in SQL servers.");
                //break;

        }


        if (isRegExp) {

            List ropLst = getRHS();

            if (_Debug) System.out.print("\n\n");
            if (_Debug) System.out.println("Found A Regular Expression!");

            for (int i = 0; i < ropLst.size(); i++) {


                SubClause rhs_i = (SubClause) ropLst.get(i);

                List rhs_i_Children = rhs_i.getChildren();


                if (!rhs_i_Children.isEmpty())
                    throw new InvalidOperatorException("Nested expressions are not supported! (RHS[" + i +
                            "] of RelOpClause has children!)");

                BaseType rhsVal = rhs_i.getValue();

                if (_Debug) System.out.print("Rop[" + i + "] BaseType.getName: \"" + rhsVal.getName() + "\"");
                if (_Debug) System.out.println(" BaseType.getTypeName: \"" + rhsVal.getTypeName() + "\"");

                if (rhsVal instanceof sqlString) {

                    String regexp = ((sqlString) rhsVal).getValue();

                    if (_Debug) System.out.println("regexp: \"" + regexp + "\"");

                    rop = regexp2SQL(regexp);
                } else {
                    throw new InvalidOperatorException("Regular Expressions MUST Be Strings!");
                }
                if (_Debug) System.out.println("ROP: " + rop);

                if (i > 0)
                    s += " OR ";

                s += lop + op + rop;


            }
            System.out.print("\n\n");

        } else {

            List ropLst = getRHS();

            for (int i = 0; i < ropLst.size(); i++) {


                SubClause rhs_i = (SubClause) ropLst.get(i);

                List rhs_i_Children = rhs_i.getChildren();


                if (!rhs_i_Children.isEmpty())
                    throw new InvalidOperatorException("Nested expressions are not supported! (RHS[" + i +
                            "] of RelOpClause has children!)");

                BaseType rhsVal = rhs_i.getValue();

                if (_Debug) System.out.print("Rop[" + i + "] BaseType.getName: \"" + rhsVal.getName() + "\"");
                if (_Debug) System.out.println(" BaseType.getTypeName: \"" + rhsVal.getTypeName() + "\"");



                //Is this a constant? Or a reference to another variable?
                if (rhs_i.isConstant()) {

                    // Strings (and thus URL's) need to encased in single quotes
                    // 'like this' or they won't fly at query time.
                    // So, we detect Strings and URL's and handle them differently
                    // then other types.

                    if (rhsVal instanceof SDString) {

                        rop = "'" + ((SDString) rhsVal).getValue() + "'";

                    } else if (rhsVal instanceof SDURL) {
                        rop = "'" + ((SDURL) rhsVal).getValue() + "'";

                    } else {

                        StringWriter sw = new StringWriter();
                        PrintWriter pw = new PrintWriter(sw);

                        // Because printVal (as overloaded for the server-side) only
                        // prints a variable's value if it is in the current projection,
                        // we set the project property to true in this instrumentation
                        // code and then reset its original value after printing.
                        // 2/11/2000 jhrg
                        ServerMethods sm = (ServerMethods) rhsVal;
                        boolean proj = sm.isProject();
                        sm.setProject(true);

                        rhsVal.printVal(pw, "", false);

                        sm.setProject(proj);

                        rop = sw.toString();

                    }
                } else {
                    rop = rhsVal.getLongName();
                    rop = dds.getName() + "." + rop;
                }
                if (_Debug) System.out.println("ROP: " + rop);

                if (i > 0)
                    s += " OR ";

                s += lop + op + rop;


            }

        }

        return s;


    }


    private String regexpError1 = "\n\n    The SQL Server only supports part\n" +
            "    of the regular expression syntax.\n";


    private String regexpError2 = "    The supported special characters are\n" +
            "    '.' and '.*'. The set notation '[...]'\n" +
            "    is also supported, but not in conjunction\n" +
            "    with the '*' character.\n";

    /** This method attempts to convert a true (perl-esque) regexp into the
     limited regexp syntax supported by SQL. If it can't manage the conversion
     then it throws an <code>InvalidOperatorException</code>. Eventually that
     may prove to be the wrong tactic: The current implmentation forces the client
     to provide a regexp that can be converted into a clause in an SQL query. One
     possible alternative is to attempt the conversion: If successful then the SQL
     representation is returned and the DBMS applies the condition to the data. If
     the representation isn't possible then a <code>null</code> is returned and the
     Clause will stay in the Clause list to be evaluated in the traditional manner
     by the DODS server when that data ios returned to it from the DBMS. Which is better?
     I guess only time will tell...
     */
    private String regexp2SQL(String regexp) throws InvalidOperatorException {


        String unsupportedChars[] = {"\\|", "\\(", "\\)", "$", "+", "?"};
        //int index;


        // Scan for unsupported regular expression characters...
        for (int i = 0; i < unsupportedChars.length; i++) {
            if (regexp.indexOf(unsupportedChars[i]) >= 0) {
                String emsg = regexpError1 + "\n    The '" +
                        unsupportedChars[i] +
                        "' special character is not supported.\n\n" +
                        regexpError2;
                throw new InvalidOperatorException(emsg);
            }
        }


        StringBuffer resb = new StringBuffer(regexp);
        if (_Debug) System.out.println("resb: '" + resb + "'  length: " + resb.length());

        // Escape characters used by SQL "LIKE" syntax
        for (int i = 0; i < resb.length(); i++) {

            if (resb.charAt(i) == '%') {

                if (i > 0) {
                    if (resb.charAt(i - 1) != '\\')
                        resb.insert(i, '\\');
                } else
                    resb.insert(i, '\\');
            }
            if (resb.charAt(i) == '_') {

                if (i > 0) {
                    if (resb.charAt(i - 1) != '\\')
                        resb.insert(i, '\\');
                } else
                    resb.insert(i, '\\');
            }

        }

        // QC the use of the '*' character
        boolean badFormat = false;
        for (int i = 0; i < resb.length(); i++) {

            if (_Debug) System.out.print("*");
            if (resb.charAt(i) == '*') {
                if (i > 0) {
                    if (resb.charAt(i - 1) != '.') {
                        badFormat = true;
                    }
                } else
                    badFormat = true;
            }
        }
        if (badFormat) {
            String emsg = regexpError1 + "\n    The '*'" +
                    " must be used in conjunction with '.'\n" +
                    "    As in '.*' only!\n\n" +
                    regexpError2;
            throw new InvalidOperatorException(emsg);
        }


        // Convert what regular expressions we support to the SQL "LIKE" syntax.
        for (int i = 0; i < resb.length(); i++) {

            if (_Debug) System.out.print(".");
            if (resb.charAt(i) == '.') {

                if (i < resb.length() - 1) {
                    if (resb.charAt(i + 1) == '*')
                        resb.replace(i, i + 2, "%");
                    else
                        resb.replace(i, i + 1, "_");
                } else {
                    if (_Debug) System.out.println("Found a dot ('.') at the end of the regexp.");
                    resb.replace(i, i + 1, "_");
                }
            }

        }

        String result = "'" + resb.toString() + "'";

        if (_Debug) System.out.println("RegExp after replacement: " + result);

        return (result);

    }


}
