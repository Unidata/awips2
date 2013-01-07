/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, University of Rhode Island
// ALL RIGHTS RESERVED.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: James Gallagher <jgallagher@gso.uri.edu>
//
/////////////////////////////////////////////////////////////////////////////

package dods.servers.sql;

import java.io.*;
import java.util.*;
import java.sql.*;

import dods.dap.*;
import dods.dap.Server.*;

/** sqlDDS is a specialization of ServerDDS for the SQL server-side
 of DODS. This class includes methods used to build SQL query strings
 by exploring the DDS after the desired projection has been established.
 <p>
 The relationship that we are going to establish between the DODS
 data heirarchy and that found in a DBMS will be as follows:
 A "DataBase" with in a DBMS corresponds to a DODS DDS.
 Each Table in the "DataBase" corresponds to a different Sequence within
 The DODS DDS.
 Columns in the table correspond to variables in the Sequence.
 </p>
 @version $Revision: 1.6.2.1 $
 @author jhrg
 @see ServerDDS
 @see sqlCEEval */
public class sqlDDS extends ServerDDS implements Cloneable {

    private static final boolean _Debug = false;


    protected sqlDDS() {
        super();
    }

    /** Creates an empty <code>sqlDDS</code> with the given dataset name.
     @param n the dataset name */
    protected sqlDDS(String n) {
        super(n);
    }

    /** Creates an empty <code>sqlDDS</code> with the given
     <code>BaseTypeFactory</code>. This will be used for DODS servers
     which need to construct subclasses of the various
     <code>BaseType</code> objects to hold additional server-side
     information.
     @param factory the server <code>BaseTypeFactory</code> object. */
    public sqlDDS(BaseTypeFactory factory) {
        this(null, factory);
    }

    /** Creates an empty <code>sqlDDS</code> with the given dataset name
     and <code>BaseTypeFactory</code>. This will be used for DODS servers
     which need to construct subclasses of the various
     <code>BaseType</code> objects to hold additional server-side
     information.
     @param n the dataset name
     @param factory the server <code>BaseTypeFactory</code> object. */
    public sqlDDS(String n, BaseTypeFactory factory) {
        super(n, factory);
    }


    /** Return a clone of the <code>sqlDDS</code>. A deep copy is
     performed on this object and those it contains.
     @return a ServerDDS object. */
    public Object clone() {
        sqlDDS d = (sqlDDS) super.clone();
        return (d);
    }


    /** Returns a string naming of all of the projected variables
     in the DDS using their full qualified names. This becomes
     the list of columns requested from the database in the
     SELECT statement.
     Used by <code>sqlCEEval.getSQLQuery()</code>
     */
    public Vector getRequestedVars() {

        return (listProjected(true));
    }


    /** Returns a string naming of all of the projected variables
     at the highest levelin the DDS. This becomes the list of
     tables requested of the database in the SELECT statement.
     Used by <code>sqlCEEval.getSQLQuery()</code>
     */
    public Vector getRequestedTables() {

        return (listProjected(false));
    }


    /** Debugging Instrumentation used by deepProjectionCheck() */
    private void show(Vector v, PrintStream p, String space) {

        if (v == null) {
            p.println(space + "v==null");
        } else {
            Enumeration e = v.elements();
            int i = 0;
            while (e.hasMoreElements()) {
                p.print(space + "v[" + i + "]: ");
                p.println(((BaseType) e.nextElement()).getLongName());
                i++;
            }
        }
    }

    /** Used in deepProjectionCheck() */
    private int Level = 0;


    /** Return the shortest possible list of projected variables from
     * this DDS. For example if an entire structure is projected then the
     * individual members will not appear on the list, just the name of
     * the structure.
     **/
    private Vector listProjected(boolean deep) {

        Vector projList = new Vector();

        Enumeration v = vars.elements();

        while (v.hasMoreElements()) {

            BaseType bt = (BaseType) v.nextElement();
            ServerMethods sm = (ServerMethods) bt;

            if (sm.isProject()) {

                if (deep) {

                    Vector dpcList = new Vector();


// The following stuff didn't work the way I wanted...
// Because every variable needs to be specifically listed (using wild cards is not acceptable
// beacuse the DDS may not reveal all of the variables in the table, AND just naming the table
// will not result in a correctly formatted query we need to ask for EACH thing
// that we want....
/*
		    if(deepProjCheck(bt,dpcList)){
		        if(_Debug) System.out.println("Top Level call to deepProjCheck returned TRUE");
			projList.add(bt);

		    }
		    else {
		        if(_Debug) System.out.println("Top Level call to deepProjCheck returned FALSE");
			Enumeration bte = dpcList.elements();
			while(bte.hasMoreElements()){
			    projList.add(bte.nextElement());
			}
		    }
*/

                    deepProjCheck(bt, dpcList);
                    Enumeration bte = dpcList.elements();
                    while (bte.hasMoreElements()) {
                        projList.add(bte.nextElement());
                    }


                } else {
                    projList.add(bt);
                }
            }
        }

        if (_Debug) {
            System.out.println("PROJECTED LIST: ");
            show(projList, System.out, "    ");
        }

        return (projList);
    }


    /** Recursive helper function for listProjected() */
    private boolean deepProjCheck(BaseType bt, Vector uberV) {

        Level++;
        String space = "";

        if (_Debug) {
            for (int i = 0; i < Level; i++)
                space = space + "    ";

            if (_Debug) System.out.println(space + "Entering deepProjCheck(" + Level + ") with variable: " + bt.getName());
            if (_Debug) System.out.println(space + "Passed Vector:");
            show(uberV, System.out, space);
        }

        Vector localV = new Vector();

        // This algorithm was meant to used with "allofem" set to true. This created
        // a number of problems: If the entire database table is NOT listed in the
        // DDS, ie there are "hidden" variables, then selecting all of the variables
        // in the DDS results in the SQL SELECT statement using the * wildcard and the
        // entire table (including the hidden variables) gets returned, breaking things
        // because of the unexpected returned columns. Bummer!
        // The right fix is to rewrite the code here... The cheap fix is to set
        // "allofem" to false. It never gets set to true, so the list of columns always
        // gets written explicitly, eliminating the wildcard problem. :)  ndp 6/6/01
        //
        // Ok, I set it back to true, and fixed the way it is handled at the higher level.
        //
        boolean allofem = true;

        if (((ServerMethods) bt).isProject()) {
            if (_Debug) System.out.println(space + "Some part of " + bt.getName() + " is projected...");


            if (bt instanceof DConstructor) {

                if (_Debug) System.out.println(space + bt.getName() + " is a DConstructor");


                if (Level > 1) {

                    if (_Debug) System.out.println(space + "At Level " + Level + ", Must add entire object and eval selection after read()...");
                    uberV.add(bt);

                } else {


                    if (_Debug) System.out.println(space + "At Level " + Level + ", searching for projected members...");

                    Enumeration e = ((DConstructor) bt).getVariables();

                    while (e.hasMoreElements()) {
                        BaseType dc_bt = (BaseType) e.nextElement();

                        if (_Debug) System.out.println(space + "Checking DConstructor member: " + dc_bt.getName());
                        allofem = deepProjCheck(dc_bt, localV) && allofem;
                    }

                    if (_Debug) System.out.println(space + "DConstructor \"" + bt.getName() + "\" processed. allofem: " + allofem);
                    if (_Debug) System.out.println(space + "Vector localV: ");
                    if (_Debug) show(localV, System.out, space);


                    if (allofem && Level > 1) {
                        uberV.add(bt);
                    } else {
                        Enumeration bdce = localV.elements();
                        while (bdce.hasMoreElements()) {
                            uberV.add(bdce.nextElement());
                        }
                    }

                    if (_Debug) System.out.println(space + "Vector uberV: ");
                    if (_Debug) show(uberV, System.out, space);
                }

            } else if (bt instanceof DVector) {
                if (_Debug) System.out.println(space + bt.getName() + " is a DVector");

                PrimitiveVector pv = ((DVector) bt).getPrimitiveVector();

                if (pv instanceof BaseTypePrimitiveVector) {

                    allofem = deepProjCheck(pv.getTemplate(), localV);
                    if (allofem) {
                        uberV.add(bt);

                    } else {
                        Enumeration dve = localV.elements();
                        while (dve.hasMoreElements()) {
                            uberV.add(dve.nextElement());
                            if (_Debug) System.out.println(space + "Added Darray member: " + ((String) uberV.lastElement()) + " to uber vector");
                        }
                    }

                } else {
                    uberV.add(bt);
                    if (_Debug) System.out.println(space + "Added Darray member: " + ((String) uberV.lastElement()) + " to uber vector");
                }


            } else {
                if (_Debug) System.out.println(space + bt.getName() + " is a Simple Type");
                if (_Debug) System.out.println(space + "Adding simple type \"" + bt.getName() + "\" to uberV");
                uberV.add(bt);

            }
        } else {
            if (_Debug) System.out.println(space + bt.getName() + " is NOT projected...");

            allofem = false;
        }

        Level--;
        return allofem;
    }


}
