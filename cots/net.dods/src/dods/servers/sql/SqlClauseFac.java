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


/* $Id: SqlClauseFac.java,v 1.3.2.1 2004/02/04 22:30:29 ndp Exp $
*
*/

package dods.servers.sql;

import dods.dap.Server.*;
import dods.dap.BaseType;
import dods.dap.NoSuchFunctionException;

import java.util.List;


/** Generates Clause objects for the constraint expression parser.
 *  In order to parse constraints using your own custom Clause objects,
 *  pass a customized ClauseFactory, which generates
 *  those clause objects, into the constructor of CEEvaluator. This
 *  particular ClauseFactory is used by the DRDS so that SQL enabled
 *  Clauses are built by the expression parser.
 *  @author ndp
 */
public class SqlClauseFac extends ClauseFactory {

    /** Creates a new clause factory. */
    public SqlClauseFac() {
        super();
    }

    /** Creates a new clause factory.
     * @param functionLibrary The function library that will be used
     * when creating clauses that invoke server-side functions.
     */
    public SqlClauseFac(FunctionLibrary functionLibrary) {
        super(functionLibrary);
    }

    /** Generates a clause which which compares subclauses, using one of the
     *  relative operators supported by the Operator class.
     */
    public TopLevelClause newRelOpClause(int operator,
                                         SubClause lhs,
                                         List rhs)
            throws SDODSException {

        return new SqlRelOpClause(operator, lhs, rhs);
    }

}
