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


/* $Id: SSFunique.java,v 1.5.2.2 2004/07/23 21:41:15 ndp Exp $
*
*/

package dods.servers.sql;

import java.util.*;
//import java.io.*;

//import dods.dap.*;
import dods.dap.Server.*;


/** Server side function for the DRDS. invoking this function in the DODS URL
 * will cause the DRDS to only return those rows of the result that are unique.
 * The SQL keyword DISTINCT is added to the SELECT statement to induce this behaviour.
 * @author Nathan David Potter
 */

public class SSFunique implements SqlBoolFunction {

    /** This methods returns the SQL representation of this function.
     *  If this function cannot produce ansensible SQL representation then
     *  this method should return <code>null</code>.
     *  @param args A list of Clauses containing the arguments specified
     *  for this method in the DODS URL.
     *  @return A String containing the SQL respresentation for this
     *  Server Side Function. If no such representation exisit,
     *  then it shall return <code>null</code>.
     */
    public String getSQLCommand(List args) {
        return (" DISTINCT ");
    }


    /** Evaluates the function using the argument list given.
     * @exception SDODSException Thrown if the function
     *  cannot evaluate successfully. The exact type of exception is up
     *  to the author of the server-side function.
     */
    public boolean evaluate(List args) throws SDODSException {
        return (true);
    }

    /** Returns the name of the server-side function, as it will appear in
     *  constraint expressions. This must be a valid DODS identifier.
     *  All functions must have distinct names.
     */
    public String getName() {
        return ("unique");
    }

    /** Checks that the arguments given are acceptable arguments for this
     * function. This method should only use those attributes of a SubClause
     * which do not change over its lifetime - whether it is constant,
     * what class of SubClause it is, what class of BaseType it returns, etc.
     * Thus, the method should not look at the actual value of an argument
     * unless the argument is flagged as constant.
     *
     * @param args A list of SubClauses that the caller is considering passing
     *             to the evaluate() method of the function.
     * @exception InvalidParameterException Thrown if the function will not
     * evaluate successfully using these arguments.
     */
    public void checkArgs(List args) throws InvalidParameterException {

        // if(!args.isEmpty())
        //   throw new InvalidParameterException("\nThe unique() function does not accept arguments!\n");

    }

}



