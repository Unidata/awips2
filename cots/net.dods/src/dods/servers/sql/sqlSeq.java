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

import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Enumeration;

import dods.dap.BaseType;
import dods.dap.DDS;
import dods.dap.NoSuchVariableException;
import dods.dap.Server.CEEvaluator;
import dods.dap.Server.SDODSException;
import dods.dap.Server.SDSequence;
import dods.dap.Server.ServerDDS;
import dods.dap.Server.ServerMethods;

/**
 * Holds a DODS Server <code>Sequence</code> value.
 * 
 * @version $Revision: 1.3.4.1 $
 * @author ndp
 * @see BaseType
 */
public class sqlSeq extends SDSequence {

    private static final boolean _Debug = false;

    private int rowCount = 0;

    /** Constructs a new <code>test_SDSequence</code>. */
    public sqlSeq() {
        super();
    }

    /**
     * Constructs a new <code>test_SDSequence</code> with name <code>n</code>.
     * 
     * @param n
     *            the name of the variable.
     */
    public sqlSeq(String n) {
        super(n);
    }

    // --------------- FileIO Interface

    /**
     * Read a value from the named dataset for this variable. This
     * implementation is intended to be reading data from a JDBC connection to a
     * relational database.
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
     * This Sequence read() method handles "rewinding" the currentColumn index
     * in the sqlResponse object to insure that for as long as there is more
     * data to be read into this Sequence that the currentColumn is set
     * correctly for each invocation of this method. In addition, this method
     * handles the task of moving the ResultSet object in the sqlResponse
     * through the rows of the response data, insuring that at each invocation
     * of a top level sequence read() invocation the ResultSet is set up for a
     * new row of data.
     * 
     * @param datasetName
     *            String identifying the file or other data store from which to
     *            read a vaue for this variable.
     * @param specialO
     *            This <code>Object</code> is used by this method. It is assumed
     *            to be of type sqlResponse, a container for the ResultSet and
     *            the index value of next column to evaluate.
     * @return <code>true</code> if more data remains to be read, otherwise
     *         <code>false</code>.
     * @exception NoSuchVariableException
     * @exception IOException
     * @exception EOFException
     */
    public boolean read(String datasetName, Object specialO)
            throws NoSuchVariableException, IOException, EOFException {

        boolean retVal, addRow = false;
        sqlResponse res = (sqlResponse) specialO;

        // Cache the current column. We need to do this so that: As this
        // Sequence is serialized it moves through the ResultSet row by row
        // reading some number of consecutive columns from each row.
        // In order for the column counter to be correctly positioned
        // at the begining of each call to read() we need to cache that
        // first currentColumn and use it to restore the currentColumn pointer
        // at the end of read()
        int startColumn = res.getCurrentColumn();

        if (_Debug)
            System.out.println("\nReading row " + rowCount + " of Sequence \""
                    + getName() + "\" from " + datasetName + ":");

        Enumeration enumi = this.getVariables();

        while (enumi.hasMoreElements()) {
            ServerMethods sm = (ServerMethods) enumi.nextElement();
            // System.out.println("Reading variable: "+((BaseType)sm).getName());
            if (sm.isProject()) {
                sm.read(datasetName, specialO);
                // ((BaseType)sm).printVal(System.out,"   ");
            }
        }

        boolean moreToRead = false;

        if (_Debug)
            System.out.println("Sequence " + getName() + " startColumn: "
                    + startColumn + "  currentColumn: "
                    + res.getCurrentColumn());

        // Check and make sure not to move to the next row of results if we
        // aren't at the highest
        // level...
        if (getLevel() == 0) {
            try {
                // Try to get next row.
                moreToRead = res.getResultSet().next();
            } catch (SQLException sqle) {

                throw new IOException(sqle.toString());
            }
        }

        // Artifcially limit the response evaluation to _maxRows
        rowCount++;
        if (rowCount >= res.getMaxRows()) {
            moreToRead = false;
        }

        setRead(true);

        // If there are more rows left in the ResultSet (thus
        // more data to read in this Sequence) reposition the
        // currentColumn pointer so that it's at the begining
        // of the rows relevent for this Sequence when the
        // flow of control re-enters this read() method.
        if (moreToRead) {
            if (_Debug)
                System.out.println("Resetting Current Column to: "
                        + startColumn);
            res.setCurrentColumn(startColumn);
        }

        // System.out.println("Read finished. Returning");

        return (moreToRead);
    }

    /**
     * Server-side serialization for DODS variables (sub-classes of
     * <code>BaseType</code>). We override the serialize() method of the parent
     * SD<i>Class</i> in order to stop the evaluation of the CEEvaluator's
     * Clauses, as this has been handled implicitly by the SQL Database that
     * this Server is designed to interrogate.
     * 
     * @param sink
     *            a <code>DataOutputStream</code> to write to.
     * @exception IOException
     *                thrown on any <code>OutputStream</code> exception.
     * @see BaseType
     * @see DDS
     * @see ServerDDS
     */
    public void serialize(String dataset, DataOutputStream sink,
            CEEvaluator ce, Object specialO) throws NoSuchVariableException,
            SDODSException, IOException {

        boolean moreToRead = true;

        while (moreToRead) {

            if (!isRead()) {
                // ************* Pulled out the getLevel() check in order to
                // support the "new"
                // and "improved" serialization of dods sequences. 8/31/01 ndp
                // if(getLevel() != 0 ) // Read only the outermost level
                // return;
                moreToRead = read(dataset, specialO);
            }

            // ************* Pulled out the getLevel() check in order to support
            // the "new"
            // and "improved" serialization of dods sequences. 8/31/01 ndp
            // if(getLevel() == 0){
            writeMarker(sink, START_OF_INSTANCE);
            // }

            for (Enumeration e = varTemplate.elements(); e.hasMoreElements();) {
                ServerMethods sm = (ServerMethods) e.nextElement();
                if (sm.isProject()) {
                    if (_Debug)
                        System.out.println("Sending variable: "
                                + ((BaseType) sm).getName());
                    sm.serialize(dataset, sink, ce, specialO);
                }
            }

            if (moreToRead)
                setAllReadFlags(false);
        }

        // ************* Pulled out the getLevel() check in order to support the
        // "new"
        // and "improved" serialization of dods sequences. 8/31/01 ndp
        // if(getLevel() == 0){
        writeMarker(sink, END_OF_SEQUENCE);
        // }

        return;
    }

}
