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


/* $Id: sqlDataset.java,v 1.5.2.3 2004/08/26 21:47:49 ndp Exp $
*
*/


package dods.servers.sql;

import java.io.*;
//import java.util.Map;
//import java.util.HashMap;

import dods.dap.*;
import dods.dap.parser.*;
import dods.dap.Server.*;
//import dods.servlet.GuardedDataset;
import dods.servlet.ReqState;


/**
 * This is the DODS Test servlet (dts). It allows the owner of the server
 * to deliver data in ANY valid DDS to a client. This DDS will be
 * filled with invented data if the client requests a DataDDS.
 * This kind of test fixture is useful for evaluating a clients
 * ability to handle the various complexities of the DODS data
 * types.
 *
 * @version $Revision: 1.5.2.3 $
 * @author Nathan David Potter
 */


public class sqlDataset implements GuardedSQLDataset {


    private ReqState rs;

    sqlDataset(ReqState rs) {
        this.rs = rs;
    }


    public void release() {
    } // noop


    /***************************************************************************
     * For the DODS SQL server this method does the following:
     *	<ul>
     *	<li> Makes a newsqlServerFactory (aka BaseTypeFactory) for the
     *        dataset requested.
     *	<li> Instantiates a sqlDDS using the sqlServerFactory and populates
     *        it (this is accomplished by opening a locally cached DDS from a file
     *        and parsing it)
     *	<li> Returns this freshly minted sqlDDS object (to the servlet code
     *        where it is used.)
     *	</ul>
     *
     *
     * @return The <code>sqlDDS</code> for the named data set.
     *
     * @see dods.dap.Server.ServerDDS
     * @see dods.servers.sql.sqlServerFactory
     * @see dods.servers.test.test_ServerFactory
     */
    public sqlDDS getSQLDDS() throws DODSException, ParseException {

        sqlDDS myDDS = null;
        DataInputStream ddsSource = null;

        try {

            // Add the unique function support to the DDS
            //Map functionMap = new HashMap();
            //functionMap.put("unique", new UniqueFunction());

            // Get your class factory and instantiate the DDS
            sqlServerFactory sfactory = new sqlServerFactory();
            myDDS = new sqlDDS(rs.getDataSet(), sfactory);

            // Open the Cached DDS for this dataset
            ddsSource = openCachedDDS(rs);

            // Parse the cached DDS and in the process build your data structures
            myDDS.parse(ddsSource);

        } finally {

            try {
// Close the cached DDS
                if (ddsSource != null)
                    ddsSource.close();
            } catch (IOException ioe) {

                throw new DODSException(DODSException.UNKNOWN_ERROR, ioe.getMessage());
            }
        }

        return (myDDS);


    }
    /***************************************************************************/






    /***************************************************************************
     * For the test server this method does the following:
     *	<ul>
     *	<li> Makes a new test_ServerFactory (aka BaseTypeFactory) for the
     *        dataset requested.
     *	<li> Instantiates a ServerDDS using the test_ServerFactory and populates
     *        it (this is accomplished by opening a locally cached DDS from a file
     *        and parsing it)
     *	<li> Returns this freshly minted ServerDDS object (to the servlet code
     *        where it is used.)
     *	</ul>
     *
     *
     * @return The <code>ServerDDS</code> for the named data set.
     *
     * @see dods.dap.Server.ServerDDS
     * @see dods.servers.test.test_ServerFactory
     */
    public ServerDDS getDDS() throws DODSException, ParseException {
        return (getSQLDDS());

    }
    /***************************************************************************/



    /***************************************************************************
     * Opens a DDS cached on local disk. This can be used on DODS servers (such
     * as the DODS SQL Server) that rely on locally cached DDS files as opposed
     * to dynamically generated DDS's.
     *
     * <p>This method uses the <code>iniFile</code> object cached by <code>
     * loadIniFile()</code> to determine where to look for the cached <code>
     * DDS</code>.
     *
     * @param rs The ReqState object for this client request.
     *
     * @return An open <code>DataInputStream</code> from which the DDS can
     * be read.
     *
     * @exception DODSException
     *
     * @see ReqState
     */
    public DataInputStream openCachedDDS(ReqState rs) throws DODSException {


        String cacheDir = rs.getInitParameter("DDScache");

        if (cacheDir == null)
            cacheDir = rs.getDDSCache();


        try {

            // go get a file stream that points to the requested DDSfile.

            File fin = new File(cacheDir + rs.getDataSet());
            FileInputStream fp_in = new FileInputStream(fin);
            DataInputStream dds_source = new DataInputStream(fp_in);

            return (dds_source);
        } catch (FileNotFoundException fnfe) {
            throw new DODSException(DODSException.CANNOT_READ_FILE, fnfe.getMessage());
        }


    }
    /***************************************************************************/



    /***************************************************************************
     *
     * In this (default) implementation of the getDAS() method a locally cached
     * DAS is retrieved and parsed. In this method the DAS for the passed dataset
     * is loaded from the "das_cache_dir" indidcated in the "[Server]" section of the
     * DODSiniFile. If the there is no file available a DODSException is
     * thrown. It is certainly possible (and possibly very desirable) to override
     * this method when overriding the getDDS() method. One reason for doing this
     * is if the DODS server being implemented can generate the DAS information
     * dynamically.
     *
     * When overriding this method be sure that it does the following:
     *	<ul>
     *	<li> Instantiates the DAS for the indicated (passed) dataset and
     *        populates it. This is accomplished in the default implementation
     *        by opening a (cached?) DAS stored in a file and parsing it. In
     *        a different implementation it could be created dynamically.
     *   <li> Returns this freshly minted DAS object. (to the servlet code where it is used.)
     *   </ul>
     *
     *
     *
     * @return The DAS object for the data set specified in the parameter <code>dataSet</code>
     *
     * @see dods.dap.DAS
     */
    public DAS getDAS() throws DODSException, ParseException {

        DataInputStream is = null;
        DAS myDAS = new DAS();
        boolean gotIt = false;

        try {
            is = openCachedDAS(rs);

            myDAS.parse(is);
            gotIt = true;
        } catch (FileNotFoundException fnfe) {
            // This is no big deal. We just trap it and return an
            // empty DAS object.
            gotIt = false;
        } finally {
            try {
                if (is != null) is.close();
            } catch (IOException ioe) {

                throw new DODSException(DODSException.UNKNOWN_ERROR, ioe.getMessage());
            }
        }

        if (gotIt)
            System.out.println("Successfully opened and parsed DAS cache: " + rs.getDataSet());
        else
            System.out.println("No DAS present for dataset: " + rs.getDataSet());

        return (myDAS);

    }
    /***************************************************************************/





    /***************************************************************************
     * Opens a DAS cached on local disk. This can be used on DODS servers (such
     * as the DODS SQL Server) that rely on locally cached DAS files as opposed
     * to dynamically generated DAS's.
     *
     * <p>This method uses the <code>iniFile</code> object cached by <code>
     * loadIniFile()</code> to determine where to look for the cached <code>
     * DDS</code>.
     *
     * <p>If the DAS cannot be found an error is sent back to the client.
     *
     * @param rs The ReqState object for this client request.
     *
     * @return An open <code>DataInputStream</code> from which the DAS can
     * be read.
     *
     * @exception FileNotFoundException
     *
     */
    public DataInputStream openCachedDAS(ReqState rs) throws FileNotFoundException {


        String cacheDir = rs.getInitParameter("DAScache");

        if (cacheDir == null)
            cacheDir = rs.getDASCache();

        // go get a file stream that points to the requested DASfile.
        File fin = new File(cacheDir + rs.getDataSet());
        FileInputStream fp_in = new FileInputStream(fin);
        DataInputStream das_source = new DataInputStream(fp_in);
        return (das_source);


    }
    /***************************************************************************/


}



