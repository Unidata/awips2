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






/* $Id: drds.java,v 1.7.2.3 2004/08/26 21:47:49 ndp Exp $
*
*/

package dods.servers.sql;

import java.io.*;
import java.util.*;
//import dods.servlet.DODSServlet;

import dods.servlet.ReqState;
import dods.dap.*;
import dods.dap.parser.ParseException;


/**
 * This servlet, the DODS Relational Database Server (drds),
 * uses the generic implementation of the DODS
 * SQL server types. This implementation allows this servlet
 * to deliver all JDBC data types with the exception of the
 * BLOB (Binary Large Object) types such as BINARY, VARBINARY
 * and LONGVARBINARY. This implies that when serving data
 * from a DBMS containing tables that use the cooresponding
 * types for that DBMS (such as the IMAGE type in Microsofts
 * SQL Server 7.0 product) this servlet will NOT be delivering
 * that information. If the owner of the DODS server installation
 * wishes to server these types of data they will have to extend
 * the implementations of the DODS Server types found in the
 * dods.dap.Server.sql package to do so. Have Fun :)
 *
 * <p>
 * <b>Configuration:</b><br>
 * The DODSServlet relies on the javax.servlet.ServletConfig
 * interface (in particular the getInitParameter() method)
 * to retrieve configuration information used by the servlet.
 * <b>InitParameters:</b>
 *
 *<ul>
 *   <li>
 *       DebugOn - This controls ouput to the terminal from which
 *       the servlet engine was launched. The value is a list of
 *       flags that turn on debugging instrumentation in different
 *	parts of the code. Values are:
 *       <ul>
 *           <li>showRequest - Show information about the clients request. </li>
 *           <li>showResponse - Show information about the servlets response.</li>
 *           <li>probeRequest - Show an exhaustive amount of information about
 *                              the clients request object.</li>
 *           <li>JDBC - Show JDBC debugging information.</li>
 *       </ul>
 *   </li>
 *
 *   <li>
 *       INFOcache - This is should be set to the directory containing the
 *	files used by the ".info" service for the servlet. This directory
 *	should contain any dataset specific "over-ride" files (see below),
 *	any dataset specific additional information files (see below), and any
 *	servlet specific information files(see below).
 *   </li><br>
 *
 *   <li>
 *       DDScache - This is should be set to the directory containing the DDS
 *	files for the datasets used by the servlet. Some servlets have been
 *	developed that do not use DDS's that are cached on the disk, however
 *	the default behaviour is for the servlet to load DDS images from disk.
 *   </li><br>
 *
 *   <li>
 *       DAScache - This is should be set to the directory containing the DAS
 *	files for the datasets used by the servlet. Some servlets have been
 *	developed that do not use DAS's that are cached on the disk, however
 *	the default behaviour is for the servlet to load DAS images from disk.
 *   </li><br>
 *
 *        JDBCdriver - This should be set to the java CLASSPATH name of the JDBC
 *	drivers that the servlet is to use to make the JDBC connection to the
 *	DBMS. For example, in my server that is using the Merant Data Direct
 *	drivers the value is set to: "com.merant.sequelink.jdbc.SequeLinkDriver"
 *   </li><br>
 *
 *   <li>
 *
 *        JDBCconnectionURL - This is the connection URL (aka the connection string)
 *	that the DRDS is to use to connecto to the DBMS. This is usually defined
 *	by the devlopers of the JDBC driver. It is not always easy to ascertain for
 *	a particular installation. For example, in my server that is using the Merant Data Direct
 *	drivers the value is set to: "jdbc:sequelink://sugar.oce.orst.edu:19996"
 *   </li><br>
 *
 *   <li>
 *        JDBCusername - This is the user name for the DBMS that the JDBC connection
 *	will be made under. This is often set to "guest".
 *   </li><br>
 *
 *   <li>
 *        JDBCpassword - The password associated with the above username. This is stored
 *	as simple text, so make sure that the JDBC user doesn't have any significant
 *	privileges!
 *   </li><br>
 *
 *   <li>
 *        JDBCMaxResponseLength - This limits the number of lines that the DRDS will
 *	for a given client request. For debugging I use 300. For production I use 300000.
 *   </li><br>
 *
 *   <li>
 *       UseDatasetName - This is a (probably temporary) hack. Some databases (MS-SQL
 *	Server 7.0) require that the database name and the owner of the database be
 *	specified in every variable and table name. This is akward for the current
 *	implmentation of the DRDS. The work around is to name the dataset (in the DDS file)
 *	with the database name and owner name of the table being served. For example in
 *	one dataset that I server the database name is "EOSDB" and the owner of the
 *	database is "DBO". For this database I set the value of UseDatasetName to true
 *	and then I define the DDS as follows:
 *<pre>
 *            Dataset {
 *                Sequence {
 *                    Float64 battery;
 *                    Float64 checksum;
 *                    Float64 data_age;
 *                } Drifters;
 *            } EOSDB.DBO;
 *
 *</pre>
 *        Thus the hack is invoked. If you don't want to use this hack then DO NOT
 *	even included the InitParameter "UseDatasetName" in the web.xml entry for
 *	the DRDS.
 *
 *   </li>
 *
 *   </ul>
 *
 *	Here is an example entry from the web.xml file (for tomcat3.3a) associated
 *	with a DRDS servlet:
 *
 *<pre>
 *        &lt;servlet&gt;
 *            &lt;servlet-name&gt;
 *                drds
 *            &lt;/servlet-name&gt;
 *
 *            &lt;servlet-class&gt;
 *                dods.servers.sql.drds
 *            &lt;/servlet-class&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;JDBCdriver&lt;/param-name&gt;
 *                &lt;param-value&gt; com.merant.sequelink.jdbc.SequeLinkDriver&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;JDBCconnectionURL&lt;/param-name&gt;
 *                &lt;param-value&gt;jdbc:sequelink://sugar:19996&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;JDBCusername&lt;/param-name&gt;
 *                &lt;param-value&gt;guest&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;JDBCpassword&lt;/param-name&gt;
 *                &lt;param-value&gt;&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;JDBCMaxResponseLength&lt;/param-name&gt;
 *                &lt;param-value&gt;300&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;UseDatasetName&lt;/param-name&gt;
 *                &lt;param-value&gt;&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;INFOcache&lt;/param-name&gt;
 *                &lt;param-value&gt;/usr/Java-DODS/sdds-testsuite/info/&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;DDScache&lt;/param-name&gt;
 *                &lt;param-value&gt;/usr/Java-DODS/sdds-testsuite/dds/&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;DAScache&lt;/param-name&gt;
 *                &lt;param-value&gt;/usr/Java-DODS/sdds-testsuite/das/&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *            &lt;init-param&gt;
 *                &lt;param-name&gt;DebugOn&lt;/param-name&gt;
 *                &lt;param-value&gt;showRequest showResponse JDBC&lt;/param-value&gt;
 *            &lt;/init-param&gt;
 *
 *        &lt;/servlet&gt;
 *
 *</pre>
 *
 * @version $Revision: 1.7.2.3 $
 * @author Nathan David Potter
 */

public class drds extends dodsSQLServlet {


    /***************************************************************************
     * DODS Server Version for the test server...
     *
     * @serial Ostensibly it's a serializable item...
     */
    private String ServerVersion = "DODS/3.2";


    //private String LocalDDSPath = "/home/carbon/ndp/DODS/cvs/Java-DODS/sdds-testsuite/dds/";


    /***************************************************************************
     * This method returns a String containing the DODS Server Version...
     */
    public String getServerVersion() {
        return (ServerVersion);
    }
    /***************************************************************************/



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
     * @param dataSet The name of the data set requested.
     *
     * @returns The <code>sqlDDS</code> for the named data set.
     *
     * @see dods.dap.Server.ServerDDS
     * @see dods.servers.sql.sqlServerFactory
     * @see dods.servers.test.test_ServerFactory
     */
    protected GuardedSQLDataset getSQLDataset(ReqState rs) throws DODSException, IOException, ParseException {
        return new sqlDataset(rs);
    }
    /***************************************************************************/


}



