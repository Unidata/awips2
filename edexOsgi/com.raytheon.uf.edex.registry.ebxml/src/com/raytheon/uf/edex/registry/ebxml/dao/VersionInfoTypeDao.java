/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Data access object for handling VersionInfoType objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2012 #363       bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class VersionInfoTypeDao extends RegistryDao {

    /**
     * Creates a new VersionInfoTypeDao
     */
    public VersionInfoTypeDao() {
        super(VersionInfoType.class);
    }

    /**
     * Gets a version object with the given versionName
     * 
     * @param versionName
     *            The version name to query for
     * @return The version object with the given version name
     * @throws EbxmlRegistryException
     *             If errors occur during the query process
     */
    public VersionInfoType getByVersionName(String versionName)
            throws EbxmlRegistryException {
        return getResult(this
                .executeHQLQuery("from VersionInfoType x where x.versionName='"
                        + versionName + "'"));

    }

    /**
     * Gets a version object with the given userVersionName
     * 
     * @param userVersionName
     *            The userVersionName name to query for
     * @return The version object with the given userVersionName
     * @throws EbxmlRegistryException
     *             If errors occur during the query process
     */
    public VersionInfoType getByUserVersionName(String userVersionName)
            throws EbxmlRegistryException {
        return getResult(this
                .executeHQLQuery("from VersionInfoType x where x.userVersionName='"
                        + userVersionName + "'"));
    }

    /**
     * Gets a version object with the given versionName and userVersionName
     * 
     * @param versionName
     *            The version name to query for
     * @param userVersionName
     *            The userVersionName name to query for
     * @return The version object with the given userVersionName
     * @throws EbxmlRegistryException
     *             If errors occur during the query process
     */
    public VersionInfoType getVersion(String versionName, String userVersionName)
            throws EbxmlRegistryException {
        return getResult(this
                .executeHQLQuery("from VersionInfoType x where x.userVersionName='"
                        + userVersionName
                        + "' and x.versionName='"
                        + versionName + "'"));
    }

    /**
     * Gets a version object with the given version number
     * 
     * @param version
     *            The version number to query upon
     * @return The version object with the given version number
     * @throws EbxmlRegistryException
     */
    public VersionInfoType getByVersionNumber(int version)
            throws EbxmlRegistryException {
        return getResult(this
                .executeHQLQuery("from VersionInfoType x where x.versionNumber="
                        + version));
    }

    /**
     * Checks the database to see if a version object matching the given version
     * object already exists
     * 
     * @param versionInfo
     *            The version info to check
     * @return The persistent version if exists, else the provided object
     * @throws EbxmlRegistryException
     *             If errors occur during the query process
     */
    public VersionInfoType sync(VersionInfoType versionInfo)
            throws EbxmlRegistryException {
        VersionInfoType dbVersion = getVersion(versionInfo.getVersionName(),
                versionInfo.getUserVersionName());
        if (dbVersion == null) {
            this.save(versionInfo);
            dbVersion = getVersion(versionInfo.getVersionName(),
                    versionInfo.getUserVersionName());
        }
        return dbVersion;
    }

    /**
     * Gets a single result from a list of results
     * 
     * @param results
     *            The result list to check
     * @return null if the provided list is empty, else returns the first
     *         element of the list
     */
    private VersionInfoType getResult(List<Object> results) {
        if (results.isEmpty()) {
            return null;
        } else {
            return (VersionInfoType) results.get(0);
        }
    }
}
