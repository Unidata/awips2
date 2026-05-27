package com.raytheon.uf.edex.backupsvc.service;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Check if a recipient EDEX version is compatible with the specified version or
 * above
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2021 93179      Amanuel Challa      Initial creation
 *
 * </pre>
 */
public class EdexVersionChecker {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EdexVersionChecker.class);

    /*
     * check if a recipient Edex version is compatible major.minor.release.build
     * (ex. 21.4.1.0)
     */

    public EdexVersionChecker() {

    }

    public boolean isCompatibleEdexVersion(String recipientEdexVersion,
            String compatibleEdexVersion) {
        boolean retVal = false;
        if (recipientEdexVersion == null || recipientEdexVersion.isEmpty()) {
            return retVal;
        }

        String theCompatibleEdexVersion[] = compatibleEdexVersion.split("\\."),
                theRecipientEdexVersion[] = recipientEdexVersion.split("\\.");
        int sysMajor, sysMinor, sysRelease, rcpntMajor, rcpntMinor,
                rcpntRelease;
        try {
            sysMajor = Integer.parseInt(theCompatibleEdexVersion[0]);
            sysMinor = Integer.parseInt(theCompatibleEdexVersion[1]);
            sysRelease = Integer.parseInt(theCompatibleEdexVersion[2]);

            rcpntMajor = Integer.parseInt(theRecipientEdexVersion[0]);
            rcpntMinor = Integer.parseInt(theRecipientEdexVersion[1]);
            rcpntRelease = Integer.parseInt(theRecipientEdexVersion[2]);

            if (rcpntMajor > sysMajor) {
                retVal = true;
            } else if (rcpntMajor == sysMajor) {
                if (rcpntMinor > sysMinor) {
                    retVal = true;
                } else if (rcpntMinor == sysMinor) {
                    if (rcpntRelease >= sysRelease) {
                        retVal = true;
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to parse Edex version: " + recipientEdexVersion);
        }
        return retVal;
    }

}
