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
package com.raytheon.uf.edex.damagepath.handler;

/**
 * Configuration class to store connection information for
 * {@code ExportToLdadHandler} when it uses scp to send damage path data to
 * LDAD.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 09, 2015  #3755     dgilling    Initial creation
 * Mar 10, 2016  #5288     dgilling    Add postProcessCommand field.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ExportToLdadJobConfig {

    private final String user;

    private final String host;

    private final String destinationPath;

    private final String postProcessCommand;

    public ExportToLdadJobConfig(String user, String host,
            String destinationPath, String postProcessCommand) {
        this.user = user.trim();
        this.host = host.trim();
        this.destinationPath = destinationPath.trim();
        this.postProcessCommand = postProcessCommand.trim();
    }

    public String getUser() {
        return user;
    }

    public String getHost() {
        return host;
    }

    public String getDestinationPath() {
        return destinationPath;
    }

    public String getPostProcessCommand() {
        return postProcessCommand;
    }
}
