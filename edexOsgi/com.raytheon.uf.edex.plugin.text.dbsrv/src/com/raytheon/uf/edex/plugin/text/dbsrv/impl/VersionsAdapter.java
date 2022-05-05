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
package com.raytheon.uf.edex.plugin.text.dbsrv.impl;

import com.raytheon.uf.common.dataplugin.text.dbsrv.ICommandExecutor;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextDBSrvCommandTags;
import com.raytheon.uf.common.dataplugin.text.dbsrv.VersionsTableTags;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * Manipulates version headers on textdbsrv messages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2008  1538       jkorman     Initial creation
 * May 15, 2014 2536       bclement    moved from uf.edex.textdbsrv
 * Aug 22, 2014 2926       bclement    compatibility changes with new textdb service
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class VersionsAdapter implements ICommandExecutor {

    private TextDB textDB;

    /**
     * 
     */
    public VersionsAdapter() {
        textDB = new TextDB();
    }

    /**
     * 
     * @return
     */
    public static final String getViewTag() {
        return "versions";
    }

    /**
     * 
     */
    public void dispose() {

    }

    /**
     * 
     */
    @Override
    public Message execute(Message cmdMessage) {
        Header sHeader = cmdMessage.getHeader();

        // Get the operation code
        String op = sHeader.getProperty(VersionsTableTags.OP.name());

        TextDBSrvCommandTags opTag = TextDBSrvCommandTags.valueOf(op);
        String productId = sHeader.getProperty(VersionsTableTags.PRODID.name());

        if (opTag != null && productId != null) {
            AFOSProductId prodId = new AFOSProductId(productId);
            if (prodId.isFilled()) {
                switch (opTag) {

                case PUT: {
                    String versions = sHeader
                            .getProperty(VersionsTableTags.VERSION.name());

                    addVersionInfo(sHeader, prodId.getCcc(), prodId.getNnn(),
                            prodId.getXxx(), versions);
                    break;
                }

                case GET: {
                    getVersionInfo(sHeader, prodId.getCcc(), prodId.getNnn(),
                            prodId.getXxx());
                    break;
                }
                case DELETE: {
                    deleteVersionInfo(sHeader, prodId.getCcc(),
                            prodId.getNnn(), prodId.getXxx());
                    break;
                }
                default: {
                    String tagName = (opTag != null) ? opTag.name() : "null";
                    Property[] props = new Property[] { new Property(
                            CommandExecutor.STDERR,
                            "ERROR:Invalid command tag = [" + tagName + "]"), };
                    sHeader.setProperties(props);
                    break;
                }
                }

            }

        }
        cmdMessage.setHeader(sHeader);

        return cmdMessage;
    }

    /**
     * 
     * @param state
     * @param xxxId
     * @param cccId
     * @return
     */
    private void addVersionInfo(Header header, String ccc, String nnn,
            String xxx, String versions) {
        Property newProperty = new Property(CommandExecutor.STDERR,
                "NORMAL:Adding productId " + ccc + nnn + xxx + " versions "
                        + versions);
        Property errProperty = new Property(CommandExecutor.STDERR,
                "ERROR:Failure adding to versions table.");

        Property[] props = new Property[] { newProperty, };
        if (!textDB.addVersions(ccc, nnn, xxx, Integer.parseInt(versions))) {
            props = new Property[] { newProperty, errProperty };
        }
        header.setProperties(props);
    }

    /**
     * 
     * @param header
     * @param ccc
     * @param nnn
     * @param xxx
     */
    private void getVersionInfo(Header header, String ccc, String nnn,
            String xxx) {
        String PROP_FMT = CommandExecutor.STDOUT;

        Property[] props = null;
        String vers = textDB.getVersions(ccc, nnn, xxx);

        if (vers != null) {
            props = new Property[] { new Property(PROP_FMT, vers) };
        } else {
            props = new Property[] { new Property(CommandExecutor.STDERR,
                    "ERROR:Failure reading versions table."), };
        }
        header.setProperties(props);
    }

    /**
     * 
     * @param state
     * @param xxxId
     * @param cccId
     * @return
     */
    private void deleteVersionInfo(Header header, String ccc, String nnn,
            String xxx) {
        Property newProperty = new Property(CommandExecutor.STDERR,
                "NORMAL:Deleting product id " + ccc + nnn + xxx
                        + " from versionstable.");
        Property errProperty = new Property(CommandExecutor.STDERR,
                "ERROR:Failure deleting " + ccc + nnn + xxx
                        + " from versionstable.");

        Property[] props = new Property[] { newProperty, };
        if (!textDB.deleteVersions(ccc, nnn, xxx)) {
            props = new Property[] { newProperty, errProperty };
        }
        header.setProperties(props);
    }
}
