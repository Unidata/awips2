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
package com.raytheon.uf.edex.services.textdbimpl;

import static com.raytheon.edex.textdb.dbapi.impl.TextDB.asciiToHex;
import static com.raytheon.edex.textdb.dbapi.impl.TextDB.getProperty;

import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.edex.services.textdbsrv.ICommandExecutor;
import com.raytheon.uf.edex.services.textdbsrv.TextDBSrvCommandTags;
import com.raytheon.uf.edex.services.textdbsrv.VersionsTableTags;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2008        1538 jkorman     Initial creation
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
        String op = getProperty(sHeader, VersionsTableTags.OP.name());

        TextDBSrvCommandTags opTag = TextDBSrvCommandTags.valueOf(op);
        String productId = getProperty(sHeader, VersionsTableTags.PRODID.name());

        if (opTag != null && productId != null) {
            AFOSProductId prodId = new AFOSProductId(productId);
            if (prodId.isFilled()) {
                switch (opTag) {

                case PUT: {
                    String versions = getProperty(sHeader,
                            VersionsTableTags.VERSION.name());

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
                    Property[] props = new Property[] { new Property("STDERR",
                            asciiToHex("ERROR:Invalid command tag = ["
                                    + tagName + "]")), };
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
        Property newProperty = new Property("STDERR",
                asciiToHex("NORMAL:Adding productId " + ccc + nnn + xxx
                        + " versions " + versions));
        Property errProperty = new Property("STDERR",
                asciiToHex("ERROR:Failure adding to versions table."));

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
        String PROP_FMT = "STDOUT";

        Property[] props = null;
        String vers = textDB.getVersions(ccc, nnn, xxx);

        if (vers != null) {
            props = new Property[] { new Property(PROP_FMT, asciiToHex(vers)) };
        } else {
            props = new Property[] { new Property("STDERR",
                    asciiToHex("ERROR:Failure reading versions table.")), };
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
        Property newProperty = new Property("STDERR",
                asciiToHex("NORMAL:Deleting product id " + ccc + nnn + xxx
                        + " from versionstable."));
        Property errProperty = new Property("STDERR",
                asciiToHex("ERROR:Failure deleting " + ccc + nnn + xxx
                        + " from versionstable."));

        Property[] props = new Property[] { newProperty, };
        if (!textDB.deleteVersions(ccc, nnn, xxx)) {
            props = new Property[] { newProperty, errProperty };
        }
        header.setProperties(props);
    }
}
