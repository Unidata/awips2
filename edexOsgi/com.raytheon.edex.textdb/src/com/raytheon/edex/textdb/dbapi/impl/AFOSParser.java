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
package com.raytheon.edex.textdb.dbapi.impl;

import com.raytheon.uf.common.site.SiteMap;

/**
 * TODO Add Description
 * 
 * <pre>
 * usage :
 * &lt;code&gt;
 * 
 * AFOSParser parser = new AFOSParser(&quot;-3:OMAMTROMA&quot;);
 * if (parser.isValidCommand()) {
 *     // use parsed data
 * } else {
 *     // complain
 * }
 *                      cmd    -N      A:     ALL    SS.    E:  
 *  ---------------     ---    ---     ---    ---    ---    ---
 *  CCCNNNXXX            x      x       x      x      -      x
 *  000NNNXXX            x      -       x      -      -      -
 *  sssNNNXXX                   x       -      x      -      x
 *  
 *  CCCNNN000            x      -       -      -      -      x
 *  CCCNNN...            x      -       x      -      -      -
 *  sssNNN000            x      -       -      -      -      x
 *  sssNNN...            -      -       x      -      x      -
 * where 'sss' is replaced with the site id.
 *       '...' is replaced with '000'
 * ************* AFOS COMMANDS
 *  ALL:CCCNNNXXX   Read all product versions for CCCNNNXXX
 *  ALL:NNNXXX      Read all product versions of NNNXXX for local node CCC
 * ******
 *  A:HH CCCNNNXXX  Read all versions of products CCCNNNXXX for last HH hours 
 *                    creates an afosid(CCC,NNN,XXX) = (CCC,NNN,XXX)
 *  A:HH 000NNNXXX  Read all versions of products NNNXXX at all nodes for last HH hours
 *                    creates an afosid(CCC,NNN,XXX) = (000,NNN,XXX)
 *  A:HH CCCNNN     Read latest version of all products CCCNNN for all sites, xxxs from last HH hours
 *                    creates an afosid(CCC,NNN,XXX) = (CCC,NNN,000)
 *  A:HH NNN        Read latest version of all products NNN for local node CCC and all sites XXXs for last HH hours
 *                    creates an afosid(CCC,NNN,XXX) = (Site,NNN,000)
 *  A:CCCNNN        Read current-hour version of all products CCCNNN for all sites XXXs
 *                    creates an afosid(CCC,NNN,XXX) = (CCC,NNN,000)
 *  A:NNN           Read current-hour version of all products NNN for local node CCC and all sites XXXs
 *                    creates an afosid(CCC,NNN,XXX) = (Site,NNN,000)
 * ******
 *  -N:CCCNNNXXX    Read Nth previous version of product for CCCNNNXXX
 *  -N:NNNXXX       Read Nth previous version of product for NNNXXX and local node CCC
 *  -:CCCNNNXXX     Read previous version of latest product for CCCNNNXXX
 *  -:NNNXXX        Read previous version of latest product NNNXXX for local node CCC
 * ******
 *  SS.NNN          Read latest version of all products for state SS and category NNN.
 *                    creates an afosid(CCC,NNN,XXX) = (---,NNN,---) where CCC and XXX will be
 *                    supplied by a statetable query.
 * ******
 *  NNNXXX          Read latest version of all products for NNNXXX and local node CCC.
 *                    creates an afosid(CCC,NNN,XXX) = (Site,NNN,XXX)
 *  CCCNNN000       Read latest version of all products CCCNNN for all sites XXXs.
 *                    creates an afosid(CCC,NNN,XXX) = (CCC,NNN,000)
 *  NNN000          Read latest version of all products NNN for local node CCC and all sites XXXs.
 *                    creates an afosid(CCC,NNN,XXX) = (Site,NNN,000)
 *  CCCNNNXXX       Read latest version of product for CCCNNNXXX.
 *                    creates an afosid(CCC,NNN,XXX) = (CCC,NNN,XXX)
 * ******
 *  E:NNNXXX        Automatically open the text editor for returned product and local node CCC.
 *  E:CCCNNNXXX     Automatically open the text editor for returned product.
 * 
 * &lt;/code&gt;
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2008            jkorman     Initial creation
 * Mar 30, 2011   8561     J. Ortiz    Added enterEditor flag.
 * Jul 29, 2011  10237     R. Ferrel   parseCommand now properly validates
 *                                     commands with large index.
 * Jan 29, 2014 DR14595 mgamazaychikov Added handling of M: command
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AFOSParser {

    private String afosCommand;

    public static final String DRAFT_PIL = "WRKWG";

    public static final String MCP_NNN = "MCP";

    public static boolean isTemplate = false;

    private String productId;

    private boolean validCommand = false;

    private boolean allVersions = false;

    private boolean pastVers = false;

    private int pastVersNumber = -1;

    private boolean pastHours = false;

    private int pastNumberHours = -1;

    private boolean stateQuery = false;

    private String state = null;

    private String ccc = null;

    private String nnn = null;

    private String xxx = null;

    private boolean enterEditor = false;

    public AFOSParser(String afosCmd, String siteId) {

        afosCommand = afosCmd;

        validCommand = parseCommand(siteId);

    }

    /**
     * 
     */
    private boolean parseCommand(String siteId) {

        AFOSParser.isTemplate = false;

        StringBuilder cmd = new StringBuilder(afosCommand);

        ccc = SiteMap.getInstance().getCCCFromXXXCode(siteId);

        if (ccc == null) {
            ccc = siteId;
        }
        xxx = "000";

        productId = afosCommand;

        // When index string larger then 2 digits (e.g. -100:) will
        // make the length > 13; continue parsing.
        if ((afosCommand.length() < 4)
                || ((afosCommand.length() > 13) && !afosCommand.startsWith("-"))) {
            return false;
        }

        validCommand = true;

        if (afosCommand.startsWith("ALL:")) {
            cmd.delete(0, 4);
            allVersions = true;
            if (cmd.length() > 6) {
                ccc = cmd.substring(0, 3);
                nnn = cmd.substring(3, 6);
                xxx = cmd.substring(6);
            } else if (cmd.length() > 3) {
                ccc = SiteMap.getInstance().getCCCFromXXXCode(siteId);
                if (ccc == null) {
                    ccc = siteId;
                }
                nnn = cmd.substring(0, 3);
                xxx = cmd.substring(3);
            } else {
                return false;
            }
            // No wildcards allowed.
            validCommand = (!"000".equals(ccc) && !"000".equals(nnn) && !"000"
                    .equals(xxx));
        } else if (afosCommand.startsWith("A:")) {
            cmd.delete(0, 2);
            pastHours = true;
            // find the space position
            int i = cmd.indexOf(" ");
            if (i < 0) {
                // no hours, most current version only.
                pastNumberHours = 1;
            } else if (i > 0) {
                // position 0..i is the number of hours
                try {
                    pastNumberHours = Integer.parseInt(cmd.substring(0, i));
                } catch (NumberFormatException e) {
                    return false;
                }

                // remove up to and including space
                if (i >= 0) {
                    cmd.delete(0, i + 1);
                }
            } else {
                return false;
            }

            if (cmd.length() > 9) {
                return false;
            } else if (cmd.length() > 6) {
                ccc = cmd.substring(0, 3);
                nnn = cmd.substring(3, 6);
                xxx = cmd.substring(6);
            } else if (cmd.length() > 3) {
                ccc = cmd.substring(0, 3);
                nnn = cmd.substring(3);
            } else if (cmd.length() == 3) {
                nnn = cmd.toString();
            } else {
                return false;
            }
        } else if (afosCommand.startsWith("-")) {
            pastVers = true;
            cmd.delete(0, 1);
            int i = cmd.indexOf(":");
            if (i == 0) {
                pastVersNumber = 1;
            } else if (i > 0) {
                try {
                    pastVersNumber = Integer.parseInt(cmd.substring(0, i));
                } catch (NumberFormatException e) {
                    return false;
                }
            } else {
                return false;
            }

            // delete up to and including colon
            cmd.delete(0, i + 1);

            if (cmd.length() > 9) {
                return false;
            } else if (cmd.length() > 6) {
                ccc = cmd.substring(0, 3);
                nnn = cmd.substring(3, 6);
                xxx = cmd.substring(6);
            } else if (cmd.length() > 3) {
                nnn = cmd.substring(0, 3);
                xxx = cmd.substring(3);
            } else {
                return false;
            }
        } else if (afosCommand.startsWith(DRAFT_PIL)) {
            nnn = cmd.substring(0, 3);
            xxx = cmd.substring(3);
            return true;
        } else if (afosCommand.startsWith("M:")) {
            // Remove the M: from afosCommand
            cmd.delete(0, 2);
            nnn = MCP_NNN;
            xxx = cmd.toString();
            AFOSParser.isTemplate = true;
            afosCommand = nnn.trim()+xxx.trim();
        } else {
            // Check for SS.NNN request
            int i = cmd.indexOf(".");
            if (i > 0) {
                if (i == 2 && cmd.length() > 3) {
                    state = cmd.substring(0, i);
                    nnn = cmd.substring(i + 1);
                    stateQuery = true;
                } else {
                    return false;
                }
            } else {
                // Check for the "enter editor" flag first.
                // This flag should be removed from the command before
                // requesting data.
                if (afosCommand.startsWith("E:")) {
                    cmd.delete(0, 2);
                    afosCommand = cmd.toString();
                    enterEditor = true;
                }

                // When we get here, any flags and other ancillary information
                // has been determined and we are left with a command in the
                // form of "CCCNNNXXX"
                if (cmd.length() > 9) {
                    return false;
                } else if (cmd.length() > 6) {
                    ccc = cmd.substring(0, 3);
                    nnn = cmd.substring(3, 6);
                    xxx = cmd.substring(6);
                } else if (cmd.length() > 3) {
                    // set ccc to current siteID of command (disabled to attempt
                    // fix)
                    // ccc = siteId;
                    nnn = cmd.substring(0, 3);
                    xxx = cmd.substring(3);
                } else {
                    return false;
                }
                // Only the "xxx" designator may be a wildcard.
                return (!"000".equals(ccc) && !"000".equals(nnn));
            }
        }

        return true;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * Is the AFOS command valid?
     * 
     * @return Is the AFOS command valid?
     */
    public boolean isValidCommand() {
        return validCommand;
    }

    /**
     * @return the afosCommand
     */
    public String getAfosCommand() {
        return afosCommand.startsWith(DRAFT_PIL) ? ccc + nnn + xxx
                : afosCommand;
    }

    /**
     * @return the allVersions
     */
    public boolean isAllVersions() {
        return allVersions;
    }

    /**
     * @return the pastHours
     */
    public boolean isPastHours() {
        return pastHours;
    }

    /**
     * @return the pastNumberHours
     */
    public int getPastNumberHours() {
        return pastNumberHours;
    }

    /**
     * @return the stateQuery
     */
    public boolean isStateQuery() {
        return stateQuery;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @return the ccc
     */
    public String getCcc() {
        return ccc;
    }

    /**
     * @return the nnn
     */
    public String getNnn() {
        return nnn;
    }

    /**
     * @return the xxx
     */
    public String getXxx() {
        return xxx;
    }

    /**
     * @return the pastVers
     */
    public boolean isPastVers() {
        return pastVers;
    }

    /**
     * @return the pastVersNumber
     */
    public int getPastVersNumber() {
        return pastVersNumber;
    }

    public boolean isEnterEditor() {
        return enterEditor;
    }

    /**
     * Get the string representation of this object instance as a StringBuilder.
     * 
     * @return The string representation of this object instance.
     */
    public StringBuilder toString(StringBuilder buffer) {
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        buffer.append("{");
        buffer.append("valid-");
        if (validCommand) {
            buffer.append("T");
        } else {
            buffer.append("F");
        }
        buffer.append(":");
        buffer.append((ccc == null) ? "---" : ccc);
        buffer.append((nnn == null) ? "---" : nnn);
        buffer.append((xxx == null) ? "---" : xxx);
        buffer.append(":");
        if (stateQuery) {
            buffer.append("T-");
            buffer.append(state);
        } else {
            buffer.append("F---");
        }
        buffer.append(":hours:");
        if (pastHours) {
            buffer.append(String.format("T-%02d", pastNumberHours));
        } else {
            buffer.append("F-00");
        }

        buffer.append(":vers:");
        if (pastVers) {
            buffer.append(String.format("T-%02d", pastVersNumber));
        } else {
            buffer.append("F-00");
        }

        buffer.append(":");
        if (allVersions) {
            buffer.append("ALL");
        }
        buffer.append("}");
        return buffer;
    }

    /**
     * Get the string representation of this object instance.
     * 
     * @return The string representation of this object instance.
     */
    public String toString() {
        return toString(null).toString();
    }

    public static final void main(String[] args) {

        String[] testData = { "FSDMTROMA", "000MTROMA", "MTROMA", "DSMMTR000",
                "TOPMTR", "MTR000", "MTR", "ALL:DENMTROMA", "ALL:MTROMA",
                "NE.MTR", "A:000MTROMA", "A:3 LBFMTROMA", "A:2 000MTROMA",
                "A:22 MTROMA", "A:08 MTR", "-3:LBFMTROMA", "-04:000MTROMA",
                "-22:MTROMA", "-5:MTR", "-:LBFMTROMA", "-:000MTROMA",
                "-:MTROMA", "-:MTR", };

        for (String s : testData) {
            AFOSParser parser = new AFOSParser(s, "OMA");
            System.out.println(String.format("%-15s %s", s, parser));
        }

        AFOSParser parser = new AFOSParser("A:OMAMTROMA", "OMA");
        System.out.println(parser.isValidCommand());

    }

}
