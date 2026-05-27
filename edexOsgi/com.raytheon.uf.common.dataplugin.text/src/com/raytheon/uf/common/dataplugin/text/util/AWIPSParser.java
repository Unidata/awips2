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
package com.raytheon.uf.common.dataplugin.text.util;

/**
 * This class used to parse AWIPS commands determining what needs to be done to
 * obtain the command information.
 * 
 * <pre>
 * usage :
 * &lt;code&gt;
 * 
 * AWIPSParser parser = new AWIPSParser(&quot;-3:KOMAMTROMA&quot;);
 * if (parser.isValidCommand()) {
 *     // use parsed data
 * } else {
 *     // complain
 * }
 *                      cmd    -N      A:     ALL    SS.    E:
 *  ---------------     ---    ---     ---    ---    ---    ---
 *  CCCCNNNXXX           x      x       x      x      -      x
 *  0000NNNXXX           x      -       x      -      -      -
 *  ssssNNNXXX                  x       -      x      -      x
 *  
 *  ssssNNN000           x      -       -      -      -      x
 *  NNNXXX...            x      -       x      -      -      -
 *  ssssNNN000           x      -       -      -      -      x
 *  ssssNNN..            -      -       x      -      x      -
 * where 'ssss' is replaced with the ICAO.
 *       '...' is replaced with '000'
 * ************* AWIPS COMMANDS
 *  ALL:CCCCNNNXXX   Read all product versions for CCCCNNNXXX
 *  ALL:NNNXXX       Read all product versions of NNNXXX for local site CCCC
 * ******
 *  A:HH CCCCNNNXXX  Read all versions of products CCCCNNNXXX for last HH hours 
 *                    creates an awipsid(CCCC,NNN,XXX) = (CCCC,NNN,XXX)
 *  A:HH 0000NNNXXX  Read all versions of products NNNXXX at all sites for last HH hours
 *                    creates an awipsid(CCCC,NNN,XXX) = (0000,NNN,XXX)
 *  A:HH CCCCNNN     Read latest version of all products NNN for site;= CCCC and any XXXs from last HH hours
 *                    creates an awipsid(CCCC,NNN,XXX) = (CCCC,NNN,000)
 *  A:HH NNN         Read latest version of all products NNN for local site CCCC and all XXXs for last HH hours
 *                    creates an awipsid(CCCC,NNN,XXX) = (Site,NNN,000)
 *  A:CCCCNNN        Read current-hour version of all products for site CCCC with NNN and any XXXs
 *                    creates an awipsid(CCCC,NNN,XXX) = (CCCC,NNN,000)
 *  A:NNN            Read current-hour version of all products NNN for local site CCCC and all XXXs
 *                    creates an awipsid(CCCC,NNN,XXX) = (Site,NNN,000)
 * ******
 *  -N:CCCCNNNXXX    Read Nth previous version of product forNNNXXX and site CCCC
 *  -N:NNNXXX        Read Nth previous version of product for NNNXXX and local site CCCC
 *  -:CCCCNNNXXX     Read previous version of latest product for site CCCC and product NNNXXX
 *  -:NNNXXX         Read previous version of latest product NNNXXX for local site CCCC
 * ******
 *  SS.NNN           Read latest version of all products for state SS and category NNN.
 *                    creates an awipsid(CCCC,NNN,XXX) = (---,NNN,---) where CCCC and XXX will be
 *                    supplied by a statetable query.
 * ******
 *  NNNXXX           Read latest version of all products for NNNXXX and local site CCCC.
 *                    creates an awipsid(CCCC,NNN,XXX) = (Site,NNN,XXX)
 *  CCCCNNN000       Read latest version of all products for site CCCC and NNN for all XXXs.
 *                    creates an awipsid(CCCC,NNN,XXX) = (CCCC,NNN,000)
 *  NNN000           Read latest version of all products NNN for local site CCCC and all XXXs.
 *                    creates an awipsid(CCCC,NNN,XXX) = (Site,NNN,000)
 *  CCCCNNNXXX       Read latest version of product for site CCCC and product NNNXXX.
 *                    creates an awipsid(CCC,NNN,XXX) = (CCC,NNN,XXX)
 * ******
 *  E:NNNXXX         Automatically open the text editor for returned product and local site CCCC.
 *  E:CCCCNNNXXX     Automatically open the text editor for returned product.
 * 
 * &lt;/code&gt;
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2016 4716       rferrel     Initial creation.
 * Aug 28, 2016 5839       rferrel     Implement IParser
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class AWIPSParser implements IParser {

    private String awipsCommand;

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

    private String site = null;

    private String nnn = null;

    private String xxx = null;

    private boolean enterEditor = false;

    private String localSite = null;

    public AWIPSParser(String awipsCommand, String localSite) {

        if (awipsCommand.startsWith("AWIPS:")) {
            this.awipsCommand = awipsCommand.replaceFirst("AWIPS:", "")
                    .replaceAll(" ", "");
        } else {
            this.awipsCommand = awipsCommand;
        }
        this.localSite = localSite;

        validCommand = parseCommand();

    }

    /**
     * 
     */
    private boolean parseCommand() {

        AWIPSParser.isTemplate = false;

        StringBuilder cmd = new StringBuilder(awipsCommand);

        xxx = "000";

        productId = awipsCommand;

        /*
         * When index string larger then 2 digits (e.g. -100:) will make the
         * length > 16; continue parsing.
         */
        if ((awipsCommand.length() < 5)
                || ((awipsCommand.length() > 16) && !awipsCommand
                        .startsWith("-"))) {
            return false;
        }

        validCommand = true;

        if (awipsCommand.startsWith("ALL:")) {
            cmd.delete(0, 4);
            allVersions = true;
            if (!ccccnnnxxxParse(cmd)) {
                return false;
            }

            // No wildcards allowed.
            validCommand = (!"0000".equals(site) && !"000".equals(nnn) && !"000"
                    .equals(xxx));
            if (site == null) {
                site = localSite;
            }
        } else if (awipsCommand.startsWith("A:")) {
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

            if (!ccccnnnxxxParse(cmd)) {
                return false;
            }

            if (site == null) {
                site = localSite;
            }
        } else if (awipsCommand.startsWith("-")) {
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

            if (!ccccnnnxxxParse(cmd)) {
                return false;
            }

            if (site == null) {
                site = localSite;
            }

        } else if (awipsCommand.startsWith(DRAFT_PIL)) {
            nnn = cmd.substring(0, 3);
            xxx = cmd.substring(3);
            return true;
        } else if (awipsCommand.startsWith("M:")) {
            // Remove the M: from awipsCommand
            cmd.delete(0, 2);
            nnn = MCP_NNN;
            xxx = cmd.toString();
            AWIPSParser.isTemplate = true;
            awipsCommand = nnn.trim() + xxx.trim();
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
                /*
                 * Check for the "enter editor" flag first. This flag should be
                 * removed from the command before requesting data.
                 */
                if (awipsCommand.startsWith("E:")) {
                    cmd.delete(0, 2);
                    awipsCommand = cmd.toString();
                    enterEditor = true;
                }

                if (!ccccnnnxxxParse(cmd)) {
                    return false;
                }

                if (site == null) {
                    site = localSite;
                }

                // Only the "xxx" designator may be a wildcard.
                return (!"0000".equals(site) && !"000".equals(nnn));
            }
        }

        return true;
    }

    /**
     * Parse command for cccc, nnn, and xxx values. Assume any flags and other
     * ancillary information has been determined and we are left with a command
     * in the form of "CCCCNNNXXX"
     * 
     * @param cmd
     * @return true when parse is successful
     * 
     */
    private boolean ccccnnnxxxParse(StringBuilder cmd) {
        int cmdLen = cmd.length();

        if ((cmdLen < 3) || (cmdLen > 10)) {
            return false;
        }

        if (cmdLen > 7) {
            site = cmd.substring(0, 4);
            nnn = cmd.substring(4, 7);
            xxx = cmd.substring(7);
        } else if (cmdLen == 7) {
            site = cmd.substring(0, 4);
            nnn = cmd.substring(4);
        } else if (cmdLen > 3) {
            nnn = cmd.substring(0, 3);
            xxx = cmd.substring(3);
        } else {
            nnn = cmd.toString();
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
     * Is the AWIPS command valid?
     * 
     * @return true the AWIPS command is valid
     */
    public boolean isValidCommand() {
        return validCommand;
    }

    /**
     * @return the awips Command
     */
    public String getAwipsCommand() {
        return awipsCommand.startsWith(DRAFT_PIL) ? site + nnn + xxx
                : awipsCommand;
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
     * @return the cccc
     */
    @Override
    public String getSite() {
        return site;
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
        buffer.append((site == null) ? "----" : site);
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

    @Override
    public String getCcc() {
        return site;
    }
}
