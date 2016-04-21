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
package com.raytheon.uf.edex.plugin.bufrobs.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import com.raytheon.uf.common.nc.bufr.util.TranslationTableGenerator;

/**
 * Command line utility to generate obs translation table base files. The
 * resulting files will be edited to add mapping values to synoptic tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2014 2906       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BaseTranslationTableGenerator {

    public static final String PREFIX_OPT = "-p";

    public static final String TABLES_OPT = "-t";

    public static final String DEST_DIR_OPT = "-d";

    public static final Set<String> OPTIONS = new HashSet<String>(
            Arrays.asList(PREFIX_OPT, TABLES_OPT, DEST_DIR_OPT));

    public static final String DEFAULT_DEST_DIR = "utility/edex_static/base/bufrobs/tables";

    public static final String ARGDESC = "Arguments: -p [optional file prefix]"
            + " -d [optional destination directory]"
            + " -t [optional comma separated list of table names in 'F XX YYY' format]"
            + " [WMO BUFR Code Table XML file]";

    /**
     * Example arguments:
     * 
     * <pre>
     * -p synoptic_land -d utility/edex_static/base/bufrobs/tables -t "0 10 063,0 20 011,0 20 012,0 20 003" BUFRCREX_21_0_0_CodeFlag_en.xml
     * </pre>
     * 
     * @param args
     * @throws XMLStreamException
     * @throws JAXBException
     * @throws IOException
     */
    public static void main(String[] args) throws XMLStreamException,
            JAXBException, IOException {
        OptArgs optArgs = processArgs(args, OPTIONS);
        if (optArgs.args.size() != 1) {
            System.out.println(ARGDESC);
            System.exit(1);
        }
        String bufrFile = optArgs.args.get(0);
        String dest = optArgs.opts.get(DEST_DIR_OPT);
        if (dest == null) {
            dest = DEFAULT_DEST_DIR;
        }
        Set<String> includedTables;
        String tablesStr = optArgs.opts.get(TABLES_OPT);
        if (tablesStr == null || tablesStr.trim().isEmpty()) {
            includedTables = Collections.emptySet();
        } else {
            includedTables = parseTables(tablesStr);
        }
        String prefix = optArgs.opts.get(PREFIX_OPT);
        if (prefix == null) {
            prefix = "";
        }
        InputStream bufrTables = new FileInputStream(bufrFile);
        try {
            TranslationTableGenerator.generate(prefix, new File(
                    dest), includedTables, true, bufrTables);
        } finally {
            bufrTables.close();
        }
    }

    /**
     * Split string by comma and place in set
     * 
     * @param tablesStr
     * @return
     */
    private static Set<String> parseTables(String tablesStr) {
        Set<String> rval = new HashSet<String>();
        for (String part : tablesStr.split(",")) {
            part = part.trim();
            if (!part.isEmpty()) {
                rval.add(part);
            }
        }
        return rval;
    }

    /*
     * return value container for options map and list of arguments
     */
    private static class OptArgs {
        final Map<String, String> opts;

        final List<String> args;

        public OptArgs(Map<String, String> opts, List<String> args) {
            this.opts = opts;
            this.args = args;
        }
    }

    /**
     * Process list of command line arguments that have option flags with values
     * followed by a possible list of additional arguments
     * 
     * @param args
     *            command line arguments
     * @param options
     *            option flags
     * @return
     */
    public static OptArgs processArgs(String[] args, Set<String> options) {
        Map<String, String> opts = new HashMap<String, String>();
        List<String> leftOver = new ArrayList<String>();
        for (int i = 0; i < args.length; ++i) {
            if (options.contains(args[i])) {
                if (i + 1 > args.length || options.contains(args[i + 1])) {
                    throw new InvalidParameterException("Invalid option: '"
                            + args[i]
                            + "'. All options must be followed by value");
                }
                opts.put(args[i], args[i + 1]);
                i += 1;
            } else {
                leftOver.add(args[i]);
            }
        }
        return new OptArgs(opts, leftOver);
    }

}
