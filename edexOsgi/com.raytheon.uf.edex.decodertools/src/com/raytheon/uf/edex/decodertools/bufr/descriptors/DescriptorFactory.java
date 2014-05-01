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
package com.raytheon.uf.edex.decodertools.bufr.descriptors;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080214            862 jkorman     BUFRMOS implementation changes.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DescriptorFactory {

    private Log logger = LogFactory.getLog(getClass());

    private static Pattern DESC_PATTERN = Pattern
            .compile("[0-3] \\d{2} \\d{3}");

    private static final String DEFAULT_TABLE_B = "/res/bufrtables/BUFR_Table_B.txt";

    private static final String DEFAULT_TABLE_D = "/res/bufrtables/BUFR_Table_D_1.txt";

    // private static DescriptorFactory factoryInstance = null;

    private HashMap<Integer, BUFRTableB> tableBEntries = new HashMap<Integer, BUFRTableB>();

    private HashMap<Integer, BUFRTableD> tableDEntries = null;

    /**
     * 
     * @param resourceClassReference
     */
    public DescriptorFactory() {
        this(DescriptorFactory.class, null, null);
    }

    /**
     * 
     * @param resourceClassReference
     * @param tableB
     * @param tableD
     */
    public DescriptorFactory(Class<?> resourceClassReference, String tableB,
            String tableD) {
        logger.info("Loading tableB " + tableB);
        populateTableB(resourceClassReference, tableB);
        logger.info("Loading tableD " + tableD);
        populateTableD(resourceClassReference, tableD);
    }

    /**
     * 
     * @param resourceClassReference
     * @param tableB
     */
    private void populateTableB(Class<?> resourceClassReference, String tableB) {
        InputStream strm = null;

        BufferedReader bf = null;
        try {
            strm = this.getClass().getResourceAsStream(DEFAULT_TABLE_B);
            int count = 0;
            if (strm != null) {
                bf = new BufferedReader(new InputStreamReader(strm));

                String line = null;
                while ((line = bf.readLine()) != null) {

                    BUFRTableB entry = BUFRTableB.createEntry(line);
                    if (entry != null) {
                        count++;
                        logger.debug("Source = " + tableB + " " + entry);
                        tableBEntries.put(entry.getDescriptor(), entry);
                    }
                }
            }
            logger.debug(count + " entries entered from " + DEFAULT_TABLE_B);
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }

        if (tableB != null) {
            tableB = "/res/bufrtables/" + tableB;
            bf = null;
            try {
                strm = resourceClassReference.getResourceAsStream(tableB);
                int count = 0;
                if (strm != null) {
                    bf = new BufferedReader(new InputStreamReader(strm));

                    String line = null;
                    while ((line = bf.readLine()) != null) {

                        BUFRTableB entry = BUFRTableB.createEntry(line);
                        if (entry != null) {
                            count++;
                            logger.debug("Source = " + tableB + " " + entry);
                            tableBEntries.put(entry.getDescriptor(), entry);
                        }
                    }
                } else {
                    logger.error("Could not get stream to " + tableB);
                }
                logger.debug(count + " entries entered from " + tableB);
            } catch (IOException ioe) {
                ioe.printStackTrace();
            } finally {
                if (bf != null) {
                    try {
                        bf.close();
                    } catch (IOException ioe) {
                        ioe.printStackTrace();
                    }
                }
            }
        }
    }

    /**
     * 
     * @param resourceClassReference
     * @param tableD
     */
    private void populateTableD(Class<?> resourceClassReference, String tableD) {
        if (tableDEntries == null) {
            tableDEntries = new HashMap<Integer, BUFRTableD>();
        }
        readTableD(resourceClassReference, DEFAULT_TABLE_D, false);
        if (tableD != null) {
            readTableD(resourceClassReference, "/res/bufrtables/" + tableD, true);
        }
    }

    /**
     * 
     * @param resourceClassReference
     * @param tableD
     */
    private void readTableD(Class<?> resourceClassReference, String tableD, boolean allowOverwrites) {
        InputStream strm = null;
        BufferedReader bf = null;

        try {
            String line = null;
            BUFRDescriptor tableDInstance = null;
            strm = resourceClassReference.getResourceAsStream(tableD);
            if (strm != null) {
                bf = new BufferedReader(new InputStreamReader(strm));

                while ((line = bf.readLine()) != null) {
                    if (line.length() > 1) {
                        logger.debug("Processing [" + line + "]");
                        switch (line.charAt(0)) {

                        case ' ': {
                            if(tableDInstance != null) {
                                BUFRDescriptor desc = getDescriptor(line);
                                if (desc != null) {
                                    if(tableDInstance.isDefined()) {
                                        logger.debug("Attempting to add [" + line + "] to a defined descriptor");
                                    } else {
                                        logger.debug("Adding [" + desc + "] to [" + tableDInstance + "]");
                                        tableDInstance.addDescriptor(desc);
                                    }
                                }
                            }
                            break;
                        }
                        case '3': {
                            if(tableDInstance != null) {
                                // we were defining an entry
                                tableDInstance.setDefined(true);
                            }
                            tableDInstance = null;
                            
                            Matcher m = DESC_PATTERN.matcher(line);
                            if (m.find()) {
                                String s = line.substring(m.start(),
                                        m.end()).trim();
                                tableDInstance = getDescriptor(s);
                                if(tableDInstance.isDefined() && allowOverwrites) {
                                    tableDInstance.setSubList(new ArrayList<BUFRDescriptor>());
                                }
                            } else {
                                logger
                                        .debug("Could not create descriptor for ["
                                                + line + "]");
                            }
                            break;
                        }
                        default: {

                            break;
                        }
                        }
                    }
                }

                bf.close();
            } else {
                logger.error("Reading table D : Could not get stream to "
                        + tableD);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    public synchronized BUFRDescriptor getDescriptor(int descriptor) {
        BUFRDescriptor descInstance = null;
        int desciptor_f = (descriptor & 0xC000) >> 14;
        switch (desciptor_f) {
        case 0: {
            descInstance = queryTableB(descriptor);
            break;
        }
        case 1: {
            descInstance = new BUFRReplicationDescriptor(descriptor);
            break;
        }
        case 2: {
            descInstance = new BUFRTableC(descriptor);
            break;
        }
        case 3: {
            descInstance = queryTableD(descriptor);
            if (descInstance == null) {
                // Not currently defined, create an undefined descriptor
                BUFRTableD d = new BUFRTableD(descriptor);
                // and put the into the table as a forward reference.
                tableDEntries.put(d.getDescriptor(), d);
                descInstance = d;
            }
            break;
        }
        }
        return descInstance;
    }

    /**
     * 
     * @param f
     * @param x
     * @param y
     * @return
     */
    public synchronized BUFRDescriptor getDescriptor(int f, int x, int y) {
        return getDescriptor((f << 14) | (x << 8) | y);
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    public BUFRDescriptor getDescriptor(String descriptor) {
        BUFRDescriptor descInstance = null;
        synchronized (DESC_PATTERN) {
            Matcher m = DESC_PATTERN.matcher(descriptor);
            if (m.find()) {

                int f = Integer.parseInt(descriptor.substring(m.start(),
                        m.start() + 1).trim());
                int x = Integer.parseInt(descriptor.substring(m.start() + 2,
                        m.start() + 4).trim());
                int y = Integer.parseInt(descriptor.substring(m.start() + 5,
                        m.start() + 8).trim());

                descInstance = getDescriptor(f, x, y);
            }
        }

        return descInstance;
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    public synchronized BUFRTableB queryTableB(Integer descriptor) {
        return tableBEntries.get(descriptor);
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    public synchronized BUFRTableD queryTableD(Integer descriptor) {
        return tableDEntries.get(descriptor);
    }

    public List<BUFRTableB> getTableB() {
        ArrayList<BUFRTableB> table = new ArrayList<BUFRTableB>();
        table.addAll(tableBEntries.values());
        return table;
    }

    public List<BUFRTableD> getTableD() {
        ArrayList<BUFRTableD> table = new ArrayList<BUFRTableD>();
        table.addAll(tableDEntries.values());
        return table;
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    public synchronized BUFRTableD addToTableD(BUFRTableD descriptor) {
        BUFRTableD desc = tableDEntries.put(descriptor.getDescriptor(),
                descriptor);
        return desc;
    }

    // public static synchronized DescriptorFactory getInstance() {
    // if (factoryInstance == null) {
    // factoryInstance = new DescriptorFactory();
    // }
    // return factoryInstance;
    // }
    //
    // public static synchronized DescriptorFactory getInstance(String tableB,
    // String tableD) {
    // if (factoryInstance == null) {
    // factoryInstance = new DescriptorFactory();
    // }
    // return factoryInstance;
    // }

    
    /**
     * 
     * @param descriptor
     * @param indent
     */
    private static void displayDescriptor(BUFRDescriptor descriptor, String indent, Log logger) {
        if (descriptor.getSubList() != null) {
            logger.debug(descriptor.getStringDescriptor());
            logger.debug(indent + "  [");
            display(descriptor.getSubList(), indent + "  ", logger);
            logger.debug(indent + "  ]");
        } else {
            logger.debug(descriptor.getStringDescriptor());
        }
    }

    /**
     * 
     */
    public static void display(List<BUFRDescriptor> list, String indent, Log logger) {
        for (BUFRDescriptor d : list) {
            displayDescriptor(d, indent, logger);
        }
    }
}
