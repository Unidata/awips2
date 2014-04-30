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
package com.raytheon.uf.edex.plugin.bufrobs;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.nc.bufr.BufrParser;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.bufrobs.category.CategoryKey;

/**
 * Entry point for BUFR obs decoding. Determines the correct decoder instance
 * using BUFR attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2014 2906       bclement     Initial creation
 * Apr 29, 2014 2906       bclement     close parser when finished
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrObsProcessor {

    protected static final IUFStatusHandler log = UFStatus
            .getHandler(BufrObsProcessor.class);

    public static final String BUFR_CAT_ATTRIBUTE = "BUFR:category";

    public static final String BUFR_SUBCAT_ATTRIBUTE = "BUFR:subCategory";

    private static final List<AbstractBufrSfcObsDecoder> decoders = new ArrayList<AbstractBufrSfcObsDecoder>();

    /**
     * @param bufrFile
     * @return
     * @throws BufrObsDecodeException
     */
    public PluginDataObject[] process(File bufrFile)
            throws BufrObsDecodeException {
        BufrParser parser = null;
        PluginDataObject[] rval;
        try {
            parser = new BufrParser(bufrFile);
            CategoryKey key = getBufrCategory(parser);
            AbstractBufrSfcObsDecoder decoder = getDecoder(key);
            if (decoder == null) {
                throw new BufrObsDecodeException(
                        "Unable to find decoder for file: "
                                + bufrFile.getAbsolutePath() + ". Category: "
                                + key.getCategory() + ", Subcategory: "
                                + key.getSubcategory());
            } else {
                rval = decoder.decode(parser, key);
            }
        } catch (IOException e) {
            throw new BufrObsDecodeException("Unable to read BUFR file: "
                    + bufrFile, e);
        } finally {
            if (parser != null)
                try {
                    parser.close();
                } catch (IOException e) {
                    throw new BufrObsDecodeException(
                            "Unable to close parser for file: " + bufrFile, e);
                }
        }
        return rval;
    }

    /**
     * Get category key from BUFR file
     * 
     * @param parser
     * @return
     * @throws BufrObsDecodeException
     */
    private CategoryKey getBufrCategory(BufrParser parser)
            throws BufrObsDecodeException {
        int cat = getIntAttribute(parser, BUFR_CAT_ATTRIBUTE);
        int subcat = getIntAttribute(parser, BUFR_SUBCAT_ATTRIBUTE);
        return new CategoryKey(cat, subcat);
    }

    /**
     * Get integer global attribute from BUFR file with specified name
     * 
     * @param parser
     * @param name
     * @return
     * @throws BufrObsDecodeException
     */
    private int getIntAttribute(BufrParser parser, String name)
            throws BufrObsDecodeException {
        NetcdfFile ncfile = parser.getNcfile();
        Attribute attrib = ncfile.findGlobalAttributeIgnoreCase(name);
        if (attrib == null) {
            throw new BufrObsDecodeException("BUFR file " + parser.getFile()
                    + " missing required attribute: " + name);
        }
        Number num = attrib.getNumericValue();
        if (num == null) {
            throw new BufrObsDecodeException("BUFR file " + parser.getFile()
                    + " unexpected type for attribute: " + name);
        }
        return num.intValue();
    }

    /**
     * Registers decoder with processor
     * 
     * @param decoder
     * @return
     */
    public BufrObsProcessor register(AbstractBufrSfcObsDecoder decoder) {
        synchronized (decoders) {
            decoders.add(decoder);
        }
        return this;
    }

    /**
     * Find the first decoder that supports category key
     * 
     * @param key
     * @return
     */
    public AbstractBufrSfcObsDecoder getDecoder(CategoryKey key) {
        AbstractBufrSfcObsDecoder rval = null;
        synchronized (decoders) {
            for (AbstractBufrSfcObsDecoder decoder : decoders) {
                Set<CategoryKey> categoryKeys;
                try {
                    categoryKeys = decoder.getCategoryKeys();
                } catch (BufrObsDecodeException e) {
                    log.error("Problem getting categories from decoder: "
                            + decoder, e);
                    continue;
                }
                if (categoryKeys.contains(key)) {
                    rval = decoder;
                    break;
                }
            }
        }
        return rval;
    }
}
