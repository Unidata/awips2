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

package com.raytheon.uf.edex.plugin.text;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import com.google.common.collect.ArrayListMultimap;
import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Look up AFOS ID or WMO ID in afos2awips.txt based on specified criteria.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * Date          Ticket#     Engineer     Description
 * ------------  ----------  -----------  --------------------------
 * Aug  9, 2016  5801        tgurney      Initial creation
 * 
 * </pre>
 * 
 * @author tgurney
 */

public class AfosToAwipsLookup {

    private static final int MAX_NNN_XXX_LENGTH = 3;

    private static final ReadWriteLock a2aLock = new ReentrantReadWriteLock();

    private static final ArrayListMultimap<String, AfosToAwips> a2aByAfosId = ArrayListMultimap
            .create();

    private static final ArrayListMultimap<Pair<String, String>, AfosToAwips> a2aByWmoSiteId = ArrayListMultimap
            .create();

    private static final ArrayListMultimap<String, AfosToAwips> a2aBySiteId = ArrayListMultimap
            .create();

    private static final ArrayListMultimap<Pair<String, String>, AfosToAwips> a2aByNnnXxx = ArrayListMultimap
            .create();

    private static final ArrayListMultimap<Triple<String, String, String>, AfosToAwips> a2aByNnnXxxSiteId = ArrayListMultimap
            .create();

    static {
        refresh();
        PathManagerFactory.getPathManager().addLocalizationPathObserver(
                AfosToAwipsUtil.AFOS2AWIPS_FILE,
                new ILocalizationPathObserver() {

                    @Override
                    public void fileChanged(ILocalizationFile file) {
                        refresh();
                    }
                });
    }

    private AfosToAwipsLookup() {
        // static interface only
    }

    private static void refresh() {
        try {
            a2aLock.writeLock().lock();
            a2aByAfosId.clear();
            a2aByWmoSiteId.clear();
            a2aBySiteId.clear();
            a2aByNnnXxx.clear();
            a2aByNnnXxxSiteId.clear();
            TreeSet<AfosToAwips> a2aRecords = new TreeSet<>();
            a2aRecords.addAll(AfosToAwipsUtil.readAllLocalizationFiles());
            for (AfosToAwips record : a2aRecords) {
                String cccc = record.getWmocccc();
                String nnn = getNnn(record);
                String xxx = getXxx(record);
                a2aByAfosId.put(record.getAfosid(), record);
                a2aByWmoSiteId
                        .put(Pair.of(record.getWmottaaii(), cccc), record);
                a2aBySiteId.put(cccc, record);
                a2aByNnnXxx.put(Pair.of(nnn, xxx), record);
                a2aByNnnXxxSiteId.put(Triple.of(nnn, xxx, cccc), record);
            }
        } finally {
            a2aLock.writeLock().unlock();
        }
    }

    private static String getNnn(AfosToAwips a) {
        return a.getAfosid().substring(3, 6);
    }

    private static String getXxx(AfosToAwips a) {
        return StringUtils.rightPad(a.getAfosid().substring(6),
                MAX_NNN_XXX_LENGTH);
    }

    /**
     * @param ttaaii
     *            the WMO id to look up
     * @param cccc
     *            the 4-char site id
     * @return A container with the AfosToAwips objects found.
     */
    public static AfosWmoIdDataContainer lookupAfosId(String ttaaii, String cccc) {
        try {
            a2aLock.readLock().lock();
            ttaaii = ttaaii.toUpperCase();
            cccc = cccc.toUpperCase();
            List<AfosToAwips> records = null;
            records = a2aByWmoSiteId.get(Pair.of(ttaaii, cccc));
            if (records == null) {
                records = new ArrayList<>();
            }
            AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();
            rval.setIdList(records);
            return rval;
        } finally {
            a2aLock.readLock().unlock();
        }
    }

    /**
     * @param afosid
     * @return A container with the AfosToAwips objects found.
     */
    public static AfosWmoIdDataContainer lookupWmoId(String afosid) {
        try {
            a2aLock.readLock().lock();
            afosid = AfosToAwipsUtil.cleanAfosId(afosid);
            List<AfosToAwips> records = null;
            records = a2aByAfosId.get(afosid);
            if (records == null) {
                records = new ArrayList<>();
            }
            AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();
            rval.setIdList(records);
            return rval;
        } finally {
            a2aLock.readLock().unlock();
        }
    }

    /**
     * Look up afosid by cccc, or by nnn and xxx, or by all three.
     * 
     * @param cccc
     *            may be null if both nnn and xxx are not null. will then look
     *            up by nnn and xxx.
     * @param nnn
     *            may be null. will then look up by cccc only.
     * @param xxx
     *            may be null. will then look up by cccc only.
     * @return A container with the AfosToAwips objects found.
     */
    public static AfosWmoIdDataContainer lookupAfosId(String cccc, String nnn,
            String xxx) {
        try {
            a2aLock.readLock().lock();
            if (cccc != null) {
                cccc = cccc.toUpperCase();
            }
            if (nnn != null) {
                nnn = StringUtils.rightPad(nnn.toUpperCase(),
                        MAX_NNN_XXX_LENGTH);
            }
            if (xxx != null) {
                xxx = StringUtils.rightPad(xxx.toUpperCase(),
                        MAX_NNN_XXX_LENGTH);
            }
            List<AfosToAwips> records = null;
            if (cccc == null) {
                records = a2aByNnnXxx.get(Pair.of(nnn, xxx));
            } else if (nnn == null && xxx == null) {
                records = a2aBySiteId.get(cccc);
            } else {
                records = a2aByNnnXxxSiteId.get(Triple.of(nnn, xxx, cccc));
            }
            if (records == null) {
                records = new ArrayList<>();
            }
            AfosWmoIdDataContainer rval = new AfosWmoIdDataContainer();
            rval.setIdList(records);
            return rval;
        } finally {
            a2aLock.readLock().unlock();
        }
    }
}
