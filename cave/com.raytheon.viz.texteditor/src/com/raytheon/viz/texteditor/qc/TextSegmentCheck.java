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
package com.raytheon.viz.texteditor.qc;

import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.uf.viz.vtec.VtecUtil;

/**
 * QC text segment
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 *                                        Initial creation
 * Aug 25, 2011  10719    rferrel         Changed ugcPtrn to handle multi-line
 *                                        UGCs
 * Sep 01, 2011  10764    rferrel         Allow multiple bullet types for given
 *                                        Vtec.
 * Jul 20, 2012  15003    mgamazaychikov  Allow standalone MWS have no headline
 *                                        Add vtec to checkHeadline signature
 * Jul 20, 2012  15006    mgamazaychikov  Do not perform search for a list of
 *                                        county/zones names in the MWS segment
 *                                        heading.
 * Nov 07, 2012  15003    mgamazaychikov  Do not perform QC check on standalone
 *                                        MWS headline.
 * May 21, 2013  16200    Qinglu Lin      Prevent countyOrZoneCounter from being
 *                                        increased for a line that has no word
 *                                        County/Parish/Municipality in it.
 * May 13, 2014  17177    Qinglu Lin      Updated runQC().
 * Sep 15, 2014  529      mgamazaychikov  Create
 *                                        firstBulletImmediateCauseQCExclusions
 *                                        list and add IC to it.
 * May 29, 2015  4441     randerso        Fixed QC to work with mixed case
 * Nov 24, 2015  17501    dhuffman        Added lookaheads to ugc pattern to
 *                                        remove the telephone number special
 *                                        case.
 * Apr 28, 2016  18947    D. Friedman     Fixed UGC pattern.
 * Nov 03, 2016  5934     randerso        Moved VtecObject and VtecUtil to a
 *                                        separate plugin.
 * Apr 11, 2016  6251     dgilling        Support changes to QualityControl.
 *
 * </pre>
 *
 * @version 1.0
 */
public class TextSegmentCheck extends AbstractTextSegmentCheck {

    @Override
    protected boolean isSegmented(String nnn, VtecObject vtec) {
        return QualityControl.getInstance().getSegmentedNNN().contains(nnn);
    }
}
