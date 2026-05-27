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

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Aug 25, 2011 10719      rferrel     Removed the no longer common ugcPtrn.
 * Aug  6, 2012 15219      Qinglu Lin  For tmlPtrn, changed d{1,3}DEG to d{3}DEG.
 * May  1, 2013 15893	mgamazaychikov Changed listOfAreaNamePtrn.
 * Feb 26, 2014 16386      Qinglu Lin  Updated listOfAreaNamePtrn to handle issue caused by
 *                                     hyphens in county name, and that by "'" and "/" as well.
 * May 29, 2015 4441       randerso    Made bullet patterns case insensitive
 * Apr 25, 2017 6251       dgilling    Moved out regex patterns that were not shared.
 *
 * </pre>
 *
 * @version 1.0
 */
public interface IQCCheck {

    String runQC(String header, String body, String nnn);
}
