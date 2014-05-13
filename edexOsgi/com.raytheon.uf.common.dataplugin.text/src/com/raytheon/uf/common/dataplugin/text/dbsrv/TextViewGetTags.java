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
package com.raytheon.uf.common.dataplugin.text.dbsrv;

/**
 * Enumeration for textdbsrv view retrieval options
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/15/2008   1538       jkorman     Initial creation.
 * 08/05/2009   2191       rjpeter     Added PRODXML and JOINXML.
 * 05/17/2010   2187       cjeanbap    Added OPERATIONAL.
 * May 15, 2014 2536       bclement    moved from uf.edex.textdbsrv
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public enum TextViewGetTags {
    LATEST, ALL, INFO, PROD, JOIN, RAW, UNIX, DEFAULT, CLIENT, PRODXML, JOINXML, OPERATIONAL;
}
