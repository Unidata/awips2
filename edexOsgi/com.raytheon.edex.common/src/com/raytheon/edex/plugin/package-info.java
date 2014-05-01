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
/**
 * Plugin pattern base classes, interfaces and factory classes.
 * <P>
 * Each plug-in must implement the basic plug-in interfaces: 
 * {@link com.raytheon.edex.plugin.IMessageDecoder},
 * {@link com.raytheon.edex.plugin.IMessageWriter}, 
 * and {@link com.raytheon.edex.plugin.AbstractRecordSeparator}. 
 * Basic implementation classes are provided for
 * {@link com.raytheon.edex.plugin.RecordSeparatorImpl AbstractRecordSeparator} and
 * {@link com.raytheon.uf.common.datastorage.records.IDataRecord IDataRecord}.
 */
package com.raytheon.edex.plugin;