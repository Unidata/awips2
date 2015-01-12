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
package com.raytheon.uf.common.nc.bufr.tables;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.serialization.JAXBManager;

/**
 * JAXB manager for translation table objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TranslationTableManager extends JAXBManager {
    
    private static volatile TranslationTableManager instance;

    private TranslationTableManager() throws JAXBException {
        super(TranslationTable.class);
    }

    public static TranslationTableManager getInstance() throws JAXBException {
        if (instance == null) {
            synchronized (TranslationTableManager.class) {
                instance = new TranslationTableManager();
            }
        }
        return instance;
    }

}
