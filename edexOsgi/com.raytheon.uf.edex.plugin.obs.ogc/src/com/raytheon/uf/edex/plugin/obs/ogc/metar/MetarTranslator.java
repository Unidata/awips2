/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.plugin.obs.ogc.metar;

import java.util.ArrayList;

import javax.xml.bind.JAXBElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.edex.plugin.obs.ogc.metar.feature.Metar;
import com.raytheon.uf.edex.plugin.obs.ogc.metar.feature.MetarObjectFactory;
import com.raytheon.uf.edex.wfs.reg.IPdoGmlTranslator;

/**
 * 
 * Metar Translator
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   753       dhladky      Modified from a class written by Brian Clements
 * 04/01/2013   1746      dhladky      Updated for MADIS impl.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MetarTranslator implements IPdoGmlTranslator {

    public static final String GML_VERSION = "3.1.1";

    public MetarTranslator() {

    }

    @Override
    public ArrayList<JAXBElement<?>> translate(PluginDataObject[] pdos) {
        ArrayList<JAXBElement<?>> rval = new ArrayList<JAXBElement<?>>(
                pdos.length);
        for (PluginDataObject pdo : pdos) {
            rval.add(translate(pdo));
        }
        return rval;
    }

    /**
     * @param pdo
     * @return
     */
    public JAXBElement<Metar> translate(PluginDataObject pdo) {
        MetarRecord record = (MetarRecord) pdo;
        Metar to = new Metar(record);
        return new MetarObjectFactory().create(to);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.PdoGmlTranslator#getVersion()
     */
    @Override
    public String getVersion() {
        return GML_VERSION;
    }

}
