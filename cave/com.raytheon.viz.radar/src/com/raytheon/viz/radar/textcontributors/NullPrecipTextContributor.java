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
package com.raytheon.viz.radar.textcontributors;

import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;

/**
 * 
 * Contribute a line of text from the symbology block when the null product flag
 * is set for a dual pol precip product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class NullPrecipTextContributor implements IRadarTextContributor {

    @XmlAttribute(required = true)
    private int line;

    @Override
    public String contributeText(RadarRecord record) {
        int line = 0;
        if (record.getProductDependentValues() == null) {
            // don't need even more loading lines
            if (this.line == 0) {
                return "Loading";
            } else {
                return "";
            }
        }
        // Check null product flag
        if (record.getProductDependentValue(2) == 0) {
            return "";
        }
        SymbologyBlock symbologyBlock = record.getSymbologyBlock();
        if (symbologyBlock == null) {
            return "";
        }
        for (Layer layer : symbologyBlock.getLayers()) {
            for (SymbologyPacket packet : layer.getPackets()) {
                if (packet instanceof TextSymbolPacket) {
                    if (line == this.line) {
                        return ((TextSymbolPacket) packet).getTheText().trim();
                    } else {
                        line++;
                    }
                }
            }
        }
        return "";
    }
}