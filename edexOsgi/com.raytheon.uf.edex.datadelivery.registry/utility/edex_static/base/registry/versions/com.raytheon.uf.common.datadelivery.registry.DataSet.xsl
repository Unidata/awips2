<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- 
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
 * 
 * DataSet.xsl
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 ******************************************************************
 * Mar 02, 2014 2789      dhladky      XSLT transformation of other versions to current.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:ns9="com.raytheon.uf.common.datadelivery.registry" version="1.0">
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.WFSPointDataSet.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet.xsl" />

    <!-- Add any new DataSet types/templates to this list of if checks -->
    <xsl:if test="openDapGriddedDataSet">
        <xsl:call-template name="openDapGriddedDataSet" />
    </xsl:if>
    <xsl:if test="wfsPointDataSet">
        <xsl:call-template name="wfsPointDataSet" />
    </xsl:if>

</xsl:stylesheet>
	