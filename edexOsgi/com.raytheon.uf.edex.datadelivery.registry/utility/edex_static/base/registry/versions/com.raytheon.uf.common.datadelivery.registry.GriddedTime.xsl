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
 * GriddedTime.xsl
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
    version="1.0">
    <xsl:output method="xml" encoding="utf-8" indent="yes" />

    <xsl:template match="time" name="griddedTime">
        <xsl:if test="step">
            <xsl:element name="step">
                <xsl:value-of select="step" />
            </xsl:element>
        </xsl:if>
        <xsl:if test="stepUnit">
            <xsl:element name="stepUnit">
                <xsl:value-of select="stepUnit" />
            </xsl:element>
        </xsl:if>
        <xsl:for-each select="selectedTimeIndices">
            <xsl:element name="selectedTimeIndices">
                <xsl:value-of select="." />
            </xsl:element>
        </xsl:for-each>
        <xsl:for-each select="cycleTimes">
            <xsl:element name="cycleTimes">
                <xsl:value-of select="." />
            </xsl:element>
        </xsl:for-each>
    </xsl:template>

</xsl:stylesheet>